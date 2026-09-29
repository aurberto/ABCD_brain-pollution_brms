
## Mediation coefficients: standardization and correction for multiple comparisons
# This code is built to run on CSC after having run all mediation models.
# Berto Aurora (aurber@utu.fi), 09/2026

remotes::install_github("mar-cald/multibayes")
library(multibayes)
library(brms)
library(posterior)
library(dplyr)
library(writexl)

# SETTINGS
METRIC <- "fch"
POLLUTANT <- c("pm25", "no2", "o3")

data <- read.csv( "path/to/ABCD-brms_pollution/data/DATASET_analysis_052026.csv" )
model_files <- unlist(lapply(POLLUTANT, function(pollutant) {
  models_dir <- paste0(
    "/path/to/ABCD-brms_pollution/results/", METRIC, "/mediations/", pollutant )
  list.files( models_dir, pattern = "\\.RDS$", full.names = TRUE )}))

# METRICS SPECIFIC CONFIGURATIONS
# a_pattern: regex to identify path a (X -> M)
# mediator_regex: regex to extract mediator name from a_name
# mediator_prefix: prefix to remove from mediator to construct path b
# b_pattern_template: template regex for path b (M -> Y); %s = suffix mediator
# outcome_regex: regex to extract outcome from b_name
metric_config <- list(
  fractional_occupancy = list(
    a_pattern          = "^b_Pk13c[0-9]+_.*_(pm25|no2|o3).*$",
    mediator_regex      = "^b_(Pk13c[0-9]+)_.*$",
    mediator_prefix      = "P",
    b_pattern_template  = "^b_.*_P_%s$",
    outcome_regex       = "^b_([^_]+)_P_?.*$"
  ),
  dwell_times = list(
    a_pattern          = "^b_LTk13c[0-9]+_.*_(pm25|no2|o3).*$",
    mediator_regex      = "^b_(LTk13c[0-9]+)_.*$",
    mediator_prefix      = "LT",
    b_pattern_template  = "^b_.*_LT_%s$",
    outcome_regex       = "^b_([^_]+)_LT_?.*$"
  ),
  fch = list(
    a_pattern          = "^b_Harmonics(power|energy)[0-9]+_.*_(pm25|no2|o3).*$",
    mediator_regex      = "^b_(Harmonics(power|energy)[0-9]+)_.*$",
    mediator_prefix      = "Harmonics",
    b_pattern_template  = "^b_.*_Harmonics_%s$",
    outcome_regex       = "^b_([^_]+)_Harmonics_?.*$"
  )
)

cfg <- metric_config[[METRIC]]
if (is.null(cfg)) {
  stop( paste0( "METRIC non riconosciuta: '", METRIC, "'. Valori validi: ",
                paste(names(metric_config), collapse = ", ") ) )
}

# STORAGE
indirect_draws_all <- list()
results_all <- list()

# Custom function: finds exactly one matching column
match_one_column <- function(pattern_key, colnames_clean, colnames_orig, label, model_name) {
  hits <- colnames_orig[ colnames_clean == pattern_key ]
  if (length(hits) == 0) {
    stop( paste0( "\nModel: ", model_name,
                  "\nNo columns found for: ", label, " (pattern: ", pattern_key, ")" ) )
  }
  if (length(hits) > 1) {
    stop( paste0( "\nModel: ", model_name,
                  "\nMore than one column corresponds to ", label, " (pattern: ", pattern_key, ").",
                  "\nCandidates:\n", paste(hits, collapse = "\n"),
                  "\nMake the pattern more specific." ) )
  }
  hits
}
colnames_clean <- gsub("_", "", names(data))

# 1. PROCESS EACH MEDIATION MODEL
for (file in model_files) {
  mdl <- readRDS(file)
  model_name <- tools::file_path_sans_ext( basename(file) )
  draws <- as_draws_df(mdl)
  parameter_names <- names(draws)
  
  # 1A. IDENTIFY a PATH 
  a_effect <- parameter_names[ grepl( cfg$a_pattern, parameter_names, ignore.case = TRUE ) ]
  if (length(a_effect) != 1) {
    stop( paste0( "\nModel: ", model_name,
                  "\nExpected exactly one a-path, found ", length(a_effect),
                  ".\nCandidates:\n", paste(a_effect, collapse = "\n")))
  }
  a_name <- a_effect
  
  # 1B. MEDIATOR and EXPOSURE 
  mediator <- sub( cfg$mediator_regex, "\\1", a_name )
  exposure <- sub( paste0("^b_", mediator, "_"), "", a_name )
  
  # 1C. b PATH 
  mediator_suffix <- sub( paste0("^", cfg$mediator_prefix), "", mediator )
  b_pattern <- sprintf( cfg$b_pattern_template, mediator_suffix )
  b_effect <- parameter_names[ grepl( b_pattern, parameter_names, ignore.case = TRUE ) ]
  if (length(b_effect) != 1) {
    stop( paste0( "\nModel: ", model_name,
                  "\nCould not uniquely identify b-path for mediator ", mediator,
                  ".\nCandidates:\n", paste(b_effect, collapse = "\n")))
  }
  b_name <- b_effect
  
  # 1D. OUTCOME 
  outcome <- sub( cfg$outcome_regex, "\\1", b_name )
  
  # 1E. DIRECT EFFECT c'
  c_name <- paste0( "b_", outcome, "_", exposure )
  if (!(c_name %in% parameter_names)) {
    stop( paste0( "\nModel: ", model_name,
                  "\nCould not find direct effect c': ", c_name ))
  }
  
  # 1F. EXTRACT POSTERIOR DRAWS
  a_draws <- draws[[a_name]]
  b_draws <- draws[[b_name]]
  c_draws <- draws[[c_name]]
  
  # 1G. CALCULATE INDIRECT AND TOTAL EFFECTS
  indirect_draws <- a_draws * b_draws
  total_draws <- c_draws + indirect_draws
  
  # 1H. SDs FOR STANDARDIZATION
  exposure_var <- match_one_column( gsub("_", "", exposure), colnames_clean, names(data), "exposure", model_name )
  sd_X <- sd( data[[exposure_var]], na.rm = TRUE )
  
  mediator_var <- match_one_column( gsub("_", "", mediator), colnames_clean, names(data), "mediator", model_name )
  sd_M <- sd( data[[mediator_var]], na.rm = TRUE )
  
  outcome_var <- match_one_column( gsub("_", "", outcome), colnames_clean, names(data), "outcome", model_name )
  sd_Y <- sd( data[[outcome_var]], na.rm = TRUE )
  
  if (is.na(sd_X) || sd_X == 0) {
    stop( paste0( "\nInvalid SD for exposure: ", exposure_var, "\nModel: ", model_name ))
  }
  if (is.na(sd_M) || sd_M == 0) {
    stop( paste0( "\nInvalid SD for mediator: ", mediator_var, "\nModel: ", model_name ))
  }
  if (is.na(sd_Y) || sd_Y == 0) {
    stop( paste0( "\nInvalid SD for outcome: ", outcome, "\nModel: ", model_name ))
  }
  
  # 1I. STANDARDIZE ALL EFFECTS
  a_std <- a_draws * sd_X / sd_M
  b_std <- b_draws * sd_M / sd_Y
  c_std <- c_draws * sd_X / sd_Y
  indirect_std <- indirect_draws * sd_X / sd_Y
  total_std <- total_draws * sd_X / sd_Y
  
  # 1J. STORE INDIRECT EFFECTS FOR MULTIPLE COMPARISON
  indirect_draws_all[[model_name]] <- indirect_draws
  
  # 1K. SUMMARIZE STANDARDIZED EFFECTS
  summary_effect <- function(x) {
    c( estimate = mean(x), lower = quantile(x, 0.025), upper = quantile(x, 0.975))
  }
  a_summary <- summary_effect(a_std)
  b_summary <- summary_effect(b_std)
  c_summary <- summary_effect(c_std)
  indirect_summary <- summary_effect(indirect_std)
  total_summary <- summary_effect(total_std)
  
  # 1L. STORE RESULTS
  results_all[[model_name]] <- data.frame(
    model = model_name,
    mediator = mediator_var,
    exposure = exposure_var,
    outcome = outcome_var,
    a_path = a_name,
    b_path = b_name,
    c_path = c_name,
    a_estimate = a_summary[[1]],
    a_lower = a_summary[[2]],
    a_upper = a_summary[[3]],
    b_estimate = b_summary[[1]],
    b_lower = b_summary[[2]],
    b_upper = b_summary[[3]],
    c_estimate = c_summary[[1]],
    c_lower = c_summary[[2]],
    c_upper = c_summary[[3]],
    indirect_estimate = indirect_summary[[1]],
    indirect_lower = indirect_summary[[2]],
    indirect_upper = indirect_summary[[3]],
    total_estimate = total_summary[[1]],
    total_lower = total_summary[[2]],
    total_upper = total_summary[[3]],
    sd_exposure = sd_X,
    sd_mediator = sd_M,
    sd_outcome = sd_Y,
    stringsAsFactors = FALSE )
  
  cat( "Processed:", model_name, "\n  X:", exposure_var, "\n  M:", mediator_var, "\n  Y:", outcome_var, "\n\n")
}


# 2. COMBINE ALL INDIRECT EFFECTS
indirect_draws_df <- do.call(
  cbind, lapply( names(indirect_draws_all),
                 function(x) {
                   tmp <- indirect_draws_all[[x]]
                   names(tmp) <- paste0(x, "__indirect")
                   tmp } ) )
cat( "Number of mediation tests:", ncol(indirect_draws_df), "\n" )


# 3. BAYESIAN MULTIPLE-COMPARISON CORRECTION
pd <- pd.adjust( draws = indirect_draws_df, pi0 = 0.7, null.value = 0 )
pd_results <- pd %>%
  tibble::rownames_to_column( var = "parameter" ) %>%
  select( parameter, pd, pd.adj )


# 4. COMBINE STANDARDIZED RESULTS
results <- bind_rows( results_all )


# 5. ADD PD AND PD.ADJ TO INDIRECT EFFECT
results <- results %>%
  bind_cols( pd_results %>%
               select(pd, pd.adj) ) %>%
  arrange(desc(pd.adj))


# 6. EXPORT
output_file <- paste0( "path/to/ABCD-brms_pollution/results/", METRIC, "/mediations/mediation_standardized_effects_pd_correction.xlsx" )
write_xlsx( results, output_file )
cat( "\nResults saved to:\n", output_file, "\n" )