
## Beta standardization and correction for multiple comparisons
# Berto Aurora (aurber@utu.fi), 09/2026

# This code has to be run locally, for each metric separately, after downloading 
# from CSC the best-performing models returned from LOO-CV stacking weights 
# model comparison.

library(multibayes)
library(posterior)
library(dplyr)
library(writexl)

METRIC <- "energy" # USER.adapt!

# Data
data <- read.csv( "path/to/data/DATASET_analysis_052026.csv" )

# Models
models_dir <- paste0( "path/to/results/model_comparison/", METRIC, "/models" )
model_files <- list.files( models_dir, pattern = "\\.RDS$", full.names = TRUE )

# Store original and standardized posterior draws
draws_pd_all <- list()
draws_std_all <- list()


# --- 1. Extract and standardize interaction effects ---
for (file in model_files) {
  
  mdl <- readRDS(file)
  model_name <- tools::file_path_sans_ext(basename(file))
  draws <- as_draws_df(mdl)
  
  # Fractional occupancy: beta_std = beta * SD(X)
  if (METRIC == "fractional_occupancy") {
    effect_names <- names(draws)[ grepl( "^b_muPk13c[0-9]+_.*_(pm25|no2|o3).*($|:.*adi)", names(draws), ignore.case = TRUE ) ]
    for (b in effect_names) {
      predictor <- sub( "^b_muPk13c[0-9]+_(.*)", "\\1", b )
      predictor <- sub( ":.*$", "", predictor )
      sd_predictor <- sd( data[[predictor]], na.rm = TRUE )
      draws[[paste0(b, "_std")]] <- draws[[b]] * sd_predictor
    }
    
    # FCH power
  } else if (METRIC == "power") {
    effect_names <- names(draws)[ grepl( "^b_Harmonicspower[1-6]_.*_(pm25|no2|o3).*($|:.*adi)", names(draws), ignore.case = TRUE )]
    for (b in effect_names) {
      outcome <- sub( "^b_(Harmonicspower[1-6])_.*", "\\1", b )
      sigma_name <- paste0("sigma_", outcome)
      draws[[paste0(b, "_std")]] <- draws[[b]] / draws[[sigma_name]]
    }
    
    # FCH energy
  } else if (METRIC == "energy") {
    effect_names <- names(draws)[ grepl( "^b_Harmonicsenergy[1-6]_.*_(pm25|no2|o3).*($|:.*adi)", names(draws), ignore.case = TRUE )]
    for (b in effect_names) {
      outcome <- sub( "^b_(Harmonicsenergy[1-6])_.*", "\\1", b )
      sigma_name <- paste0("sigma_", outcome)
      draws[[paste0(b, "_std")]] <- draws[[b]] / draws[[sigma_name]]
    }
    
    # Dwell time
  } else if (METRIC == "dwell_times") {
    effect_names <- names(draws)[grepl("^b_LTk13c[0-9]+_.*_(pm25|no2|o3).*($|:.*adi)",names(draws),ignore.case = TRUE)] 
    for (b in effect_names) { 
      outcome <- sub("^b_(LTk13c[0-9]+)_.*", "\\1", b)
      sigma_name <- paste0("sigma_", outcome)
      draws[[paste0(b, "_std")]] <- draws[[b]] / draws[[sigma_name]]
    }
    
  } else {
    stop("Unknown METRIC.")
  }
  
  # Check that effects were found
  if (length(effect_names) == 0) {
    warning( paste("No interaction effects found in model:", model_name) )
    next
  }
  
  std_names <- paste0(effect_names, "_std")
  
  # Save
  original_draws <- draws[, effect_names, drop = FALSE]
  standardized_draws <- draws[, std_names, drop = FALSE]
  colnames(original_draws) <- paste0(model_name, "__", effect_names)
  colnames(standardized_draws) <- paste0(model_name, "__", effect_names)
  draws_pd_all[[model_name]] <- original_draws
  draws_std_all[[model_name]] <- standardized_draws
}

# --- 2. Combine ALL models ---
draws_pd_df <- do.call( cbind, draws_pd_all )
draws_std_df <- do.call( cbind, draws_std_all )
cat( "Number of tests:", ncol(draws_pd_df), "\n" )


# --- 3. Bayesian correction for multiple comparisons ---
pd <- pd.adjust( draws = draws_pd_df, pi0 = 0.7, null.value = 0 )
pd_sorted <- pd %>%
  tibble::rownames_to_column(var = "parameter")
# pd_sorted  #p > 0.9891


# --- 4. Standardized beta summaries ---
results <- data.frame(
  parameter = colnames(draws_std_df),
  estimate = sapply( draws_std_df, median ),
  lower = sapply( draws_std_df, quantile, probs = .025 ),
  upper = sapply( draws_std_df, quantile, probs = .975 )
)

# Add corrected probability of direction
pd_results <- pd_sorted %>%
  select( parameter, pd, pd.adj )

results <- results %>%
  left_join( pd_results %>%
      select( parameter, pd, pd.adj ), by = "parameter" ) %>%
  select( parameter, estimate, lower, upper, pd, pd.adj ) %>%
  arrange( desc(pd.adj) )


# --- 5. Export ---
write_xlsx( results, paste0( "path/to/results/model_comparison/", METRIC, "/std_beta_pd_correction.xlsx" ))
