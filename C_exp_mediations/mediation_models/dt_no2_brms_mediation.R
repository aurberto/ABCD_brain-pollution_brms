
# Mediation analysis between LEiDA lifetimes, mental health, and air pollution (K=13; NO2)
# Berto A., 05/2026 aurber@utu.fi

# This code runs three Bayesian multilevel Gaussian regression models 
# representing the exposure-mediator (X → M; path a; X = pollutants; 
# M = brain metrics), mediator–outcome (M → Y; path b; M = brain metrics; 
# Y = NIH Toolbox and CBCL scores), and direct exposure-outcome (X → Y; 
# average direct effect, ADE) relationships. 
# The indirect effect was estimated as the average causal mediation effect 
# (ACME = a × b), whereas the total effect was estimated as ADE + ACME. 

# The script is built to run on CSC - Puhti, with each model as independent 
# array. It returns as output all the run models, and a csv file with only 
# reliable associations (95% CI excluding zero).


# --- Setup libraries for custom R ---------------------------------------------
user_lib <- Sys.getenv("R_LIBS_USER")
.libPaths(c(user_lib, .libPaths()[!grepl("r-env", .libPaths())]))
print(.libPaths())
Sys.setenv(TMPDIR = "/path/to/ABCD-brms_pollution/tmp")

# --- Parallel options ---------------------------------------------------------
options(mc.cores = as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK")), scipen = 100000)
task_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

# --- Load packages ------------------------------------------------------------
library(dplyr)
library(stringr)
library(knitr)
library(brms)
library(loo)
library(rlang)
library(bayestestR)

# --- Set seed for reproducibility ---------------------------------------------
set.seed(123)

# --- Load data and set paths --------------------------------------------------
pollutant <- "no2"
metric <- "dt"

data_path <- "/path/to/data/DATASET_analysis_052026.csv"
data <- read.csv(data_path, header = TRUE, stringsAsFactors = TRUE)

models_path <- paste0("/path/to/results/fractional_occupancy/mediations/",pollutant)
# Create directory if not exist
if (!dir.exists(models_path)) {
  dir.create(models_path, recursive = TRUE)
}

tables_path <- paste0("/path/to/results/fractional_occupancy/mediations/tables/",pollutant)
# Create directory if not exist
if (!dir.exists(tables_path)) {
  dir.create(tables_path, recursive = TRUE)
}

# --- Mediation model (SLURM array version) ------------------------------------
mediators <- paste0("LT_k13c", 1:13)

outcomes <- c("nihtbx_fluidcomp_agecorrected",
              "nihtbx_cryst_agecorrected",
              "nihtbx_totalcomp_agecorrected",
              "cbcl_scr_syn_internal_t",
              "cbcl_scr_syn_external_t")

exposures <- c("reshist_addr1_no2_prenatal_avg",
               "reshist_addr1_no2_2016_aavg")

covariates <- "demo_sex_v2 + 
               pubertal_dev_score + 
               interview_age + 
               race_ethnicity + 
               reshist_addr1_adi_quint + 
               birth_weight_kg + 
               (1|site)"

# --- Create grid of combinations ----------------------------------------------
comb_grid <- expand.grid(
  outcome = outcomes,
  mediator = mediators,
  exposure = exposures,
  stringsAsFactors = FALSE
)

# --- Get SLURM task ID --------------------------------------------------------
task_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
if (is.na(task_id)) {
  stop("SLURM_ARRAY_TASK_ID not found.")
}

current_out <- comb_grid$outcome[task_id]
current_med <- comb_grid$mediator[task_id]
current_exp <- comb_grid$exposure[task_id]

cat("Running model for:\n")
cat("Outcome:", current_out, "\n")
cat("Mediator:", current_med, "\n")
cat("Exposure:", current_exp, "\n")

# --- Define formulas ----------------------------------------------------------
bf_m <- bf(as.formula(paste(current_med, "~", current_exp, "+", covariates)))
bf_y <- bf(as.formula(paste(current_out, "~", current_med, "+", current_exp, "+", covariates)))

joint_formula <- bf_m + bf_y + set_rescor(FALSE)

# --- Fit model ----------------------------------------------------------------
med_model <- brm(
  formula = joint_formula,
  data = data,
  cores = 4,
  seed = 1,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.95, max_treedepth = 15),
  refresh = 0
)

# --- Save model ---------------------------------------------------------------
saveRDS(med_model,
        file = file.path(models_path,
                         paste0(current_out ,"_", current_med, "_", current_exp, "_mediation.RDS")
        )
)

cat("Model completed and saved.\n")
