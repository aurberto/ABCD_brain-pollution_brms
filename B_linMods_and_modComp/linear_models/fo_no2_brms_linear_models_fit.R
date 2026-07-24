# ------------------------------------------------------------------------------
# LEiDA FO prediction from air pollution (K=13; NO2)
# ------------------------------------------------------------------------------
# Berto A. 05/2026 aurber@utu.fi

# This code will test 3 models: null model, only-pollutant model, and a full 
# model (pollutant+covariates) with a moderation term between ADI and pollutant.
# Each model (but the null one) will have site as random effect. 
# Final model comparison will be done to obtain the best model for following
# statistical inference.

# This code is adapted to be run in CSC - Puhti, using the script 
# run_Rscripts.sh from the terminal.

# --- Setup libraries for custom R ---------------------------------------------

user_lib <- Sys.getenv("R_LIBS_USER")
.libPaths(c(user_lib, .libPaths()[!grepl("r-env", .libPaths())]))
print(.libPaths())
Sys.setenv(TMPDIR = "/path/to/ABCD-brms_pollution/tmp")

# --- Parallel options ---------------------------------------------------------

options(mc.cores = as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK")), scipen = 100000)

# --- Load packages ------------------------------------------------------------

library(dplyr)
library(stringr)
library(knitr)
library(brms)
library(loo)
library(rlang)

# --- Load data and set paths --------------------------------------------------

data_path <- "/path/to/data/DATASET_analysis_062026.csv"
data <- read.csv(data_path, header = TRUE, stringsAsFactors = TRUE)

models_path <- "/path/to/results/fractional_occupancy"
# Create directory if not exist
if (!dir.exists(models_path)) {
  dir.create(models_path, recursive = TRUE)
}

summary_path <- "/path/to/results/fractional_occupancy/model_comparison"
# Create directory if not exist
if (!dir.exists(summary_path)) {
  dir.create(summary_path, recursive = TRUE)
}

pollutant <- "no2"
metric <- "fo"

# --- Check LEiDA FO distribution ----------------------------------------------
# Check to see if probabilities are outside the range [0,1]
# and thus need correction before applying the beta-regression model

epsilon <- 1e-4
data_rescaled <- data %>%
  mutate(across(starts_with("P_k13c"), 
                ~ pmin(pmax(., epsilon), 1 - epsilon))) %>%
  rowwise() %>%
  mutate(total = sum(c_across(starts_with("P_k13c"))),
         across(starts_with("P_k13c"), ~ . / total)) %>%
  ungroup()


# --- Null model ---------------------------------------------------------------

fit0 <- brm(
  formula = cbind(P_k13c1, P_k13c2, P_k13c3, P_k13c4, P_k13c5, P_k13c6, P_k13c7, 
                  P_k13c8, P_k13c9, P_k13c10, P_k13c11, P_k13c12, P_k13c13) 
  ~ 1,
  data = data_rescaled,
  family = dirichlet,
  cores = 8, seed = 1
)
# fit0
saveRDS(fit0, file = file.path(models_path, paste0(metric,"_",pollutant,"_fit0_null.RDS")))


# --- Model with reshist_addr1_no2_prenatal_avg -------------------------------

fit1 <- brm(
  formula = cbind(P_k13c1, P_k13c2, P_k13c3, P_k13c4, P_k13c5, P_k13c6, P_k13c7, 
                  P_k13c8, P_k13c9, P_k13c10, P_k13c11, P_k13c12, P_k13c13) 
  ~ reshist_addr1_no2_prenatal_avg,
  data = data_rescaled,
  family = dirichlet,
  cores = 8, seed = 1
)
# fit1
saveRDS(fit1, file = file.path(models_path, paste0(metric,"_",pollutant,"_fit1_only_prenatal.RDS")))


# --- Model with reshist_addr1_no2_2016_aavg --------------------------------------

fit2 <- brm(
  formula = cbind(P_k13c1, P_k13c2, P_k13c3, P_k13c4, P_k13c5, P_k13c6, P_k13c7, 
                  P_k13c8, P_k13c9, P_k13c10, P_k13c11, P_k13c12, P_k13c13) 
  ~ reshist_addr1_no2_2016_aavg,
  data = data_rescaled,
  family = dirichlet,
  cores = 8, seed = 1
)
# fit2
saveRDS(fit2, file = file.path(models_path, paste0(metric,"_",pollutant,"_fit2_only_current.RDS")))

# --- Model with reshist_addr1_no2_prenatal_avg + cov. + mod. -----------------

fit3 <- brm(
  formula = cbind(P_k13c1, P_k13c2, P_k13c3, P_k13c4, P_k13c5, P_k13c6, P_k13c7, 
                  P_k13c8, P_k13c9, P_k13c10, P_k13c11, P_k13c12, P_k13c13) 
  ~ reshist_addr1_no2_prenatal_avg*reshist_addr1_adi_quint + demo_sex_v2 + 
    pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg,
  data = data_rescaled,
  family = dirichlet,
  cores = 8, seed = 1
)
# fit3
saveRDS(fit3, file = file.path(models_path, paste0(metric,"_",pollutant,"_fit3_mod_prenatal.RDS")))


# --- Model reshist_addr1_no2_2016_aavg + cov. + mod. -----------------------------

fit4 <- brm(
  formula = cbind(P_k13c1, P_k13c2, P_k13c3, P_k13c4, P_k13c5, P_k13c6, P_k13c7, 
                  P_k13c8, P_k13c9, P_k13c10, P_k13c11, P_k13c12, P_k13c13) 
  ~ reshist_addr1_no2_2016_aavg*reshist_addr1_adi_quint + demo_sex_v2 + 
    pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg,
  data = data_rescaled,
  family = dirichlet,
  cores = 8, seed = 1
)
# fit4
saveRDS(fit4, file = file.path(models_path, paste0(metric,"_",pollutant,"_fit4_mod_current.RDS")))
