# ------------------------------------------------------------------------------
# LEiDA DT prediction from air pollution (K=13; O3)
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

data_path <- "/path/to/data/DATASET_analysis_052026.csv"
data <- read.csv(data_path, header = TRUE, stringsAsFactors = TRUE)

models_path <- "/path/to/results/dwell_times"
# Create directory if not exist
if (!dir.exists(models_path)) {
  dir.create(models_path, recursive = TRUE)
}

summary_path <- "/path/to/results/dwell_times/model_comparison"
# Create directory if not exist
if (!dir.exists(summary_path)) {
  dir.create(summary_path, recursive = TRUE)
}

pollutant <- "o3"
metric <- "dt"


# --- Null model ---------------------------------------------------------------

fit0 <- brm(
  formula = bf(LT_k13c1 ~ 1) +
    bf(LT_k13c2 ~ 1) +
    bf(LT_k13c3 ~ 1) +
    bf(LT_k13c4 ~ 1) +
    bf(LT_k13c5 ~ 1) +
    bf(LT_k13c6 ~ 1) +
    bf(LT_k13c7 ~ 1) +
    bf(LT_k13c8 ~ 1) +
    bf(LT_k13c9 ~ 1) +
    bf(LT_k13c10 ~ 1) +
    bf(LT_k13c11 ~ 1) +
    bf(LT_k13c12 ~ 1) +
    bf(LT_k13c13 ~ 1),
  data = data,
  family = gaussian(),
  cores = 8,
  seed = 1
)
# fit0
saveRDS(fit0, file = file.path(models_path, paste0(metric,"_",pollutant,"_fit0_null.RDS")))


# --- Model with reshist_addr1_o3_prenatal_avg -------------------------------

fit1 <- brm(
  formula = bf(LT_k13c1 ~ reshist_addr1_o3_prenatal_avg + (1|site)) +
    bf(LT_k13c2 ~ reshist_addr1_o3_prenatal_avg + (1|site)) +
    bf(LT_k13c3 ~ reshist_addr1_o3_prenatal_avg + (1|site)) +
    bf(LT_k13c4 ~ reshist_addr1_o3_prenatal_avg + (1|site)) +
    bf(LT_k13c5 ~ reshist_addr1_o3_prenatal_avg + (1|site)) +
    bf(LT_k13c6 ~ reshist_addr1_o3_prenatal_avg + (1|site)) +
    bf(LT_k13c7 ~ reshist_addr1_o3_prenatal_avg + (1|site)) +
    bf(LT_k13c8 ~ reshist_addr1_o3_prenatal_avg + (1|site)) +
    bf(LT_k13c9 ~ reshist_addr1_o3_prenatal_avg + (1|site)) +
    bf(LT_k13c10 ~ reshist_addr1_o3_prenatal_avg + (1|site)) +
    bf(LT_k13c11 ~ reshist_addr1_o3_prenatal_avg + (1|site)) +
    bf(LT_k13c12 ~ reshist_addr1_o3_prenatal_avg + (1|site)) +
    bf(LT_k13c13 ~ reshist_addr1_o3_prenatal_avg + (1|site)),
  data = data,
  family = gaussian(),
  cores = 8,
  seed = 1
)
# fit1
saveRDS(fit1, file = file.path(models_path, paste0(metric,"_",pollutant,"_fit1_only_prenatal.RDS")))


# --- Model with reshist_addr1_o3_2016_annavg --------------------------------------

fit2 <- brm(
  formula = bf(LT_k13c1 ~ reshist_addr1_o3_2016_annavg + (1|site)) +
    bf(LT_k13c2 ~ reshist_addr1_o3_2016_annavg + (1|site)) +
    bf(LT_k13c3 ~ reshist_addr1_o3_2016_annavg + (1|site)) +
    bf(LT_k13c4 ~ reshist_addr1_o3_2016_annavg + (1|site)) +
    bf(LT_k13c5 ~ reshist_addr1_o3_2016_annavg + (1|site)) +
    bf(LT_k13c6 ~ reshist_addr1_o3_2016_annavg + (1|site)) +
    bf(LT_k13c7 ~ reshist_addr1_o3_2016_annavg + (1|site)) +
    bf(LT_k13c8 ~ reshist_addr1_o3_2016_annavg + (1|site)) +
    bf(LT_k13c9 ~ reshist_addr1_o3_2016_annavg + (1|site)) +
    bf(LT_k13c10 ~ reshist_addr1_o3_2016_annavg + (1|site)) +
    bf(LT_k13c11 ~ reshist_addr1_o3_2016_annavg + (1|site)) +
    bf(LT_k13c12 ~ reshist_addr1_o3_2016_annavg + (1|site)) +
    bf(LT_k13c13 ~ reshist_addr1_o3_2016_annavg + (1|site)),
  data = data,
  family = gaussian(),
  cores = 8,
  seed = 1
)
# fit2
saveRDS(fit2, file = file.path(models_path, paste0(metric,"_",pollutant,"_fit2_only_current.RDS")))

# --- Model with reshist_addr1_o3_prenatal_avg + cov. + mod. -----------------

fit3 <- brm(
  formula = bf(LT_k13c1 ~ reshist_addr1_o3_prenatal_avg*reshist_addr1_adi_quint + demo_sex_v2 + 
                 pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)) +
    bf(LT_k13c2 ~ reshist_addr1_o3_prenatal_avg*reshist_addr1_adi_quint + demo_sex_v2 + 
         pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)) +
    bf(LT_k13c3 ~ reshist_addr1_o3_prenatal_avg*reshist_addr1_adi_quint + demo_sex_v2 + 
         pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)) +
    bf(LT_k13c4 ~ reshist_addr1_o3_prenatal_avg*reshist_addr1_adi_quint + demo_sex_v2 + 
         pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)) +
    bf(LT_k13c5 ~ reshist_addr1_o3_prenatal_avg*reshist_addr1_adi_quint + demo_sex_v2 + 
         pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)) +
    bf(LT_k13c6 ~ reshist_addr1_o3_prenatal_avg*reshist_addr1_adi_quint + demo_sex_v2 + 
         pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)) +
    bf(LT_k13c7 ~ reshist_addr1_o3_prenatal_avg*reshist_addr1_adi_quint + demo_sex_v2 + 
         pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)) +
    bf(LT_k13c8 ~ reshist_addr1_o3_prenatal_avg*reshist_addr1_adi_quint + demo_sex_v2 + 
         pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)) +
    bf(LT_k13c9 ~ reshist_addr1_o3_prenatal_avg*reshist_addr1_adi_quint + demo_sex_v2 + 
         pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)) +
    bf(LT_k13c10 ~ reshist_addr1_o3_prenatal_avg*reshist_addr1_adi_quint + demo_sex_v2 + 
         pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)) +
    bf(LT_k13c11 ~ reshist_addr1_o3_prenatal_avg*reshist_addr1_adi_quint + demo_sex_v2 + 
         pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)) +
    bf(LT_k13c12 ~ reshist_addr1_o3_prenatal_avg*reshist_addr1_adi_quint + demo_sex_v2 + 
         pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)) +
    bf(LT_k13c13 ~ reshist_addr1_o3_prenatal_avg*reshist_addr1_adi_quint + demo_sex_v2 + 
         pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)),
  data = data,
  family = gaussian(),
  cores = 8,
  seed = 1
)
# fit3
saveRDS(fit3, file = file.path(models_path, paste0(metric,"_",pollutant,"_fit3_mod_prenatal.RDS")))


# --- Model reshist_addr1_o3_2016_annavg + cov. + mod. -----------------------------

fit4 <- brm(
  formula = bf(LT_k13c1 ~ reshist_addr1_o3_2016_annavg*reshist_addr1_adi_quint + demo_sex_v2 + 
                 pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)) +
    bf(LT_k13c2 ~ reshist_addr1_o3_2016_annavg*reshist_addr1_adi_quint + demo_sex_v2 + 
         pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)) +
    bf(LT_k13c3 ~ reshist_addr1_o3_2016_annavg*reshist_addr1_adi_quint + demo_sex_v2 + 
         pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)) +
    bf(LT_k13c4 ~ reshist_addr1_o3_2016_annavg*reshist_addr1_adi_quint + demo_sex_v2 + 
         pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)) +
    bf(LT_k13c5 ~ reshist_addr1_o3_2016_annavg*reshist_addr1_adi_quint + demo_sex_v2 + 
         pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)) +
    bf(LT_k13c6 ~ reshist_addr1_o3_2016_annavg*reshist_addr1_adi_quint + demo_sex_v2 + 
         pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)) +
    bf(LT_k13c7 ~ reshist_addr1_o3_2016_annavg*reshist_addr1_adi_quint + demo_sex_v2 + 
         pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)) +
    bf(LT_k13c8 ~ reshist_addr1_o3_2016_annavg*reshist_addr1_adi_quint + demo_sex_v2 + 
         pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)) +
    bf(LT_k13c9 ~ reshist_addr1_o3_2016_annavg*reshist_addr1_adi_quint + demo_sex_v2 + 
         pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)) +
    bf(LT_k13c10 ~ reshist_addr1_o3_2016_annavg*reshist_addr1_adi_quint + demo_sex_v2 + 
         pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)) +
    bf(LT_k13c11 ~ reshist_addr1_o3_2016_annavg*reshist_addr1_adi_quint + demo_sex_v2 + 
         pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)) +
    bf(LT_k13c12 ~ reshist_addr1_o3_2016_annavg*reshist_addr1_adi_quint + demo_sex_v2 + 
         pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)) +
    bf(LT_k13c13 ~ reshist_addr1_o3_2016_annavg*reshist_addr1_adi_quint + demo_sex_v2 + 
         pubertal_dev_score + interview_age + race_ethnicity + birth_weight_kg + (1|site)),
  data = data,
  family = gaussian(),
  cores = 8,
  seed = 1
)
# fit4
saveRDS(fit4, file = file.path(models_path, paste0(metric,"_",pollutant,"_fit4_mod_current.RDS")))
