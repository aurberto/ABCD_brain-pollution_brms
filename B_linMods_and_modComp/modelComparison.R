# ------------------------------------------------------------------------------
# MODEL COMPARISON
# ------------------------------------------------------------------------------
# Berto A. 05/2026

# This code will run model comparison using LOO-CV and Bayesian stacking weights
# between: null model, pollutant-only model, fully-adjusted model 
# (pollutant+covariates) with ADI x pollutant moderation. 
# The model with the highest stacking weight will be kept for following 
# statistical inference.

# This code is adapted to be run in CSC - Puhti, using the script 
# run_Rscripts.sh from the terminal.

# --- Setup libraries for custom R ---------------------------------------------

user_lib <- Sys.getenv("R_LIBS_USER")
.libPaths(c(user_lib, .libPaths()[!grepl("r-env", .libPaths())]))
print(.libPaths())
Sys.setenv(TMPDIR = "/scratch/project_2006897/bertoaur/ABCD-brms_pollution/tmp")

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

models_path <- "/path/results/connectome/metric"
summary_path <- "/path/results/connectome/metric/model_comparison"

pollutant <- "no2" # specify: pm, o3, no2
metric <- "power" # specify: fo, dt, power, energy

fit0 <- readRDS(file.path(models_path, 
                          paste0(metric, "_", pollutant, "_fit0_null.RDS")))
fit1 <- readRDS(file.path(models_path, 
                          paste0(metric, "_", pollutant, "_fit1_only_prenatal.RDS")))
fit2 <- readRDS(file.path(models_path, 
                          paste0(metric, "_", pollutant, "_fit2_only_current.RDS")))
fit3 <- readRDS(file.path(models_path, 
                          paste0(metric, "_", pollutant, "_fit5_mod_prenatal.RDS")))
fit4 <- readRDS(file.path(models_path, 
                          paste0(metric, "_", pollutant, "_fit6_mod_current.RDS")))

# --- Model comparison - prenatal ----------------------------------------------

looList <- list(fit0 = loo(fit0), fit1 = loo(fit1), fit3 = loo(fit3))
W <- loo_model_weights(looList)

# save weights
saveRDS(W, file = file.path(summary_path, paste0(metric, "_", pollutant, "_loo_weights_prenatal.rds")))
write.csv(as.data.frame(W), 
          file = file.path(summary_path, paste0(metric, "_", pollutant, "_loo_weights_prenatal.csv")),
          row.names = TRUE)

# save complete loo objects
saveRDS(looList, file = file.path(summary_path, paste0(metric, "_", pollutant, "_loo_objects_prenatal.rds")))
loo_comp <- as.data.frame(loo_compare(looList))
write.csv(loo_comp,
          file = file.path(summary_path,paste0(metric, "_", pollutant, "_loo_compare_prenatal.csv")),
          row.names = TRUE)

# --- Model comparison - late-childhood ----------------------------------------

looList <- list(fit0 = loo(fit0), fit2 = loo(fit2), fit4 = loo(fit4))
W <- loo_model_weights(looList)

# save weights
saveRDS(W, file = file.path(summary_path, paste0(metric, "_", pollutant, "_loo_weights_current.rds")))
write.csv(as.data.frame(W), 
          file = file.path(summary_path, paste0(metric, "_", pollutant, "_loo_weights_current.csv")),
          row.names = TRUE)

# save complete loo objects
saveRDS(looList, file = file.path(summary_path, paste0(metric, "_", pollutant, "_loo_objects_current.rds")))
loo_comp <- as.data.frame(loo_compare(looList))
write.csv(loo_comp,
          file = file.path(summary_path,paste0(metric, "_", pollutant, "_loo_compare_current.csv")),
          row.names = TRUE)