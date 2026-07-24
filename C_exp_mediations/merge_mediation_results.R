# ------------------------------------------------------------------------------
# MERGE ALL MEDIATION RESULTS FOR THE SAME METRIC AND POLLUTANT
# ------------------------------------------------------------------------------
# Berto A. 05/2026 aurber@utu.fi
# Script to merge single model's mediation results in the same .csv file

library(dplyr)

pollutant <- "pm25" # specify: pm25, o3, no2
tables_path <- paste0("/path/to/metric/mediations/tables/",pollutant)
output_path <- "/path/to/metric/mediations/tables"


files <- list.files(tables_path, pattern = "mediation_results_task_.*\\.csv$", full.names = TRUE)
results_df <- bind_rows(lapply(files, read.csv))

write.csv(results_df, 
    file.path(output_path, paste0(pollutant,"_significant_mediation_results.csv")), row.names = FALSE)

cat("Merged results saved.\n")