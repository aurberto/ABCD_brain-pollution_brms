## BUILD DATASET FOR THE ANALYSIS - script 01
# Add the connectome metrics from FCH and LEiDA to the dataset.
# Aurora Berto, aurber@utu.fi

## --- Libraries ---------------------------------------------------------------
library(R.matlab)

## --- Allocate dataset --------------------------------------------------------
df <- data.frame()
K = 2:20 

df_path <- "~/path/to/output/data"

## --- Sites and subjects' ID --------------------------------------------------
sites <- c("G010","G031","G032","G075","G087","P023","P043","P064","S011","S012","S013","S014","S020","S021","S022","S042","S053","S065","S076","S086","S090")

sbjID_path <- "~/path/to/subjsIDs"
sbjID_df <- data.frame(site = character(), src_subject_id = character(), stringsAsFactors = F)

for (site in sites){
  file_path <- file.path(sbjID_path, paste0(site, "_subjs_passed_to_LEiDA.txt"))
  
  if (file.exists(file_path)){
    df <- read.table(file_path, header = F, col.names = c("src_subject_id"), stringsAsFactors = F)
    df$site <- site # add the column with site name
    sbjID_df <- rbind(sbjID_df, df) # add to other sites
  } else {
    warning(paste("File not found:", file_path))
  }
}

df <- sbjID_df[, c("site","src_subject_id")]
df$src_subject_id <- gsub("^sub-", "", df$src_subject_id)

# head(df)
# rm(file_path, sbjID_path, sbjID_df, site)

## --- FCH metrics -------------------------------------------------------------
harmonics_root <- "~/path/to/connectome_data/FCH-metrics"
power_file_name <- "Extracted_Harmonics_power.csv"
energy_file_name <- "Extracted_Harmonics_energy.csv"

power_file_path <- file.path(harmonics_root, power_file_name)
energy_file_path <- file.path(harmonics_root, energy_file_name)

power_fch_results <- read.csv(power_file_path, stringsAsFactors = FALSE)
energy_fch_results <- read.csv(energy_file_path, stringsAsFactors = FALSE)

# Concatenate harmonics metrics (by column)
harmonics_results <- cbind(power_fch_results, energy_fch_results)

# head(harmonics_results)
df <- cbind(df, harmonics_results)


## --- LEiDA probabilities of occurrence ---------------------------------------
sbjLEiDA_path <- "~/path/to/connectome_data"
sbjLEiDA_file <- file.path(sbjLEiDA_path, "Kmeans_results_ordered_k20.mat")

### Read matlab file
LEiDA_data <- readMat(sbjLEiDA_file)
P_list <- LEiDA_data$P.order

for (i in 1:19) {
  K <- i + 1  
  P_matrix <- P_list[,i,1:K]
  P_df <- as.data.frame(P_matrix)
  colnames(P_df) <- paste0("P_k", K, "c", 1:K)
  df <- cbind(df, P_df)
}
# head(df)
# rm(sbjLEiDA_path, sbjLEiDA_file, P_df, P_matrix, P_list, i)

## --- LEiDA lifetimes ---------------------------------------------------------
LT_list <- LEiDA_data$LT.order

for (i in 1:19) {
  K <- i + 1  
  LT_matrix <- LT_list[,i,1:K]
  LT_df <- as.data.frame(LT_matrix)
  colnames(LT_df) <- paste0("LT_k", K, "c", 1:K)
  df <- cbind(df, LT_df)
}
# head(df)
# rm( LEiDA_data, LT_df, LT_matrix, LT_list, i)

## --- LEiDA transition probabilities ------------------------------------------
pt.path <- "~/path/to/connectome_data/Transitions/K2toK20"

transition_data <- list()

for (K in 2:20) {
  dir_K <- file.path(pt.path, paste0("K", K))
  
  for (C in 1:K) {
    file_name <- paste0("K", K, "x", C, "_Extracted_LEiDA_transitions.csv")
    file_path <- file.path(dir_K, file_name)
    
    if (file.exists(file_path)) {
      data <- read.csv(file_path)
      for (col_name in colnames(data)) {
        transition_data[[paste0("TR_K", K, "_C", C, col_name)]] <- data[[col_name]]
      }
    }
  }
}

df <- cbind(df, transition_data)
# rm(dir_K, file_path, file_name, pt.path, C, data)


### --- Save dataset  ----------------------------------------------------------
write.csv(df, file.path(df_path, "DATASET_connectomeMetrics.csv"), row.names = F)

