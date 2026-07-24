## BUILD DATASET FOR THE ANALYSIS - script 02
# Add demographics, developmental, environmental, and mental health metrics to the dataset.
# Aurora Berto, aurber@utu.fi

## --- Libraries ---------------------------------------------------------------
library(dplyr)

## --- Load connectome-based dataset -------------------------------------------
img_df_path <- "~/path/to/output/data/DATASET_connectomeMetrics.csv"
img_df <- read.csv(img_df_path)

img_df$src_subject_id <- gsub("NDARINV", "NDAR_INV", img_df$src_subject_id)
K <- 20

df_path <- "~/path/to/output/data"

## --- Demographics ------------------------------------------------------------
file_path <- "~/path/to/abcd-data-release-5.0/core/abcd-general/abcd_p_demo.csv"
raw_file <- read.csv(file_path)

vars_to_keep <- c("demo_sex_v2","race_ethnicity", "demo_prnt_ed_v2","demo_prtnr_ed_v2","src_subject_id")
cols_to_keep <- raw_file[raw_file$eventname == "baseline_year_1_arm_1",vars_to_keep, drop=F]
# head(cols_to_keep)

# Calculate and append highest parental education
cols_to_keep$highest_parental_education <- pmax(
  cols_to_keep$demo_prnt_ed_v2,
  cols_to_keep$demo_prtnr_ed_v2,
  na.rm = TRUE
)
# Remove parent's and partner's education
cols_to_keep <- cols_to_keep %>%
  select(-demo_prnt_ed_v2, -demo_prtnr_ed_v2)

df <- merge(cols_to_keep, img_df, by="src_subject_id", all.y=TRUE)
# head(df)
rm(file_path, raw_file, vars_to_keep, cols_to_keep)


## --- Interview age -----------------------------------------------------------
file_path <- "~/path/to/abcd-data-release-5.0/core/abcd-general/abcd_y_lt.csv"
raw_file <- read.csv(file_path)

vars_to_keep <- c("interview_age","src_subject_id")
cols_to_keep <- raw_file[raw_file$eventname == "baseline_year_1_arm_1",vars_to_keep, drop=F]
# head(cols_to_keep)

df <- merge(cols_to_keep, df, by="src_subject_id", all.y=TRUE)
# head(df)
rm(file_path, raw_file, vars_to_keep, cols_to_keep)


## --- Pubertal developmental scale  -------------------------------------------
# Parents reported scores
file_path_parents <- "~/path/to/abcd-data-release-5.0/core/physical-health/ph_p_pds.csv"
file_parents <- read.csv(file_path_parents)

vars_to_keep <- c("src_subject_id", "pds_p_ss_female_category", "pds_p_ss_male_category")
cols_to_keep_parents <- file_parents[file_parents$eventname == "baseline_year_1_arm_1",vars_to_keep, drop=F]

# Children reported scores
file_path_youngs <- "~/path/to/abcd-data-release-5.0/core/physical-health/ph_y_pds.csv"
file_youngs <- read.csv(file_path_youngs)

vars_to_keep <- c("src_subject_id", "pds_y_ss_female_category", "pds_y_ss_male_category")
cols_to_keep_youngs <- file_youngs[file_youngs$eventname == "baseline_year_1_arm_1",vars_to_keep, drop=F]

df_merged <- merge(cols_to_keep_parents, cols_to_keep_youngs, by = "src_subject_id")
# head(df_merged)

# Calculate means dividing by sex
df_merged$pubertal_dev_score <- rowMeans(
  cbind(
    ifelse(!is.na(df_merged$pds_p_ss_female_category), df_merged$pds_p_ss_female_category, df_merged$pds_p_ss_male_category),
    ifelse(!is.na(df_merged$pds_y_ss_female_category), df_merged$pds_y_ss_female_category, df_merged$pds_y_ss_male_category)
  ),
  na.rm = TRUE
)

cols_to_keep <- data.frame(pubertal_dev_score = df_merged$pubertal_dev_score, 
                           src_subject_id = df_merged$src_subject_id)
df <- merge(cols_to_keep, df, by="src_subject_id", all.y=TRUE)
# head(df)
rm(file_path_parents, file_path_youngs, file_parents, file_youngs, vars_to_keep, cols_to_keep_parents, cols_to_keep_youngs, cols_to_keep)


## --- MRI device information  -------------------------------------------------
file_path <- "~/path/to/abcd-data-release-5.0/core/imaging/mri_y_adm_info.csv"
raw_file <- read.csv(file_path)
vars_to_keep <- c("mri_info_deviceserialnumber","src_subject_id")

cols_to_keep <- raw_file[raw_file$eventname == "baseline_year_1_arm_1",vars_to_keep, drop=F]
# head(cols_to_keep)

cols_to_keep$mri_info_deviceserialnumber <- factor(cols_to_keep$mri_info_deviceserialnumber)

df <- merge(cols_to_keep, df, by="src_subject_id", all.y=TRUE)
# head(df)
rm(file_path, raw_file, vars_to_keep, cols_to_keep)


## --- Genetic relatedness (twins)  --------------------------------------------
file_path <- "~/path/to/abcd-data-release-5.0/core/genetics/gen_y_pihat.csv"
raw_file <- read.csv(file_path)
vars_to_keep <- c("rel_family_id","rel_group_id","src_subject_id")

cols_to_keep <- raw_file[raw_file$eventname == "baseline_year_1_arm_1",vars_to_keep, drop=F]
# head(cols_to_keep)

df <- merge(cols_to_keep, df, by="src_subject_id", all.y=TRUE)
# head(df)
rm(file_path, raw_file, vars_to_keep, cols_to_keep)


## --- Put to NA all non-available data  ---------------------------------------
# 777 = refuse to answer, 999 = don't know set to NA
df[] <- lapply(df, function(x) {
  if (is.numeric(x)) x[x %in% c(777, 999)] <- NA
  x
})


## --- Weight at birth  --------------------------------------------------------
file_path <- "~/path/to/abcd-data-release-5.0/core/physical-health/ph_p_dhx.csv"
raw_file <- read.csv(file_path)
vars_to_keep <- c("birth_weight_lbs","src_subject_id")

cols_to_keep <- raw_file[raw_file$eventname == "baseline_year_1_arm_1",vars_to_keep, drop=F]

# Convert from lb to g
cols_to_keep$birth_weight_g <- cols_to_keep$birth_weight_lbs*453.592
# head(cols_to_keep)

df <- merge(cols_to_keep, df, by="src_subject_id", all.y=TRUE)
# head(df)
rm(file_path, raw_file, vars_to_keep, cols_to_keep)


## --- Prenatal exposure  ------------------------------------------------------
file_path <- "~/path/to/abcd-data-release-5.0/core/linked-external-data/led_l_prenatal.csv"
raw_file <- read.csv(file_path)

vars_to_keep <- c("reshist_addr1_pm25_prenatal_avg","reshist_addr1_no2_prenatal_avg",
                  "reshist_addr1_o3_prenatal_avg", "src_subject_id")
cols_to_keep <- raw_file[raw_file$eventname == "baseline_year_1_arm_1",vars_to_keep, drop=F]

df <- merge(cols_to_keep, df, by = "src_subject_id", all.y = TRUE)
# head(df)
rm(file_path, raw_file, vars_to_keep, cols_to_keep)


## --- Average annual exposure to pollutants  ----------------------------------
file_path <- "~/path/to/abcd-data-release-5.0/core/linked-external-data/led_l_pm25.csv"
raw_file <- read.csv(file_path)

vars_to_keep <- c("reshist_addr1_pm252016aa", "src_subject_id")
cols_to_keep <- raw_file[raw_file$eventname == "baseline_year_1_arm_1",vars_to_keep, drop=F]

df <- merge(cols_to_keep, df, by = "src_subject_id", all.y = TRUE)
# head(df)
rm(file_path, raw_file, vars_to_keep, cols_to_keep)


## --- Average annual exposure to O3  ------------------------------------------
file_path <- "~/path/to/abcd-data-release-5.0/core/linked-external-data/led_l_o3.csv"
raw_file <- read.csv(file_path)

vars_to_keep <- c("reshist_addr1_o3_2016_annavg", "src_subject_id")
cols_to_keep <- raw_file[raw_file$eventname == "baseline_year_1_arm_1",vars_to_keep, drop=F]

df <- merge(cols_to_keep, df, by = "src_subject_id", all.y = TRUE)
# head(df)
rm(file_path, raw_file, vars_to_keep, cols_to_keep)


## --- Average annual exposure to NO2  -----------------------------------------
file_path <- "~/path/to/abcd-data-release-5.0/core/linked-external-data/led_l_no2.csv"
raw_file <- read.csv(file_path)

vars_to_keep <- c("reshist_addr1_no2_2016_aavg", "src_subject_id")
cols_to_keep <- raw_file[raw_file$eventname == "baseline_year_1_arm_1",vars_to_keep, drop=F]

df <- merge(cols_to_keep, df, by = "src_subject_id", all.y = TRUE)
# head(df)
rm(file_path, raw_file, vars_to_keep, cols_to_keep)


## --- Area Deprivation Index (ADI)  -------------------------------------------
file_path <- "~/path/to/abcd-data-release-5.0/core/linked-external-data/led_l_adi.csv"
raw_file <- read.csv(file_path)

vars_to_keep <- c("reshist_addr1_adi_perc", "src_subject_id")
cols_to_keep <- raw_file[raw_file$eventname == "baseline_year_1_arm_1",vars_to_keep, drop=F]

df <- merge(cols_to_keep, df, by = "src_subject_id", all.y = TRUE)
# head(df)
rm(file_path, raw_file, vars_to_keep, cols_to_keep)


## --- NIH Toolbox - fluid, crystallized and total components  -----------------
file_path <- "~/path/to/abcd-data-release-5.0/core/neurocognition/nc_y_nihtb.csv"
raw_file <- read.csv(file_path)

vars_to_keep <- c("nihtbx_fluidcomp_agecorrected", "nihtbx_cryst_agecorrected",
                  "nihtbx_totalcomp_agecorrected", "src_subject_id")
cols_to_keep <- raw_file[raw_file$eventname == "baseline_year_1_arm_1",vars_to_keep, drop=F]

df <- merge(cols_to_keep, df, by = "src_subject_id", all.y = TRUE)
# head(df)
rm(file_path, raw_file, vars_to_keep, cols_to_keep)


## --- CBCL mental health - internalizing and externalizing factors  -----------
file_path <- "~/path/to/abcd-data-release-5.0/core/mental-health/mh_p_cbcl.csv"
raw_file <- read.csv(file_path)

vars_to_keep <- c("cbcl_scr_syn_internal_t", "cbcl_scr_syn_external_t", "src_subject_id")
cols_to_keep <- raw_file[raw_file$eventname == "baseline_year_1_arm_1",vars_to_keep, drop=F]

df <- merge(cols_to_keep, df, by = "src_subject_id", all.y = TRUE)
# head(df)
rm(file_path, raw_file, vars_to_keep, cols_to_keep)


## --- Save the final dataset --------------------------------------------------
df <- df %>%
  select(src_subject_id, site, everything())

### Save the final dataset
write.csv(df, file.path(df_path,"DATASET_allVariables_complete.csv"), row.names = F)


