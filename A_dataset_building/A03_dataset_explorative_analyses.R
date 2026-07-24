## EXPLORATORY ANALYSES ON THE FINAL DATASET
# Aurora Berto, aurber@utu.fi

## === Create dataset for statistical analysis =================================
## --- Libraries ---------------------------------------------------------------

library(dplyr)
library(stringr)
library(knitr)
library(brms)
library(loo)
library(rlang)
library(GGally)
library(tidyr)
library(ggplot2)
library(tibble)
library(patchwork)

## --- Load data  --------------------------------------------------------------
df_path <- "~/path/to/data"
df <- read.csv(file.path(df_path, "DATASET_allVariables_complete.csv"), 
               header = TRUE, stringsAsFactors = TRUE)

fig_path <- "~/path/to/results/exploratory_analysis"

## --- Dataset creation --------------------------------------------------------
## Convert birth weight to kg
df$birth_weight_kg <- df$birth_weight_g / 1000

## Remove children with birth weight < 2.500kg
df <- df[df$birth_weight_kg >= 2.5, ] # 6624 -> 5295

## Drop duplicates (twins)
data <- df %>%
  distinct(rel_family_id, rel_group_id, .keep_all = TRUE)
data <- data %>%
  dplyr::select(-rel_family_id, -rel_group_id)

# FCH power and energy with H=6
df_fch <- data %>%
  dplyr::select(src_subject_id, Harmonics_power1, Harmonics_power2, Harmonics_power3,
                Harmonics_power4,  Harmonics_power5,  Harmonics_power6,
                Harmonics_energy1, Harmonics_energy2, Harmonics_energy3,
                Harmonics_energy4, Harmonics_energy5, Harmonics_energy6,)

# LEiDA probabilities and lifetimes with K=13
df_fo <- data %>%
  dplyr::select(src_subject_id, matches("P_k13c"))

df_lt <- data %>%
  dplyr::select(src_subject_id, matches("LT_k13c"))

df_tr <- data %>%
  dplyr::select(src_subject_id, matches("TR_K13_C"))

# Demographic variables
df_demo <- data %>%
  dplyr::select(demo_sex_v2, highest_parental_education, pubertal_dev_score, interview_age,
                race_ethnicity, birth_weight_kg, reshist_addr1_adi_perc, site, src_subject_id)

df_demo$demo_sex_v2 <- factor(df_demo$demo_sex_v2, levels = c(1,2), labels = c("M","F"))
df_demo$race_ethnicity <- factor(df_demo$race_ethnicity, levels = c(1:5), labels = c("W","B","H","A","O"))

x <- df_demo$reshist_addr1_adi_perc
q <- quantile(x, probs = seq(0, 1, 0.2), na.rm = TRUE)
df_demo$reshist_addr1_adi_quint <- cut(x, breaks = q, include.lowest = TRUE, labels = paste0("Q", 1:5))
df_demo$reshist_addr1_adi_quint <- factor(df_demo$reshist_addr1_adi_quint, levels = paste0("Q", 1:5), ordered = TRUE)


# Pollution exposure
df_air <- data %>%
  dplyr::select(reshist_addr1_pm25_prenatal_avg, reshist_addr1_pm252016aa, 
                reshist_addr1_no2_prenatal_avg,  reshist_addr1_no2_2016_aavg,
                reshist_addr1_o3_prenatal_avg,   reshist_addr1_o3_2016_annavg,
                src_subject_id)

# Mental health and neurocognition
df_neuro <- data %>%
  dplyr::select(nihtbx_fluidcomp_agecorrected, nihtbx_cryst_agecorrected,
                nihtbx_totalcomp_agecorrected, cbcl_scr_syn_internal_t,
                cbcl_scr_syn_external_t, src_subject_id)

# Merge in final dataset 
df_analysis <- merge(df_demo,     df_air,   by = "src_subject_id")     
df_analysis <- merge(df_analysis, df_neuro, by = "src_subject_id")    
df_analysis <- merge(df_analysis, df_fo,    by = "src_subject_id") 
df_analysis <- merge(df_analysis, df_lt,    by = "src_subject_id")
df_analysis <- merge(df_analysis, df_tr,    by = "src_subject_id")
df_analysis <- merge(df_analysis, df_fch,   by = "src_subject_id") 

# Remove all rows with NA
df_analysis <- na.omit(df_analysis) # 4915 -> 3298

## --- Save the final dataset --------------------------------------------------
write.csv(df_analysis, file.path(df_path,"DATASET_analysis_052026.csv"), row.names = F)


## === Exploratory analyses ====================================================
## --- Descriptive statistics table --------------------------------------------

# Continuous variables
cont_vars <- c(
  "interview_age",
  "birth_weight_kg",
  "pubertal_dev_score",
  "reshist_addr1_adi_perc",
  "reshist_addr1_pm25_prenatal_avg",
  "reshist_addr1_pm252016aa",
  "reshist_addr1_no2_prenatal_avg",
  "reshist_addr1_no2_2016_aavg",
  "reshist_addr1_o3_prenatal_avg",
  "reshist_addr1_o3_2016_annavg",
  "nihtbx_fluidcomp_agecorrected",
  "nihtbx_cryst_agecorrected",
  "nihtbx_totalcomp_agecorrected",
  "cbcl_scr_syn_internal_t",
  "cbcl_scr_syn_external_t"
)

table_cont <- data.frame(
  Variable = cont_vars,
  Mean_SD = sapply(cont_vars, function(v)
    sprintf("%.2f ± %.2f",
            mean(df_analysis[[v]], na.rm = TRUE),
            sd(df_analysis[[v]], na.rm = TRUE)))
)

# Categorical variables
cat_vars <- c(
  "demo_sex_v2",
  "race_ethnicity",
  "highest_parental_education",
  "reshist_addr1_adi_quint"
)

table_cat <- bind_rows(
  lapply(cat_vars, function(v){
    
    tab <- table(df_analysis[[v]])
    perc <- prop.table(tab)*100
    
    data.frame(
      Variable = v,
      Level = names(tab),
      N = as.integer(tab),
      Percent = sprintf("%.1f%%", perc)
    )
  })
)

# Save tables
write.csv(table_cont,
          file.path(fig_path, "table1_continuous_variables.csv"),
          row.names = FALSE)

write.csv(table_cat,
          file.path(fig_path, "table1_categorical_variables.csv"),
          row.names = FALSE)

# Print
kable(table_cont, caption = "Continuous variables (Mean ± SD)")
kable(table_cat, caption = "Categorical variables (N, %)")

## --- Correlation analyses by domain ------------------------------------------

p1 <- ggpairs(df_analysis[, setdiff(c(names(df_demo),  names(df_neuro)), c("site", "src_subject_id"))])
p2 <- ggpairs(df_analysis[, setdiff(c(names(df_demo),  names(df_air)),   c("site", "src_subject_id"))])
p3 <- ggpairs(df_analysis[, setdiff(c(names(df_neuro), names(df_air)),   c("site", "src_subject_id"))])

ggsave(file.path(fig_path, "fig1a_corr_demo_neuro.png"), plot = p1, width = 20, height = 16, dpi = 300)
ggsave(file.path(fig_path, "fig1b_corr_demo_air.png"),   plot = p2, width = 20, height = 16, dpi = 300)
ggsave(file.path(fig_path, "fig1c_corr_neuro_air.png"),  plot = p3, width = 20, height = 16, dpi = 300)


## --- ADI distribution --------------------------------------------------------
png(file.path(fig_path,"fig2a_hist_adi_quintiles.png"),
    width = 4000, height = 1500, res = 300)

x <- df_analysis$reshist_addr1_adi_perc
groups <- df_analysis$reshist_addr1_adi_quint
n_groups <- table(groups)
q_min <- tapply(x, groups, min)
q_max <- tapply(x, groups, max)
hist(x, breaks = 30,
     col = "lightblue", border = "white",
     main = NA,
     xlab = "ADI Percentile",
     ylab = "Number of subjects",
     cex.main = 1.4, cex.lab = 1.2)
abline(v = q_max[1:4], col = "red", lwd = 2, lty = 2)
ymax <- par("usr")[4]

for(i in 1:5){
  xpos <- (q_min[i] + q_max[i]) / 2
  text(x = xpos, y = ymax * 0.92,
       labels = sprintf("Q%d\nN = %d\n[%d - %d]",
                        i, n_groups[i], q_min[i], q_max[i]),
       cex = 0.9, font = 1)
}
dev.off()

## --- Demographics distribution -----------------------------------------------
png(file.path(fig_path, "fig2b_demo_variables_distribution.png"), width = 4000, height = 1000, res = 300)
par(mfrow = c(1,5), mar = c(5,5,4,2), cex.main = 1.4, cex.lab = 1.2, cex.axis = 1.1)

barplot(table(df_analysis$demo_sex_v2), col = "grey80", border = "white",main = "GENDER", ylab = "Number of subjects")
barplot(table(df_analysis$race_ethnicity), col = "grey80", border = "white", main = "ETHNICITY", ylab = "Number of subjects")
hist(df_analysis$pubertal_dev_score, breaks = 25, col = "grey80", border = "white", main = "PDS", xlab = "Score (a.u.)", ylab = "Number of subjects")
hist(df_analysis$interview_age, breaks = 25, col = "grey80", border = "white", main = "AGE", xlab = "Age (months)", ylab = "Number of subjects")
hist(df_analysis$birth_weight_kg, breaks = 25, col = "grey80", border = "white", main = "BIRTH WEIGHT", xlab = "Birth weight (kg)", ylab = "Number of subjects")
dev.off()

## --- Pollutants distribution -------------------------------------------------
png(file.path(fig_path, "fig3_air_pollution_overlap_density.png"), width = 4000, height = 1000, res = 300)
par(mfrow = c(1,3), mar = c(5,5,4,2), cex.main = 1.3, cex.lab = 1.2)

x1 <- df_air$reshist_addr1_pm25_prenatal_avg
x2 <- df_air$reshist_addr1_pm252016aa
xlim_pm <- range(c(x1, x2), na.rm = TRUE)
plot(density(x1, na.rm = TRUE), col = "steelblue", lwd = 2, xlim = xlim_pm, ylim = c(0, 0.3),
     main = expression(PM[2.5]), xlab = expression(Concentration~(mu*g/m^3)))
lines(density(x2, na.rm = TRUE), col = "steelblue4", lwd = 2)
legend("topright", legend = c("Prenatal", "Childhood"),
       col = c("steelblue", "steelblue4"), lwd = 2, bty = "n")

  
x1 <- df_air$reshist_addr1_no2_prenatal_avg
x2 <- df_air$reshist_addr1_no2_2016_aavg
xlim_no2 <- range(c(x1, x2), na.rm = TRUE)
plot(density(x1, na.rm = TRUE), col = "firebrick1", lwd = 2, xlim = xlim_no2, ylim = c(0, 0.1),
     main = expression(NO[2]), xlab = expression(Concentration~(mu*g/m^3)))
lines(density(x2, na.rm = TRUE), col = "darkred", lwd = 2)
legend("topright", legend = c("Prenatal", "Childhood"),
       col = c("firebrick1", "darkred"), lwd = 2, bty = "n")

  
x1 <- df_air$reshist_addr1_o3_prenatal_avg
x2 <- df_air$reshist_addr1_o3_2016_annavg
xlim_o3 <- range(c(x1, x2), na.rm = TRUE)
plot(density(x1, na.rm = TRUE), col = "forestgreen", lwd = 2, xlim = xlim_o3, ylim = c(0, 0.12),
     main = expression(O[3]), xlab = expression(Concentration~(mu*g/m^3)))
lines(density(x2, na.rm = TRUE), col = "darkgreen", lwd = 2)
legend("topright", legend = c("Prenatal", "Childhood"),
       col = c("forestgreen", "darkgreen"), lwd = 2, bty = "n")
dev.off()

## --- Neucognition/mental health distribution ---------------------------------
png(file.path(fig_path, "fig4_mentalHealth_variables_distribution.png"), width = 4000, height = 1000, res = 300)
par(mfrow = c(1,5), mar = c(5,5,4,2), cex.main = 1.4, cex.lab = 1.2, cex.axis = 1.1)

hist(df_analysis$nihtbx_fluidcomp_agecorrected, breaks = 25, col = "grey70", border = "white",
     main = "NIH - FLUID", xlab = "Score", ylab = "Number of subjects")
hist(df_analysis$nihtbx_cryst_agecorrected, breaks = 25, col = "grey70", border = "white",
     main = "NIH - CRYSTALIZED", xlab = "Score", ylab = "Number of subjects")
hist(df_analysis$nihtbx_totalcomp_agecorrected, breaks = 25, col = "grey70", border = "white",
     main = "NIH - TOTAL", xlab = "Score", ylab = "Number of subjects")
hist(df_analysis$cbcl_scr_syn_internal_t, breaks = 25, col = "grey70", border = "white",
     main = "CBCL - INTERNALIZING", xlab = "Score", ylab = "Number of subjects")
hist(df_analysis$cbcl_scr_syn_external_t, breaks = 25, col = "grey70", border = "white",
     main = "CBCL - EXTERNALIZING", xlab = "Score", ylab = "Number of subjects")
dev.off()


## --- LEiDA FO DT distribution ------------------------------------------------
yeo_colors <- list(
  k13c1 = c(bar = "#F5B7B1", line = "#C0392B"),  # Red – DMN
  k13c2 = c(bar = "#D2B4DE", line = "#7D3C98"),  # Violet – Visual
  k13c3 = c(bar = "#D2B4DE", line = "#7D3C98"),  # Violet – Visual
  k13c4 = c(bar = "#F5B7B1", line = "#C0392B"),  # Red – DMN
  k13c5 = c(bar = "#D2B4DE", line = "#7D3C98"),  # Violet – Visual
  k13c6 = c(bar = "#D2B4DE", line = "#7D3C98"),  # Violet – Visual
  k13c7 = c(bar = "#D2B4DE", line = "#7D3C98"),  # Violet – Visual
  k13c8 = c(bar = "#A9DFBF", line = "#1E8449"),  # Dark green – Dorsal Attention
  k13c9 = c(bar = "#F5B7B1", line = "#C0392B"),  # Red – DMN
  k13c10= c(bar = "#D2B4DE", line = "#7D3C98"),  # Violet – Visual
  k13c11= c(bar = "#F9E79F", line = "#B7950B"),  # Yellow – Frontoparietal
  k13c12= c(bar = "#E8DAEF", line = "#6C3483"),  # Purple – Ventral attention
  k13c13= c(bar = "#A9DFBF", line = "#1E8449")   # Dark green – Dorsal Attention
)


df_long_prob <- df_analysis %>%
  pivot_longer(cols = starts_with("P_k13c"), names_to = "state", values_to = "probability")
df_long_prob$state <- factor(df_long_prob$state, levels = paste0("P_k13c", 1:13))

bar_colors_prob <- setNames(sapply(yeo_colors, `[[`, "bar"),paste0("P_", names(yeo_colors)))
line_colors_prob <- setNames(sapply(yeo_colors, `[[`, "line"),paste0("P_", names(yeo_colors)))

p_prob <- ggplot(df_long_prob, aes(x = state, y = probability, fill = state, color = state)) +
  geom_violin(alpha = 0.75, linewidth = 0.2, trim = TRUE) +
  stat_summary(fun = median, geom = "point", size = 2, color = "black") +
  stat_summary(fun.data = median_hilow, fun.args = list(conf.int = 0.5),
               geom = "errorbar", width = 0.2, color = "black") +
  scale_fill_manual(values = bar_colors_prob) +
  scale_color_manual(values = line_colors_prob) +
  theme_classic(base_size = 14) +
  labs(
    title = "LEiDA probabilities",
    x = "State",
    y = "Probability"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

df_long_lt <- df_analysis %>%
  pivot_longer(cols = starts_with("LT_k13c"), names_to = "state", values_to = "lifetime")
df_long_lt$state <- factor(df_long_lt$state, levels = paste0("LT_k13c", 1:13))

bar_colors_lt <- setNames(sapply(yeo_colors, `[[`, "bar"),paste0("LT_", names(yeo_colors)))
line_colors_lt <- setNames(sapply(yeo_colors, `[[`, "line"),paste0("LT_", names(yeo_colors)))

p_lt <- ggplot(df_long_lt, aes(x = state, y = lifetime, fill = state, color = state)) +
  geom_violin(alpha = 0.75, linewidth = 0.2, trim = TRUE) +
  stat_summary(fun = median, geom = "point", size = 2, color = "black") +
  stat_summary(fun.data = median_hilow, fun.args = list(conf.int = 0.5),
               geom = "errorbar", width = 0.2, color = "black") +
  scale_fill_manual(values = bar_colors_lt) +
  scale_color_manual(values = line_colors_lt) +
  theme_classic(base_size = 14) +
  labs(
    title = "LEiDA lifetimes",
    x = "State",
    y = "Lifetime"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
p_combined <- p_prob / p_lt + plot_layout(ncol = 1)
p_combined

ggsave(filename = file.path(fig_path, "fig5a_LEiDA_violin_fo_lt.png"), plot = p_combined,
  width = 12, height = 7, dpi = 300)


## --- FCH power and energy ----------------------------------------------------
fch_colors <- list(
  Harmonics_power1 = c(bar = "#FAD7A0", line = "#F5B041"), 
  Harmonics_power2 = c(bar = "#F5B041", line = "#EB984E"), 
  Harmonics_power3 = c(bar = "#EB984E", line = "#E67E22"), 
  Harmonics_power4 = c(bar = "#E67E22", line = "#CA6F1E"),  
  Harmonics_power5 = c(bar = "#CA6F1E", line = "#A04000"),  
  Harmonics_power6 = c(bar = "#A04000", line = "#A04000"),
  
  Harmonics_energy1 = c(bar = "#EAF4FB", line = "#CFE8F6"),  
  Harmonics_energy2 = c(bar = "#CFE8F6", line = "#A6D0EB"),  
  Harmonics_energy3 = c(bar = "#A6D0EB", line = "#4DA3C7"),  
  Harmonics_energy4 = c(bar = "#4DA3C7", line = "#2E86AB"),  
  Harmonics_energy5 = c(bar = "#2E86AB", line = "#1F6C91"),  
  Harmonics_energy6 = c(bar = "#1F6C91", line = "#1F6C91")
)

fch_colors_df <- data.frame(
  harmonic = names(fch_colors),
  bar = sapply(fch_colors, function(x) x["bar"]),
  line = sapply(fch_colors, function(x) x["line"]),
  row.names = NULL,
  stringsAsFactors = FALSE
)
fch_bar  <- setNames(fch_colors_df$bar,  fch_colors_df$harmonic)
fch_line <- setNames(fch_colors_df$line, fch_colors_df$harmonic)

df_long <- df_fch %>%
  pivot_longer(
    cols = matches("Harmonics_"),
    names_to = "harmonic",
    values_to = "value"
  ) %>%
  mutate(
    type = ifelse(grepl("power", harmonic, ignore.case = TRUE),
                  "Power", "Energy"),
    harmonic_n = as.integer(gsub(".*?(\\d+)$", "\\1", harmonic))
  )

df_power <- df_long %>% filter(type == "Power")
df_energy <- df_long %>% filter(type == "Energy")

fch_bar_plot  <- fch_bar[unique(df_long$harmonic)]
fch_line_plot <- fch_line[unique(df_long$harmonic)]

p_power <- ggplot(df_power, aes(x = harmonic_n, y = value,
                                fill = harmonic, color = harmonic)) +
  geom_violin(alpha = 0.75, linewidth = 0.2, trim = TRUE) +
  stat_summary(fun = median, geom = "point", size = 2, color = "black") +
  stat_summary(fun.data = median_hilow,
               fun.args = list(conf.int = 0.5),
               geom = "errorbar", width = 0.2,
               color = "black") +
  scale_fill_manual(values = fch_bar_plot) +
  scale_color_manual(values = fch_line_plot) +
  theme_classic(base_size = 14) +
  labs(title = "FCH Power", x = "Harmonic", y = "Power") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p_energy <- ggplot(df_energy, aes(x = harmonic_n, y = value,
                                  fill = harmonic, color = harmonic)) +
  geom_violin(alpha = 0.75, linewidth = 0.2, trim = TRUE) +
  stat_summary(fun = median, geom = "point", size = 2, color = "black") +
  stat_summary(fun.data = median_hilow,
               fun.args = list(conf.int = 0.5),
               geom = "errorbar", width = 0.2,
               color = "black") +
  scale_fill_manual(values = fch_bar_plot) +
  scale_color_manual(values = fch_line_plot) +
  theme_classic(base_size = 14) +
  labs(title = "FCH Energy", x = "Harmonic", y = "Energy") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p_fch <- p_power / p_energy + plot_layout(ncol = 2)
p_fch

ggsave(filename = file.path(fig_path, "fig5b_FCH_violin_power_energy.png"), plot = p_fch,
  width = 12, height = 4, dpi = 300)


