# Exploratory analysis on final dataset
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
library(colorspace)  # per lighten()/darken()

## --- Load data  --------------------------------------------------------------
df_path <- "path/to/data"
df <- read.csv(file.path(df_path, "DATASET_allVariables_complete.csv"),  header = TRUE, stringsAsFactors = TRUE)
fig_path <- "path/to/results/exploratory_analysis"

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

# LEiDA probabilities and lifetimes with K=13
df_fo <- data %>%
  dplyr::select(src_subject_id, matches("P_k13c"))
df_lt <- data %>%
  dplyr::select(src_subject_id, matches("LT_k13c"))

# FCH power and energy with H=6
df_fch <- data %>%
  dplyr::select(src_subject_id, Harmonics_power1, Harmonics_power2, Harmonics_power3,
                Harmonics_power4,  Harmonics_power5,  Harmonics_power6,
                Harmonics_energy1, Harmonics_energy2, Harmonics_energy3,
                Harmonics_energy4, Harmonics_energy5, Harmonics_energy6)

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
  "cbcl_scr_syn_external_t",
  "Harmonics_power1", "Harmonics_power2", "Harmonics_power3",
  "Harmonics_power4",  "Harmonics_power5",  "Harmonics_power6",
  "Harmonics_energy1", "Harmonics_energy2", "Harmonics_energy3",
  "Harmonics_energy4", "Harmonics_energy5", "Harmonics_energy6",
  "P_k13c1", "P_k13c2", "P_k13c3", "P_k13c4", "P_k13c5", "P_k13c6", "P_k13c7",
  "P_k13c8", "P_k13c9", "P_k13c10", "P_k13c11", "P_k13c12", "P_k13c13",
  "LT_k13c1", "LT_k13c2", "LT_k13c3", "LT_k13c4", "LT_k13c5", "LT_k13c6", "LT_k13c7",
  "LT_k13c8", "LT_k13c9", "LT_k13c10", "LT_k13c11", "LT_k13c12", "LT_k13c13"
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

## --- Pollutants per ADI quintiles --------------------------------------------
pollutant_vars <- c(
  "reshist_addr1_pm25_prenatal_avg",
  "reshist_addr1_pm252016aa",
  "reshist_addr1_no2_prenatal_avg",
  "reshist_addr1_no2_2016_aavg",
  "reshist_addr1_o3_prenatal_avg",
  "reshist_addr1_o3_2016_annavg"
)

table_pollutants_by_adi <- df_analysis %>%
  select(reshist_addr1_adi_quint, all_of(pollutant_vars)) %>%
  pivot_longer(
    cols = all_of(pollutant_vars),
    names_to = "Pollutant",
    values_to = "value"
  ) %>%
  group_by(reshist_addr1_adi_quint, Pollutant) %>%
  summarise(
    Mean = mean(value, na.rm = TRUE),
    SD   = sd(value, na.rm = TRUE),
    N    = sum(!is.na(value)),
    .groups = "drop"
  ) %>%
  mutate(Mean_SD = sprintf("%.2f ± %.2f", Mean, SD)) %>%
  select(reshist_addr1_adi_quint, Pollutant, N, Mean_SD) %>%
  arrange(Pollutant, reshist_addr1_adi_quint)

# Version "wide"
table_pollutants_by_adi_wide <- table_pollutants_by_adi %>%
  select(-N) %>%
  pivot_wider(
    names_from = reshist_addr1_adi_quint,
    values_from = Mean_SD,
    names_prefix = "ADI_Q"
  )

# Save tables
write.csv(table_pollutants_by_adi,
          file.path(fig_path, "table1_pollutants_by_adi_quint_long.csv"),
          row.names = FALSE)
write.csv(table_pollutants_by_adi_wide,
          file.path(fig_path, "table1_pollutants_by_adi_quint_wide.csv"),
          row.names = FALSE)

# Print
kable(table_pollutants_by_adi_wide,
      caption = "Pollutants (Mean ± SD) per ADI quintiles")

## --- Correlation analyses by domain ------------------------------------------
p1 <- ggpairs(df_analysis[, setdiff(c(names(df_demo),  names(df_neuro)), c("site", "src_subject_id"))])
p2 <- ggpairs(df_analysis[, setdiff(c(names(df_demo),  names(df_air)),   c("site", "src_subject_id"))])
p3 <- ggpairs(df_analysis[, setdiff(c(names(df_neuro), names(df_air)),   c("site", "src_subject_id"))])

ggsave(file.path(fig_path, "fig1a_corr_demo_neuro.png"), plot = p1, width = 20, height = 16, dpi = 300)
ggsave(file.path(fig_path, "fig1b_corr_demo_air.png"),   plot = p2, width = 20, height = 16, dpi = 300)
ggsave(file.path(fig_path, "fig1c_corr_neuro_air.png"),  plot = p3, width = 20, height = 16, dpi = 300)


## --- Shared theme for consistent styling --------------------------------
theme_paper <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = base_size * 1.1),
      plot.subtitle = element_text(hjust = 0.5, color = "grey40", size = base_size * 0.85),
      axis.title = element_text(face = "plain"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text = element_text(face = "bold", size = base_size),
      strip.background = element_rect(fill = "grey95", color = NA),
      legend.position = "none"
    )
}

## --- ADI distribution ----------------------------------------------------
quint_summary <- df_analysis %>%
  group_by(reshist_addr1_adi_quint) %>%
  summarise(N = n(),
            q_min = min(reshist_addr1_adi_perc, na.rm = TRUE),
            q_max = max(reshist_addr1_adi_perc, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(label = sprintf("Q%d\nN = %d\n[%d–%d]",
                         reshist_addr1_adi_quint, N, q_min, q_max),
         xpos = (q_min + q_max) / 2)

fig2a <- ggplot(df_analysis, aes(x = reshist_addr1_adi_perc,
                                 fill = factor(reshist_addr1_adi_quint))) +
  geom_histogram(bins = 30, color = "white", linewidth = 0.2) +
  geom_vline(data = quint_summary %>% filter(reshist_addr1_adi_quint < 5),
             aes(xintercept = q_max), color = "grey30", linetype = "dashed", linewidth = 0.4) +
  geom_text(data = quint_summary, aes(x = xpos, y = Inf, label = label),
            vjust = 1.3, size = 3.4, color = "grey20", inherit.aes = FALSE) +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "ADI Percentile", y = "Number of subjects",
       title = "Neighborhood deprivation (ADI) distribution") +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

ggsave(file.path(fig_path, "fig2a_hist_adi_quintiles.png"),
       fig2a, width = 13.3, height = 5, dpi = 300, bg = "white")

## --- Demographics distribution --------------------------------------------
p_sex <- ggplot(df_analysis, aes(x = demo_sex_v2)) +
  geom_bar(fill = "#8FA8C7", color = "white") +
  labs(title = "Sex", x = NULL, y = "Number of subjects") + theme_paper()

p_eth <- ggplot(df_analysis, aes(x = race_ethnicity)) +
  geom_bar(fill = "#8FA8C7", color = "white") +
  labs(title = "Ethnicity", x = NULL, y = NULL) + theme_paper()

p_pds <- ggplot(df_analysis, aes(x = pubertal_dev_score)) +
  geom_histogram(bins = 25, fill = "#8FA8C7", color = "white") +
  labs(title = "PDS", x = "Score (a.u.)", y = NULL) + theme_paper() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

p_age <- ggplot(df_analysis, aes(x = interview_age)) +
  geom_histogram(bins = 25, fill = "#8FA8C7", color = "white") +
  labs(title = "Age", x = "Age (months)", y = NULL) + theme_paper() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

p_bw <- ggplot(df_analysis, aes(x = birth_weight_kg)) +
  geom_histogram(bins = 25, fill = "#8FA8C7", color = "white") +
  labs(title = "Birth weight", x = "Birth weight (kg)", y = NULL) + theme_paper() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

fig2b <- p_sex + p_eth + p_pds + p_age + p_bw + plot_layout(ncol = 5)

ggsave(file.path(fig_path, "fig2b_demo_variables_distribution.png"),
       fig2b, width = 13.3, height = 4, dpi = 300, bg = "white")

## --- Pollutants distribution (density, ggplot version) --------------------
pollutant_density_df <- bind_rows(
  df_air %>% transmute(Pollutant = "PM[2.5]", Window = "Prenatal",       value = reshist_addr1_pm25_prenatal_avg),
  df_air %>% transmute(Pollutant = "PM[2.5]", Window = "Late-childhood", value = reshist_addr1_pm252016aa),
  df_air %>% transmute(Pollutant = "O[3]",    Window = "Prenatal",       value = reshist_addr1_o3_prenatal_avg),
  df_air %>% transmute(Pollutant = "O[3]",    Window = "Late-childhood", value = reshist_addr1_o3_2016_annavg),
  df_air %>% transmute(Pollutant = "NO[2]",   Window = "Prenatal",       value = reshist_addr1_no2_prenatal_avg),
  df_air %>% transmute(Pollutant = "NO[2]",   Window = "Late-childhood", value = reshist_addr1_no2_2016_aavg)
) %>%
  mutate(Pollutant = factor(Pollutant, levels = c("PM[2.5]", "O[3]", "NO[2]")),
         Window = factor(Window, levels = c("Prenatal", "Late-childhood")))

density_colors <- c(
  "PM[2.5].Prenatal" = "steelblue", "PM[2.5].Late-childhood" = "steelblue4",
  "O[3].Prenatal" = "forestgreen", "O[3].Late-childhood" = "darkgreen",
  "NO[2].Prenatal" = "firebrick1", "NO[2].Late-childhood" = "darkred"
)
pollutant_density_df <- pollutant_density_df %>%
  mutate(group_key = paste(Pollutant, Window, sep = "."))

fig3 <- ggplot(pollutant_density_df, aes(x = value, fill = group_key, color = group_key)) +
  geom_density(alpha = 0.35, linewidth = 0.8, na.rm = TRUE) +
  facet_wrap(~ Pollutant, scales = "free", nrow = 1,
             labeller = label_parsed) +
  scale_fill_manual(values = density_colors, guide = "none") +
  scale_color_manual(values = density_colors,
                     labels = c("PM[2.5].Prenatal" = "Prenatal", "PM[2.5].Late-childhood" = "Late-childhood",
                                "O[3].Prenatal" = "Prenatal", "O[3].Late-childhood" = "Late-childhood",
                                "NO[2].Prenatal" = "Prenatal", "NO[2].Late-childhood" = "Late-childhood"),
                     breaks = c("PM[2.5].Prenatal", "PM[2.5].Late-childhood", "O[3].Prenatal", "O[3].Late-childhood",
                                "NO[2].Prenatal", "NO[2].Late-childhood"),
                     name = NULL) +
  labs(x = expression(Concentration~(mu*g/m^3)), y = "Density",
       title = "Air pollution exposure distributions") +
  theme_paper() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 0, hjust = 0.5))

ggsave(file.path(fig_path, "fig3_air_pollution_overlap_density.png"),
       fig3, width = 13.3, height = 4.5, dpi = 300, bg = "white")

## --- Violin plots: pollutants per ADI quintile ----------------------------
pollutant_map <- tibble::tribble(
  ~var,                              ~Pollutant, ~Window,
  "reshist_addr1_pm25_prenatal_avg", "PM2.5",    "Prenatal",
  "reshist_addr1_pm252016aa",        "PM2.5",    "Late-childhood",
  "reshist_addr1_o3_prenatal_avg",   "O3",       "Prenatal",
  "reshist_addr1_o3_2016_annavg",    "O3",       "Late-childhood",
  "reshist_addr1_no2_prenatal_avg",  "NO2",      "Prenatal",
  "reshist_addr1_no2_2016_aavg",     "NO2",      "Late-childhood"
)

df_violin <- df_analysis %>%
  select(reshist_addr1_adi_quint, all_of(pollutant_map$var)) %>%
  pivot_longer(
    cols = all_of(pollutant_map$var),
    names_to = "var",
    values_to = "value"
  ) %>%
  left_join(pollutant_map, by = "var") %>%
  mutate(
    Pollutant = factor(Pollutant, levels = c("PM2.5", "O3", "NO2")),
    Window    = factor(Window, levels = c("Prenatal", "Late-childhood")),
    fill_group = paste(Pollutant, Window, reshist_addr1_adi_quint)
  ) %>%
  filter(!is.na(value), !is.na(reshist_addr1_adi_quint))

base_colors <- c(
  "PM2.5 Prenatal"       = "steelblue",
  "PM2.5 Late-childhood" = "steelblue4",
  "O3 Prenatal"          = "forestgreen",
  "O3 Late-childhood"    = "darkgreen",
  "NO2 Prenatal"         = "firebrick1",
  "NO2 Late-childhood"   = "darkred"
)

lighten_amounts <- c(Q1 = 0.5, Q2 = 0.25, Q3 = 0, Q4 = -0.25, Q5 = -0.5)

pollutant_colors <- unlist(lapply(names(base_colors), function(pw) {
  shades <- lighten(base_colors[[pw]], amount = lighten_amounts)
  setNames(shades, paste(pw, names(lighten_amounts)))
}))

fig_pollutants_adi <- ggplot(df_violin,
                             aes(x = reshist_addr1_adi_quint, y = value,
                                 fill = fill_group)) +
  geom_violin(trim = TRUE, alpha = 0.9, linewidth = 0.3) +
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.8, fill = "white") +
  facet_grid(Pollutant ~ Window, scales = "free_y") +
  scale_fill_manual(values = pollutant_colors, guide = "none") +
  labs(x = "ADI quintile", y = expression(Concentration~(mu*g/m^3))) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  theme_paper(base_size = 16) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  labs(title = "Pollutant exposure across ADI quintiles")

ggsave(file.path(fig_path, "fig3b_pollutants_by_adi_violin.png"),
       fig_pollutants_adi, width = 13.3, height = 8, dpi = 300, bg = "white")

## --- Neurocognition/mental health distribution ----------------------------
p_nih_fluid <- ggplot(df_analysis, aes(x = nihtbx_fluidcomp_agecorrected)) +
  geom_histogram(bins = 25, fill = "grey70", color = "white") +
  labs(title = "NIH – Fluid", x = "Score", y = "Number of subjects") +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

p_nih_cryst <- ggplot(df_analysis, aes(x = nihtbx_cryst_agecorrected)) +
  geom_histogram(bins = 25, fill = "grey70", color = "white") +
  labs(title = "NIH – Crystallized", x = "Score", y = NULL) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

p_nih_total <- ggplot(df_analysis, aes(x = nihtbx_totalcomp_agecorrected)) +
  geom_histogram(bins = 25, fill = "grey70", color = "white") +
  labs(title = "NIH – Total", x = "Score", y = NULL) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

p_cbcl_int <- ggplot(df_analysis, aes(x = cbcl_scr_syn_internal_t)) +
  geom_histogram(bins = 25, fill = "grey70", color = "white") +
  labs(title = "CBCL – Internalizing", x = "Score", y = NULL) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

p_cbcl_ext <- ggplot(df_analysis, aes(x = cbcl_scr_syn_external_t)) +
  geom_histogram(bins = 25, fill = "grey70", color = "white") +
  labs(title = "CBCL – Externalizing", x = "Score", y = NULL) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

fig4 <- p_nih_fluid + p_nih_cryst + p_nih_total + p_cbcl_int + p_cbcl_ext +
  plot_layout(ncol = 5)

ggsave(file.path(fig_path, "fig4_mentalHealth_variables_distribution.png"),
       fig4, width = 13.3, height = 4, dpi = 300, bg = "white")

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

## --- LEiDA FO/LT: cleaner labels + network legend --------------------------

network_labels <- c(
  k13c1 = "DMN", k13c2 = "Visual", k13c3 = "Visual", k13c4 = "DMN",
  k13c5 = "Visual", k13c6 = "Visual", k13c7 = "Visual",
  k13c8 = "Dorsal Att.", k13c9 = "DMN", k13c10 = "Visual",
  k13c11 = "Frontoparietal", k13c12 = "Ventral Att.", k13c13 = "Dorsal Att."
)

state_labels_prob <- setNames(paste0(1:13, "\n(", network_labels, ")"), paste0("P_k13c", 1:13))
state_labels_lt   <- setNames(paste0(1:13, "\n(", network_labels, ")"), paste0("LT_k13c", 1:13))

network_legend_colors <- c(
  "DMN" = "#C0392B", "Visual" = "#7D3C98", "Dorsal Att." = "#1E8449",
  "Frontoparietal" = "#B7950B", "Ventral Att." = "#6C3483"
)
p_network_legend <- ggplot(data.frame(network = names(network_legend_colors)),
                           aes(x = network, y = 1, fill = network)) +
  geom_col() +
  scale_fill_manual(values = network_legend_colors, name = "Network") +
  theme_void() +
  theme(legend.position = "bottom")
network_legend <- cowplot::get_legend(p_network_legend)  # o get_plot_component() con patchwork/cowplot recenti

p_prob <- p_prob +
  scale_x_discrete(labels = state_labels_prob) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9))

p_lt <- p_lt +
  scale_x_discrete(labels = state_labels_lt) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9))

p_combined <- (p_prob / p_lt) / wrap_elements(network_legend) +
  plot_layout(heights = c(5, 5, 0.5))

ggsave(file.path(fig_path, "fig5a_LEiDA_violin_fo_lt.png"),
       p_combined, width = 13.3, height = 8, dpi = 300, bg = "white")

## --- FCH power/energy: cleaner labels ---------------------------------------
p_power <- p_power +
  scale_x_continuous(breaks = 1:6, labels = paste0("ψ", 1:6)) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

p_energy <- p_energy +
  scale_x_continuous(breaks = 1:6, labels = paste0("ψ", 1:6)) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

p_fch <- p_power / p_energy + plot_layout(ncol = 2)

ggsave(file.path(fig_path, "fig5b_FCH_violin_power_energy.png"),
       p_fch, width = 13.3, height = 4, dpi = 300, bg = "white")
