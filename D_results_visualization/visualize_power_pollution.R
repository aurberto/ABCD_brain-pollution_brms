## Visualize reliable associations between power and pollution
## Aurora Berto (aurber@utu.fi) - 09/2026

# Packages
library(brms)
library(dplyr)
library(ggplot2)
library(officer)
library(patchwork)
library(readxl)

# PATHS
output_dir <- "path/to/results/model_comparison/power/figures"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

data_dir <- "path/to/data/DATASET_analysis_052026.csv"
table_dir <- "path/to/results/model_comparison/power/std_beta_pd_correction.xlsx"
models_dir <- "path/to/results/model_comparison/power/models/"

# COLORS AND LABELS
colors <- c( "#FFB480", "#FF9A5C", "#FF8038", "#F26A1A", "#D95700", "#B34700" )
labels <- c( "FCH 1", "FCH 2", "FCH 3", "FCH 4", "FCH 5", "FCH 6" )
mod_levels <- c( "Q1", "Q2", "Q3", "Q4", "Q5" )
mod_name <- "reshist_addr1_adi_quint"
pred_name <- list(
  reshist_addr1_pm25_prenatal_avg = "Prenatal PM2.5",
  reshist_addr1_pm252016aa = "Late-childhood PM2.5",
  reshist_addr1_o3_prenatal_avg = "Prenatal O3",
  reshist_addr1_o3_2016_annavg = "Late-childhood O3",
  reshist_addr1_no2_prenatal_avg = "Prenatal NO2",
  reshist_addr1_no2_2016_aavg = "Late-childhood NO2" )

# READ RESULTS TABLE
table <- read_excel(table_dir)

# NOTE: assumes the table has a column with the multiplicity-corrected
# posterior probability. Adjust the column name below (currently `pd_adj`)
# if yours is named differently (e.g. `pd.adj`).

# SELECT RELIABLE ASSOCIATIONS
table_to_visualize <- table %>%
  filter(pd > 0.975) %>%
  mutate(
    # Model name = everything before the first "."
    model = sub( "\\..*$", "", parameter ),
    # Harmonic number
    harmonic = sub( ".*Harmonicspower([0-9]+).*", "\\1", parameter ),
    # Complete pollution variable
    # Everything from "reshist" until ":"
    pollutant = sub( ".*_(reshist.*?):.*", "\\1", parameter ),
    # ADI quintile
    adi = sub( ".*adi_quintQ([0-9]+).*", "\\1", parameter ))

# Optional: inspect selected associations
# print( table_to_visualize %>%
#     select(parameter, model, harmonic, pollutant, adi))

# LOAD DATA
data <- read.csv( data_dir, header = TRUE, stringsAsFactors = TRUE )

# LOOP OVER RELIABLE ASSOCIATIONS
for (i in seq_len(nrow(table_to_visualize))) {
  
  # Information for this association
  model_name <- table_to_visualize$model[i]
  fch_n <- as.numeric( table_to_visualize$harmonic[i] )  
  b_est <- table_to_visualize$estimate[i]
  b_lo  <- table_to_visualize$lower[i]
  b_hi  <- table_to_visualize$upper[i]
  pd_adj_i <- table_to_visualize$pd.adj[i]

  # Extra annotation depending on the corrected posterior probability:
  # - pd > 0.975 but pd.adj < 0.975 -> no annotation
  # - pd.adj >= 0.975 otherwise -> show exact rounded value with an asterisk
  # subtitle <- if (is.na(pd_adj_i) || pd_adj_i < 0.975) {
  #   bquote( atop( beta == .(sprintf("%.2f", b_est)) * 
  #                   ", 95% CI [" * .(sprintf("%.2f", b_lo)) * ", " * .(sprintf("%.2f", b_hi)) * "]", 
  #                 pd[adj] * " < 0.975" ) )
  # } else {
  #   bquote( atop( beta == .(sprintf("%.2f", b_est)) *  
  #                   ", 95% CI [" * .(sprintf("%.2f", b_lo)) * ", " * .(sprintf("%.2f", b_hi)) * "]", 
  #                 pd[adj] * " = " * .(sprintf("%.3f", pd_adj_i)) ) )
  # }
  
  star <- if (!is.na(pd_adj_i) && pd_adj_i > 0.975) "*" else ""
  subtitle <- sprintf( "β = %.2f, 95%% CI [%.2f, %.2f]%s", b_est, b_lo, b_hi, star )
  
  
  harmonic <- paste0( "Harmonicspower", fch_n )
  pollutant <- table_to_visualize$pollutant[i]
  pollutant_label <- pred_name[[pollutant]]
  significant_adi <- paste0( "Q", table_to_visualize$adi[i] )
  adi <- unique( c( "Q1", significant_adi )) # Q1 + significant ADI level
  
  # Colors for this harmonic and selected ADI levels
  base_color <- colors[fch_n]
  mod_colors <- c(
    Q1 = "grey70",
    Q2 = adjustcolor(base_color, alpha.f = 0.40),
    Q3 = adjustcolor(base_color, alpha.f = 0.60),
    Q4 = adjustcolor(base_color, alpha.f = 0.80),
    Q5 = base_color )
  cols <- mod_colors[adi]
  
  # Load corresponding model
  model_path <- paste0( models_dir, model_name, ".RDS" )
  model <- readRDS(model_path)
  
  # Conditions for conditional effects
  adi_levels <- data.frame( x = mod_levels )
  names(adi_levels) <- mod_name
  
  # Conditional effects
  PP <- conditional_effects( model, effects = pollutant, resp = harmonic, conditions = adi_levels )
  pp_raw <- PP[[1]]
  
  # Prepare plotting data
  pp_df <- data.frame(
    x = pp_raw[[pollutant]],
    mod = pp_raw[[mod_name]],
    est = pp_raw$estimate__,
    lo = pp_raw$lower__,
    hi = pp_raw$upper__ )
  
  # Keep only Q1 and the significant ADI level
  pp_df <- pp_df %>%
    filter( mod %in% adi )
  
  # Correct order of ADI levels
  pp_df$mod <- factor( pp_df$mod, levels = adi )
  
  # Colors for selected ADI levels
  cols <- mod_colors[adi]
  
  # Plot
  p <- ggplot( pp_df, aes( x = x, y = est, colour = mod, fill = mod, group = mod ) ) +
    geom_ribbon( aes( ymin = lo, ymax = hi ), alpha = 0.08, colour = NA, show.legend = FALSE ) +
    geom_line( linewidth = 1.2 ) +
    scale_colour_manual( values = cols, drop = FALSE ) +
    scale_fill_manual( values = cols, drop = FALSE ) +
    labs( title = labels[fch_n], subtitle = subtitle, x = pollutant_label, y = "Power", colour = NULL ) +
    theme_classic( base_size = 13 ) +
    theme( plot.title = element_text( hjust = 0.5 ), plot.subtitle = element_text(hjust = 0.5), legend.position = "bottom" )
  
  # Output filename
  out_file <- paste0( model_name, "_H", fch_n, "_", adi[2])
  
  # Save plot
  ggsave( filename = file.path( output_dir, paste0( out_file, ".png" ) ),
    plot = p, width = 4, height = 4, dpi = 300 )
  print(p)
}
