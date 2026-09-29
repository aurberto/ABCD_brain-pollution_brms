## Visualize reliable associations between dwell_times and pollution
## Aurora Berto (aurber@utu.fi) - 09/2026

# Packages
library(brms)
library(dplyr)
library(ggplot2)
library(officer)
library(patchwork)
library(readxl)

# PATHS
output_dir <- "path/to/results/model_comparison/dwell_times/figures"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

data_dir <- "path/to/data/DATASET_analysis_052026.csv"
table_dir <- "path/to/results/model_comparison/dwell_times/std_beta_pd_correction.xlsx"
models_dir <- "path/to/results/model_comparison/dwell_times/models/"

# COLORS AND LABELS
colors <- c( "#C0392B", "#7D3C98", "#7D3C98", "#C0392B", "#7D3C98", "#7D3C98", "#7D3C98", "#1E8449", "#C0392B", "#7D3C98", "#B9770E", "#CDA2D6", "#1E8449" )
labels <- c( "DMN (1)", "VIS (2)", "VIS (3)", "DMN (4)", "VIS (5)", "VIS (6)", "VIS (7)", "DAN (8)", "DMN (9)", "VIS (10)", "FPN (11)", "VAN (12)", "DAN (13)" )
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
    # State number
    state = sub( ".*LTk13c([0-9]+).*", "\\1", parameter ),
    # Complete pollution variable
    # Everything from "reshist" until ":"
    pollutant = sub(".*_(reshist.*)$", "\\1", parameter))

# Optional: inspect selected associations
# print( table_to_visualize %>%
#     select(parameter, model, harmonic, pollutant, adi))

# LOAD DATA
data <- read.csv( data_dir, header = TRUE, stringsAsFactors = TRUE )

# LOOP OVER RELIABLE ASSOCIATIONS
for (i in seq_len(nrow(table_to_visualize))) {
  
  # Information for this association
  model_name <- table_to_visualize$model[i]
  state_n <- as.numeric( table_to_visualize$state[i] )  
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
  
  
  state <- paste0( "LTk13c", state_n )
  pollutant <- table_to_visualize$pollutant[i]
  pollutant_label <- pred_name[[pollutant]]
  
  # Colors for this harmonic and selected ADI levels
  base_color <- colors[state_n]
  
  # Load corresponding model
  model_path <- paste0( models_dir, model_name, ".RDS" )
  model <- readRDS(model_path)
  
  # Conditional effects
  PP <- conditional_effects( model, effects = pollutant, resp = state )
  pp_raw <- PP[[1]]
  
  # Prepare plotting data
  pp_df <- data.frame(
    x = pp_raw[[pollutant]],
    est = pp_raw$estimate__,
    lo = pp_raw$lower__,
    hi = pp_raw$upper__ )

  # Plot
  p <- ggplot( pp_df, aes( x = x, y = est) ) +
    geom_ribbon( aes( ymin = lo, ymax = hi ), alpha = 0.08, fill = base_color, show.legend = FALSE ) +
    geom_line( colour = base_color, linewidth = 1.2 ) +
    labs( title = labels[state_n], subtitle = subtitle, x = pollutant_label, y = "Dwell time", colour = NULL ) +
    theme_classic( base_size = 13 ) +
    theme( plot.title = element_text( hjust = 0.5 ), plot.subtitle = element_text(hjust = 0.5), legend.position = "bottom" )
  
  # Output filename
  out_file <- paste0( model_name, "_S", state_n)
  
  # Save plot
  ggsave( filename = file.path( output_dir, paste0( out_file, ".png" ) ),
          plot = p, width = 4, height = 4, dpi = 300 )
  print(p)
}
