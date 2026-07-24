## Visualize reliable associations between FO and pollution
# Berto Aurora, 05/2026 aurber@utu.fi

# This script saves, for each model, the respective regression table, and 
# visualizes all reliable associations (95% CI excluding zero) between LEiDA
# FO and pollutant's exposure, with regression lines color coded according 
# to the respective RSN network.

# ------------------------------------------------------------------------------
# Packages
# ------------------------------------------------------------------------------
library(brms)
library(dplyr)
library(ggplot2)
library(officer)

# ------------------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------------------
output_dir <- "~/path/to/results/model_comparison/fractional_occupancy/00_figures"
tables_dir <- "~/path/to/results/model_comparison/fractional_occupancy/00_summary_tables"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Models
# ------------------------------------------------------------------------------
no2_pre <- "~/path/to/results/model_comparison/fractional_occupancy/models/fo_no2_fit5_mod_prenatal.RDS"
no2_cur <- "~/path/to/results/model_comparison/fractional_occupancy/models/fo_no2_fit6_mod_current.RDS"

pm25_pre <- "~/path/to/results/model_comparison/fractional_occupancy/models/fo_pm_fit5_mod_prenatal.RDS"
pm25_cur <- "~/path/to/results/model_comparison/fractional_occupancy/models/fo_pm_fit6_mod_current.RDS"

o3_pre <- "~/path/to/results/model_comparison/fractional_occupancy/models/fo_o3_fit5_mod_prenatal.RDS"
o3_cur <- "~/path/to/results/model_comparison/fractional_occupancy/models/fo_o3_fit6_mod_current.RDS"

# ------------------------------------------------------------------------------
# Colors for FO states
# ------------------------------------------------------------------------------
colors <- c(
  "#C0392B",
  "#7D3C98",
  "#7D3C98",
  "#C0392B",
  "#7D3C98",
  "#7D3C98",
  "#7D3C98",
  "#1E8449",
  "#C0392B",
  "#7D3C98",
  "#B9770E",
  "#CDA2D6",
  "#1E8449"
)

labels <- c(
  "DMN (1)",
  "VIS (2)",
  "VIS (3)",
  "DMN (4)",
  "VIS (5)",
  "VIS (6)",
  "VIS (7)",
  "DAN (8)",
  "DMN (9)",
  "VIS (10)",
  "FPN (11)",
  "VAN (12)",
  "DAN (13)"
)

# ------------------------------------------------------------------------------
# Function
# ------------------------------------------------------------------------------
analyze_model <- function(model_path,
                          out_name,
                          out_file,
                          output_dir,
                          pred_name,
                          tables_dir,
                          colors,
                          labels) {
  
  model <- readRDS(model_path)
  
  # ------------------------------------------------------------------
  # Save coefficient table
  # ------------------------------------------------------------------
  coef_df <- as.data.frame(fixef(model))
  coef_df$Parameter <- rownames(coef_df)
  coef_df <- coef_df %>%
    select(Parameter, Estimate, Est.Error, Q2.5, Q97.5)
  
  doc <- read_docx()
  doc <- body_add_par( doc, value = out_name, style = "heading 1" )
  doc <- body_add_table( doc, value = coef_df, style = "table_template")
  print( doc, target = file.path( tables_dir, paste0(out_file, "_summary.docx")))
  
  # ------------------------------------------------------------------
  # Significant states
  # ------------------------------------------------------------------
  sig_terms <- coef_df %>%
    filter( grepl( paste0("_", pred_name, "$"), Parameter)) %>%
    filter( (Q2.5 > 0 & Q97.5 > 0) | (Q2.5 < 0 & Q97.5 < 0))
  
  if (nrow(sig_terms) == 0) {
    message( "No significant effects for ", out_name)
    return(NULL)
  }
  
  # ------------------------------------------------------------------
  # Extract significant FO names
  # ------------------------------------------------------------------
  sig_states <- sub( paste0("_", pred_name), "", sig_terms$Parameter)
  sig_states <- sub( "^muPk13c", "P_k13c", sig_states)

  message( "Significant states: ", paste(sig_states, collapse = ", "))
  
  # ------------------------------------------------------------------
  # Conditional effects
  # ------------------------------------------------------------------
  PP <- conditional_effects( model, categorical = TRUE )
  pp_name <- paste0( pred_name, ":cats__" )
  pp_raw <- PP[[pp_name]]
  predictor_col <- pred_name
  
  pp_df <- data.frame(
    var = pp_raw[[predictor_col]],
    est = pp_raw$estimate__,
    lo = pp_raw$lower__,
    hi = pp_raw$upper__,
    cat = pp_raw$cats__
  )
  
  pp_sel <- pp_df %>%
    filter(cat %in% sig_states)
  
  # ------------------------------------------------------------------
  # Colors and labels
  # ------------------------------------------------------------------
  state_nums <- as.numeric( sub("P_k13c", "", sig_states) )
  cat_colors <- setNames( colors[state_nums], sig_states )
  facet_labels <- setNames( labels[state_nums], sig_states )
  
  # ------------------------------------------------------------------
  # Plot
  # ------------------------------------------------------------------
  plots <- vector("list", length(sig_states))
  
  for(i in seq_along(sig_states)){
    st <- sig_states[i]
    dat <- pp_df %>%
      filter(cat == st)
    state_num <- as.numeric(sub("P_k13c", "", st))
    
    plots[[i]] <-
      ggplot( dat, aes( x = var, y = est ) ) +
      geom_ribbon( aes( ymin = lo, ymax = hi ),
        fill = colors[state_num], alpha = 0.08, colour = NA ) +
      geom_line( colour = colors[state_num], linewidth = 1.2 ) +
      coord_cartesian( ylim = c(0, 0.20) ) +
      labs(
        title = labels[state_num],
        x = out_name,
        y = "Fractional occupancy"
      ) +
      theme_classic(base_size = 12) +
      theme( plot.title = element_text(hjust = 0.5) )
  }
  
  p <- wrap_plots(plots, ncol = 5)
  ggsave( filename = file.path( output_dir, paste0(out_file, ".png") ),
    plot = p, width = 12, height = 4, dpi = 300 )
}

# ------------------------------------------------------------------------------
# Models list
# ------------------------------------------------------------------------------
model_list <- list(
  
  list(
    path = no2_pre,
    out_name = "Prenatal NO2",
    out_file = "no2_pre",
    pred_name = "reshist_addr1_no2_prenatal_avg"
  ),

  list(
    path = no2_cur,
    out_name = "Late-childhood NO2",
    out_file = "no2_cur",
    pred_name = "reshist_addr1_no2_2016_aavg"
  ),
  
  list(
    path = pm25_pre,
    out_name = "Prenatal PM2.5",
    out_file = "pm25_pre",
    pred_name = "reshist_addr1_pm25_prenatal_avg"
  ),
  
  list(
    path = pm25_cur,
    out_name = "Late-childhood PM2.5",
    out_file = "pm25_cur",
    pred_name = "reshist_addr1_pm252016aa"
  ),
  
  list(
    path = o3_pre,
    out_name = "Prenatal O3",
    out_file = "o3_pre",
    pred_name = "reshist_addr1_o3_prenatal_avg"
  ),

  list(
    path = o3_cur,
    out_name = "Late-childhood O3",
    out_file = "o3_cur",
    pred_name = "reshist_addr1_o3_2016_annavg"
  )
  
)

# ------------------------------------------------------------------------------
# Run
# ------------------------------------------------------------------------------
for (m in model_list) {
  
  analyze_model(
    model_path = m$path,
    out_name = m$out_name,
    out_file = m$out_file,
    pred_name = m$pred_name,
    output_dir = output_dir,
    tables_dir = tables_dir,
    colors = colors,
    labels = labels
  )
  
}