## Visualize reliable associations between energy and pollution
## Aurora Berto - 06/2026 aurber@utu.fi

# This script saves, for each model, the respective regression table, and 
# visualizes all reliable associations (95% CI excluding zero) including
# ADI x pollution predictor, with regression lines color coded according to the 
# respective harmonic. ADI first quintile (Q1) is always shown as reference.

# ------------------------------------------------------------------------------
# Packages
# ------------------------------------------------------------------------------
library(brms)
library(dplyr)
library(ggplot2)
library(officer)
library(patchwork)

# ------------------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------------------
output_dir <- "~/path/to/results/model_comparison/energy/00_figures"
tables_dir <- "~/path/to/results/model_comparison/energy/00_summary_tables"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Dataset
# ------------------------------------------------------------------------------
data_path <- "~/path/to/data/DATASET_analysis_052026.csv"

# ------------------------------------------------------------------------------
# Models
# ------------------------------------------------------------------------------
no2_pre <- "~/path/to/results/model_comparison/energy/models/energy_no2_fit5_mod_prenatal.RDS"
no2_cur <- "~/path/to/results/model_comparison/energy/models/energy_no2_fit6_mod_current.RDS"

pm25_pre <- "~/path/to/results/model_comparison/energy/models/energy_pm_fit5_mod_prenatal.RDS"
pm25_cur <- "~/path/to/results/model_comparison/energy/models/energy_pm_fit6_mod_current.RDS"

o3_pre <- "~/path/to/results/model_comparison/energy/models/energy_o3_fit5_mod_prenatal.RDS"
o3_cur <- "~/path/to/results/model_comparison/energy/models/energy_o3_fit6_mod_current.RDS"

# ------------------------------------------------------------------------------
# Colors for FCH power
# ------------------------------------------------------------------------------
colors <- c(
  "#80CFFF",
  "#5CB8FF",
  "#38A1FF",
  "#1A8AF2",
  "#006ED9",
  "#0056B3"
)

labels <- c(
  "FCH 1",
  "FCH 2",
  "FCH 3",
  "FCH 4",
  "FCH 5",
  "FCH 6"
)

# ------------------------------------------------------------------------------
# Function
# ------------------------------------------------------------------------------
analyze_interaction_model <- function(
    data_path,
    model_path,
    out_name,
    out_file,
    output_dir,
    pred_name,
    moderator_name,
    moderator_levels,
    tables_dir,
    colors,
    labels
) {
  
  # ------------------------------------------------------------
  # Load model and data
  # ------------------------------------------------------------
  model <- readRDS(model_path)
  data <- read.csv(data_path, header = TRUE, stringsAsFactors = TRUE)
  
  # ------------------------------------------------------------
  # Save coefficient table
  # ------------------------------------------------------------
  coef_df <- as.data.frame(fixef(model))
  coef_df$Parameter <- rownames(coef_df)
  coef_df <- coef_df %>%
    select( Parameter, Estimate, Est.Error, Q2.5, Q97.5 )
  
  doc <- read_docx()
  doc <- body_add_par( doc, value = out_name, style = "heading 1" )
  doc <- body_add_table( doc, value = coef_df, style = "table_template" )
  
  print( doc, target = file.path( tables_dir, paste0(out_file, "_summary.docx")))
  
  # ------------------------------------------------------------
  # Significant interactions
  # ------------------------------------------------------------
  interaction_terms <- coef_df %>%
    filter( grepl( paste0("_", pred_name, ":"), Parameter))
  sig_terms <- interaction_terms %>%
    filter( (Q2.5 > 0 & Q97.5 > 0) | (Q2.5 < 0 & Q97.5 < 0))
  
  if(nrow(sig_terms) == 0){
    message("No significant interactions for ", out_name)
    return(NULL)
  }
  
  sig_info <- sig_terms %>%
    mutate(
      state = sub(paste0("_", pred_name, ":.*"), "", Parameter),
      state = sub("^Harmonicsenergy", "Harmonics_energy", state),
      mod = sub(".*:", "", Parameter),
      mod = sub(paste0("^", moderator_name), "", mod)
    )
  
  # ------------------------------------------------------------
  # Harmonics with significant interaction
  # ------------------------------------------------------------
  sig_harmonics <- sub( paste0("_", pred_name, ":.*"), "", sig_terms$Parameter)
  sig_harmonics <- sub( "^Harmonicsenergy", "Harmonics_energy", sig_harmonics)
  sig_harmonics <- unique(sig_harmonics)
  message( "Significant harmonics: ", paste(sig_harmonics, collapse = ", "))
  
  # ------------------------------------------------------------
  # Significant moderator levels
  # ------------------------------------------------------------
  sig_mods <- sig_terms$Parameter %>%
    sub(".*:", "", .) %>%
    sub(paste0("^", moderator_name), "", .)
  sig_mods <- unique(c("Q1", sig_mods))
  message( "Significant moderator levels: ", paste(sig_mods, collapse = ", "))
  
  for(term in sig_harmonics){
    
    message("Plotting ", term)
    ## harmonic number
    fch_n <- as.numeric( sub("Harmonics_energy","",term) )
    
    ## moderator levels with significant interaction
    mods <- unique(c( "Q1", sig_info$mod[sig_info$state == term]))
    # str(mods)
    
    ## conditional effects
    cond_df <- data.frame( x = moderator_levels )
    names(cond_df) <- moderator_name
    resp_name <- sub( "Harmonics_energy", "Harmonicsenergy", term)
    
    PP <- conditional_effects(
      model,
      effects = pred_name,
      resp = resp_name,
      conditions = cond_df
    )
    
    pp_raw <- PP[[1]]
    pp_df <- data.frame(
      x   = pp_raw[[pred_name]],
      mod = pp_raw[[moderator_name]],
      est = pp_raw$estimate__,
      lo  = pp_raw$lower__,
      hi  = pp_raw$upper__
    )
    
    pp_df <- pp_df %>%
      filter(mod %in% mods)
    
    ## colours
    cols <- c(
      Q1 = "grey70",
      Q2 = adjustcolor(colors[as.numeric(fch_n)], alpha.f = 0.40),
      Q3 = adjustcolor(colors[as.numeric(fch_n)], alpha.f = 0.60),
      Q4 = adjustcolor(colors[as.numeric(fch_n)], alpha.f = 0.80),
      Q5 = colors[as.numeric(fch_n)]
    )
    cols <- cols[mods]
    pp_df$mod <- factor(pp_df$mod, levels = mods)
    
    ## plot
    p <-
      ggplot( pp_df, aes( x = x, y = est, 
                          colour = mod, fill = mod, group = mod )) +
      geom_ribbon( aes( ymin = lo, ymax = hi ),
        alpha = 0.08, colour = NA, show.legend = FALSE ) +
      geom_line( linewidth = 1.2 ) +
      scale_colour_manual( values = cols, drop = FALSE ) +
      scale_fill_manual( values = cols, drop = FALSE ) +
      labs(
        title = labels[fch_n],
        x = out_name,
        y = "energy",
        colour = NULL
      ) +
      theme_classic(base_size = 13) +
      theme( plot.title = element_text(hjust = 0.5), legend.position = "bottom" )
    
    ggsave( filename = file.path( output_dir, paste0( out_file, "_H", fch_n, ".png" )),
      plot = p, width = 3, height = 3, dpi = 300 )
  }
}
# ------------------------------------------------------------------------------
# Models list
# ------------------------------------------------------------------------------
model_list <- list(
  
  list(
    path = no2_pre,
    out_name = "Prenatal NO2",
    out_file = "no2_pre_ADImod",
    pred_name = "reshist_addr1_no2_prenatal_avg"
  ),
  
  list(
    path = no2_cur,
    out_name = "Late-childhood NO2",
    out_file = "no2_cur_ADImod",
    pred_name = "reshist_addr1_no2_2016_aavg"
  ),
  
  list(
    path = pm25_pre,
    out_name = "Prenatal PM2.5",
    out_file = "pm25_pre_ADImod",
    pred_name = "reshist_addr1_pm25_prenatal_avg"
  ),
  
  list(
    path = pm25_cur,
    out_name = "Late-childhood PM2.5",
    out_file = "pm25_cur_ADImod",
    pred_name = "reshist_addr1_pm252016aa"
  ),
  
  list(
    path = o3_pre,
    out_name = "Prenatal O3",
    out_file = "o3_pre_ADImod",
    pred_name = "reshist_addr1_o3_prenatal_avg"
  ),
  
  list(
    path = o3_cur,
    out_name = "Late-childhood O3",
    out_file = "o3_cur_ADImod",
    pred_name = "reshist_addr1_o3_2016_annavg"
  )
  
)

# ------------------------------------------------------------------------------
# Run
# ------------------------------------------------------------------------------
for (m in model_list) {
  
  analyze_interaction_model(
    data_path = data_path,
    model_path = m$path,
    out_name = m$out_name,
    out_file = m$out_file,
    output_dir = output_dir,
    pred_name = m$pred_name,
    moderator_name = "reshist_addr1_adi_quint",
    moderator_levels = c(
      "Q1",
      "Q2",
      "Q3",
      "Q4",
      "Q5"
    ),
    tables_dir = tables_dir,
    colors = colors,
    labels = labels
  )
  
}