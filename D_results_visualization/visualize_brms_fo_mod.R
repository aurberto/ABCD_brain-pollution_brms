## Visualize reliable associations between FO and pollution
## Aurora Berto - 06/2026 aurber@utu.fi

# This script saves, for each model, the respective regression table, and 
# visualizes all reliable associations (95% CI excluding zero) including
# ADI x pollution predictor, with regression lines color coded according to the 
# respective RSN network. ADI first quintile (Q1) is always shown as reference.

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
output_dir <- "~/path/to/results/model_comparison/fractional_occupancy/00_figures"
tables_dir <- "~/path/to/results/model_comparison/fractional_occupancy/00_summary_tables"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Dataset
# ------------------------------------------------------------------------------
data_path <- "~/path/to/data/DATASET_analysis_052026.csv"

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
    select(Parameter, Estimate, Est.Error, Q2.5, Q97.5)
  
  doc <- read_docx()
  doc <- body_add_par( doc, value = out_name, style = "heading 1" )
  doc <- body_add_table( doc, value = coef_df, style = "table_template" )
  
  print( doc, target = file.path( tables_dir, paste0(out_file, "_summary.docx")))
  
  # ------------------------------------------------------------
  # Significant interactions
  # ------------------------------------------------------------
  interaction_terms <- coef_df %>%
    filter( grepl( paste0("_", pred_name, ":"), Parameter ))
  sig_terms <- interaction_terms %>%
    filter( (Q2.5 > 0 & Q97.5 > 0) | (Q2.5 < 0 & Q97.5 < 0))
  
  if(nrow(sig_terms) == 0){
    message( "No significant interactions for ", out_name)
    return(NULL)
  }
  
  sig_info <- sig_terms %>%
    mutate(
      state = sub(paste0("_", pred_name, ":.*"), "", Parameter),
      state = sub("^muPk13c", "P_k13c", state),
      mod = sub(".*:", "", Parameter),
      mod = sub(paste0("^", moderator_name), "", mod)
    )
  
  # ------------------------------------------------------------
  # Extract significant states
  # ------------------------------------------------------------
  sig_states <- sub( paste0("_", pred_name, ":.*"), "", sig_terms$Parameter)
  sig_states <- sub( "^muPk13c", "P_k13c", sig_states)
  sig_states <- unique(sig_states)
  message( "Significant states: ", paste(sig_states, collapse = ", "))
  
  # ------------------------------------------------------------
  # Significant moderator levels
  # ------------------------------------------------------------
  sig_mods <- sig_terms$Parameter %>%
    sub(".*:", "", .) %>%
    sub(paste0("^", moderator_name), "", .)
  sig_mods <- unique(c("Q1", sig_mods))
  message( "Significant moderator levels: ", paste(sig_mods, collapse = ", "))
  
  # ------------------------------------------------------------
  # Conditional effects
  # ------------------------------------------------------------
  cond_df <- data.frame( x = moderator_levels )
  names(cond_df) <- moderator_name
  # str(cond_df)

  PP <- conditional_effects(
    model,
    effects = pred_name,
    categorical = TRUE,
    conditions = cond_df
  )

  pp_raw <- PP[[1]]
  pp_df <- data.frame(
    x   = pp_raw[[pred_name]],
    mod = pp_raw[[moderator_name]],
    est = pp_raw$estimate__,
    lo  = pp_raw$lower__,
    hi  = pp_raw$upper__,
    cat = pp_raw$cats__
  )
  
  pp_df$cat <- as.character(pp_df$cat)
  sig_states <- as.character(sig_states)
  pp_sel <- pp_df %>%
    filter(
      cat %in% sig_states,
      mod %in% sig_mods
    )

  # ------------------------------------------------------------
  # Create state × moderator labels
  # ------------------------------------------------------------
  pp_sel <- pp_sel %>%
    mutate(cat_mod = paste(cat,mod,sep = "_"))

  # ------------------------------------------------------------
  # Colors
  # ------------------------------------------------------------
  state_nums <- as.numeric(
    sub("P_k13c", "", sig_states)
  )

  plot_colors <- c()

  for(i in seq_along(sig_states)){
    state_name <- sig_states[i]
    base_col <- colors[state_nums[i]]
    shades <- colorRampPalette(
      c("grey70", base_col)
    )(length(moderator_levels))
    names(shades) <- paste(
      state_name,
      moderator_levels,
      sep = "_"
    )
    plot_colors <- c(
      plot_colors,
      shades
    )
  }

  # ------------------------------------------------------------
  # Plot
  # ------------------------------------------------------------
  plots <- vector("list", length(sig_states))
  
  for(i in seq_along(sig_states)){
    st <- sig_states[i]
    mods_state <- c( "Q1", sig_info$mod[sig_info$state == st] )
    mods_state <- unique(mods_state)
    
    dat <- pp_df %>%
      filter( cat == st, mod %in% mods_state )
    
    cols <- plot_colors[grep(paste0("^", st, "_"), names(plot_colors))]
    names(cols) <- sub(paste0(st, "_"), "", names(cols))
    dat$mod <- factor(dat$mod, levels = names(cols))
    
    ## -----------------------------
    ## Observed data
    ## -----------------------------
    scatter_df <- data.frame(
      x   = data[[pred_name]],
      y   = data[[st]],
      mod = data[[moderator_name]]
    )
    scatter_df <- scatter_df %>%
      filter(mod %in% mods_state)
    scatter_df$mod <- factor( scatter_df$mod, levels = names(cols) )
    state_num <- as.numeric( sub("P_k13c", "", st) )
    cols <- cols[mods_state]
    
    ## -----------------------------
    ## Plot
    ## -----------------------------
    plots[[i]] <-
      ggplot() +
      # geom_point( data = scatter_df, aes( x = x, y = y, colour = mod ),
      #   alpha = 1, size = 0.05, show.legend = FALSE ) +
      
      geom_ribbon( data = dat, aes( x = x, ymin = lo, ymax = hi, fill = mod, group = mod ),
        alpha = 0.08, colour = NA, show.legend = FALSE ) +
      
      geom_line( data = dat, aes( x = x, y = est, colour = mod, group = mod ),
        linewidth = 1.2 ) +
      scale_colour_manual(values = cols) +
      scale_fill_manual(values = cols) + 
      coord_cartesian(
        # xlim = c(5, 25),
        ylim = c(0, 0.2)
      ) +
      labs(
        title = labels[state_num],
        x = out_name,
        y = "Fractional occupancy",
        colour = NULL
      ) +
      
      theme_classic(base_size = 12) +
      theme(
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.text = element_text(size = 10),
        legend.key.height = unit(0.2, "cm"),
        legend.key.width = unit(0.2, "cm"),
        plot.title = element_text(hjust = 0.5)
      )
  }
  p <- wrap_plots(plots, ncol = 5)
  
  # ------------------------------------------------------------
  # Save
  # ------------------------------------------------------------
  ggsave( filename = file.path( output_dir, paste0(out_file, ".png")),
    plot = p, width = 12, height = 4, dpi = 300)
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