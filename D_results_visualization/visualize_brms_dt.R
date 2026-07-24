## Visualize reliable associations between DT and pollution
# Berto Aurora, 05/2026 aurber@utu.fi

# This script saves, for each model, the respective regression table, and 
# visualizes all reliable associations (95% CI excluding zero) between LEiDA
# DT and pollutant's exposure, with regression lines color coded according 
# to the respective RSN network.

# ------------------------------------------------------------------------------
# Load required packages
# ------------------------------------------------------------------------------
library(brms)
library(dplyr)
library(ggplot2)
library(officer)
library(patchwork)

# ------------------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------------------
output_dir <- "~/path/to/dwell_times/00_figures"
tables_dir <- "~/path/to/dwell_times/00_summary_tables"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Models
# ------------------------------------------------------------------------------
no2_pre <- "~/path/to/results/model_comparison/dwell_times/models/dt_no2_fit1_only_prenatal.RDS"
no2_cur <- "~/path/to/results/model_comparison/dwell_times/models/dt_no2_fit2_only_current.RDS"

pm25_pre <- "~/path/to/results/model_comparison/dwell_times/models/dt_pm_fit1_only_prenatal.RDS"
pm25_cur <- "~/path/to/results/model_comparison/dwell_times/models/dt_pm_fit2_only_current.RDS"

o3_pre <- "~/path/to/results/model_comparison/dwell_times/models/dt_o3_fit1_only_prenatal.RDS"
o3_cur <- "~/path/to/results/model_comparison/dwell_times/models/dt_o3_fit2_only_current.RDS"

# ------------------------------------------------------------------------------
# Colors
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

analyze_model <- function(model_path,
                          out_name,
                          out_file,
                          output_dir,
                          tables_dir,
                          pred_name,
                          colors,
                          labels){
  
  #------------------------------------------------------------------
  # Load model
  #------------------------------------------------------------------
  model <- readRDS(model_path)
  
  #------------------------------------------------------------------
  # Save coefficient table
  #------------------------------------------------------------------
  coef_df <- as.data.frame(fixef(model))
  coef_df$Parameter <- rownames(coef_df)
  coef_df <- coef_df %>%
    select(Parameter, Estimate, Est.Error, Q2.5, Q97.5)
  
  doc <- read_docx()
  doc <- body_add_par(doc, value = out_name, style = "heading 1")
  doc <- body_add_table(doc, value = coef_df, style = "table_template")
  
  print(doc, target = file.path(tables_dir, paste0(out_file,"_summary.docx")))
  
  #------------------------------------------------------------------
  # Find significant coefficients
  #------------------------------------------------------------------
  sig_df <- coef_df %>%
    filter(
      grepl("^LTk13c", Parameter),
      grepl(paste0("_", pred_name, "$"), Parameter),
      (Q2.5 > 0 & Q97.5 > 0) | (Q2.5 < 0 & Q97.5 < 0))
  
  if(nrow(sig_df) == 0){
    message("No significant effects for ", pred_name)
    return(NULL)
  }
  plots <- list()
  
  #------------------------------------------------------------------
  # Loop over significant coefficients
  #------------------------------------------------------------------
  for(i in seq_len(nrow(sig_df))){
    term <- sig_df$Parameter[i]
    
    ## outcome
    outcome <- sub("^(LTk13c[0-9]+)_.*", "\\1", sig_df$Parameter[i])
    state_num <- as.numeric(sub("LTk13c", "", outcome))
    state_label <- labels[state_num]
    state_color <- colors[state_num]
    
    ## predictor
    predictor <- sub("^LTk13c[0-9]+_","",term)
    message(outcome,"  ",predictor)
    
    ## conditional effect
    ce <- conditional_effects(
      model,
      effects = predictor,
      resp = outcome
    )[[1]]
    
    xvar <- predictor
    
    plot_df <- data.frame(
      x = ce[[xvar]],
      estimate = ce$estimate__,
      lower = ce$lower__,
      upper = ce$upper__
    )
    
    state_num <- as.numeric(sub("LTk13c","",outcome))
    state_col <- colors[state_num]
    state_lab <- labels[outcome]
    
    p <- ggplot(plot_df,
                aes(x = x,
                    y = estimate)) +
      geom_ribbon(aes(ymin = lower,
                      ymax = upper),
                  fill = state_color,
                  alpha = .08) +
      geom_line(colour = state_color,
                linewidth = 1.2) +
      labs(
        title = state_label,
        x = out_name,
        y = "Dwell time"
      ) +
      theme_classic(base_size = 13)
    plots[[length(plots)+1]] <- p
    
  }
  
  #------------------------------------------------------------------
  # Save figure
  #------------------------------------------------------------------
  
  fig <- wrap_plots(plots, ncol = 2)
  ggsave(file.path(output_dir, paste0(out_file,".png")),
    fig, width = 5, height = 3, dpi = 300)
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