## Visualize mediation models - Occupancies
## Aurora Berto (aurber@utu.fi) - 09/2026

# Packages
library(brms)
library(dplyr)
library(ggplot2)
library(officer)
library(patchwork)
library(readxl)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# PATHS
table_dir <- "path/to/results/mediations/fractional_occupancy/mediation_standardized_effects_pd_correction.xlsx"
output_dir <- "path/to/results/mediations/fractional_occupancy/figures"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# COLORS AND LABELS
mediators <- list(
  P_k13c1 = c("DMN (1)\\nOccupancy","#C0392B"),
  P_k13c2 = c("VIS (2)\\nOccupancy","#7D3C98"),
  P_k13c3 = c("VIS (3)\\nOccupancy","#7D3C98"),
  P_k13c4 = c("DMN (4)\\nOccupancy","#C0392B"),
  P_k13c5 = c("VIS (5)\\nOccupancy","#7D3C98"),
  P_k13c6 = c("VIS (6)\\nOccupancy","#7D3C98"),
  P_k13c7 = c("VIS (7)\\nOccupancy","#7D3C98"),
  P_k13c8 = c("DAN (8)\\nOccupancy","#1E8449"),
  P_k13c9 = c("DMN (9)\\nOccupancy","#C0392B"),
  P_k13c10 = c("VIS (10)\\nOccupancy","#7D3C98"),
  P_k13c11 = c("FPN (11)\\nOccupancy","#B9770E"),
  P_k13c12 = c("VAN (12)\\nOccupancy","#CDA2D6"),
  P_k13c13 = c("DAN (13)\\nOccupancy","#1E8449"))

outcomes <- list(
  cbcl_scr_syn_internal_t = c("CBCL\\nInternalizing factors", "#E8A9B8"),
  cbcl_scr_syn_external_t = c("CBCL\\nExternalizing factors", "#D88FA3"),
  nihtbx_cryst_agecorrected = c("NIH Toolbox\\nCrystallized\\ncomponents", "#A8D5BA"),
  nihtbx_fluidcomp_agecorrected = c("NIH Toolbox\\nFluid components", "#8FC8B0"),
  nihtbx_totalcomp_agecorrected = c("NIH Toolbox\\nTotal components", "#72B7A0") )

exposures <- list(
  reshist_addr1_pm25_prenatal_avg = c("Prenatal\\nPM2.5", "#B8CBE8"),
  reshist_addr1_pm252016aa = c("Late-childhood\\nPM2.5", "#91B4DD"),
  reshist_addr1_o3_prenatal_avg = c("Prenatal\\nO3", "#C5B5E5"),
  reshist_addr1_o3_2016_annavg = c("Late-childhood\\nO3", "#A993D2"),
  reshist_addr1_no2_prenatal_avg = c("Prenatal\\nNO2", "#B5D3E7"),
  reshist_addr1_no2_2016_aavg = c("Late-childhood\\nNO2", "#8EBBD6") )

# READ RESUPS TABLE
table <- read_excel(table_dir)
table_to_visualize <- table %>%
  filter(pd > 0.975)

# Helper: format an effect estimate with its 95% CI
fmt_effect_sr <- function(est, lo, hi, digits = 3) {
  sprintf(paste0("%.", digits, "f [%.", digits, "f, %.", digits, "f]"), est, lo, hi)
}
fmt_effect_nn <- function(est, lo, hi, digits = 3) {
  sprintf(paste0("%.", digits, "f\\n[%.", digits, "f, %.", digits, "f]"), est, lo, hi)
}

# LOOP OVER RELIABLE MEDIATION MODELS
for (i in seq_len(nrow(table_to_visualize))) {
  
  row <- table_to_visualize[i, ]
  
  exposure_info <- exposures[[row$exposure]]
  mediator_info <- mediators[[row$mediator]]
  outcome_info  <- outcomes[[row$outcome]]
  
  if (is.null(exposure_info) || is.null(mediator_info) || is.null(outcome_info)) {
    warning(sprintf("Skipping row %d: missing label/color for '%s' / '%s' / '%s'",
                    i, row$exposure, row$mediator, row$outcome))
    next
  }
  
  add_alpha <- function(hex, alpha = "80") {
    paste0(hex, alpha)
  }
  exposure_color <- add_alpha(exposure_info[2], "80")
  mediator_color  <- add_alpha(mediator_info[2], "80")
  outcome_color   <- add_alpha(outcome_info[2], "80")
  
  a_lab     <- fmt_effect_nn(row$a_estimate, row$a_lower, row$a_upper)
  b_lab     <- fmt_effect_nn(row$b_estimate, row$b_lower, row$b_upper)
  ade_lab   <- fmt_effect_nn(row$c_estimate, row$c_lower, row$c_upper)
  acme_lab  <- fmt_effect_sr(row$indirect_estimate, row$indirect_lower, row$indirect_upper)
  total_lab <- fmt_effect_sr(row$total_estimate, row$total_lower, row$total_upper)
  
  star <- if (!is.na(row$pd.adj) && row$pd.adj > 0.975) "*" else ""
  
  dot <- sprintf('
digraph mediation {

  graph [layout = neato, overlap = false, fontsize = 10, fontname = Helvetica,
         label = "ACME = %s%s\\nTotal effect = %s", labelloc = b]
  node [shape = box, style = "filled,rounded", fontname = Helvetica, fontsize = 10,
        width = 1.5, height = 0.4, fixedsize = false]
  edge [fontname = Helvetica, fontsize = 9, color = "#555555", fontcolor = "#333333"]

  X [label = "%s", fillcolor = "%s", pos = "0,0!"]
  M [label = "%s", fillcolor = "%s", pos = "1.6,1.4!"]
  Y [label = "%s", fillcolor = "%s", pos = "3.2,0!"]

  X -> M 
  M -> Y 
  X -> Y 
  A_label [shape = plaintext, style = none, label = "a = %s", pos = "0.1,0.85!", fontname = Helvetica, fontsize = 9]
  B_label [shape = plaintext, style = none, label = "b = %s", pos = "3.1,0.85!", fontname = Helvetica, fontsize = 9]
  ADE_label [shape = plaintext, style = none, label = "ADE = %s", pos = "1.55,0.4!", fontname = Helvetica, fontsize = 9]
}
', acme_lab, star, total_lab, exposure_info[1], exposure_color, mediator_info[1], mediator_color, outcome_info[1], outcome_color,
a_lab, b_lab, ade_lab)
  
  g <- grViz(dot)
  
  out_file <- file.path(output_dir, paste0(row$model, ".png"))
  g %>%
    export_svg() %>%
    charToRaw() %>%
    rsvg_png(out_file, width = 1600)
}
