# Script to visualize mediation analyses results. 
# Coefficients must be reported manually within each field 
# (paths a, b, ADE, ACME, and Total effect), as well as variables included
# (X predictor, M mediator, Y outcome). The script allows also to modify the
# color of each box.

# Berto A., 07/2026 aurber@utu.fi

library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

fig_dir <- "~/path/results/mediations/00_figures"
pollutant <- "no2"
metric <- "energy"
state <- "h6"
output <- "NIHtot"

g <- grViz("
digraph mediation {

graph[
    layout = dot
    rankdir = LR
    bgcolor = transparent
    splines = spline
    nodesep = 0.6
    ranksep = 0.4
]

node[
    shape = box
    style = 'rounded,filled'
    fontname = Helvetica
    fontsize = 18
    color = '#3C3C3C'
    penwidth = 1.4
    margin = '0.18,0.12'
]

edge[
    fontname = Helvetica
    fontsize = 15
    color = '#505050'
    arrowsize = 0.9
    penwidth = 1.8
]

X[
    label = 'Late-childhood NO2'
    fillcolor = '#DCEEFF'
]

M[
    label = 'Energy ψ6'
    fillcolor = '#CDEFFC'
]

Y[
    label = 'NIH Total Cognition'
    fillcolor = '#DFF4E4'
]

X -> M[
    color = '#2C7FB8'
    penwidth = 2.5
    label = 'a\nβ = -0.649\n95% CI [-1.257, -0.085]'
]

M -> Y[
    color = '#2C7FB8'
    penwidth = 2.5
    label = 'b\nβ = 0.010\n95% CI [0.003, 0.017]'
]

X -> Y[
    color = '#444444'
    penwidth = 2.3
    label = 'ADE\nβ = 0.171\n95% CI [0.061, 0.289]'
]

X -> Y[
    color = '#D95F02'
    fontcolor = '#D95F02'
    penwidth = 2.3
    constraint = false
    minlen = 2
    label = 'ACME\nβ = -0.006\n95% CI [-0.016, -0.0005]'
]

TE[
    shape = note
    style = 'filled'
    fillcolor = '#F7F7F7'
    color = '#9A9A9A'
    fontsize = 16
    label = 'Total effect\nβ = 0.164\n95% CI [0.054, 0.283]'
]

{rank = same; Y; TE}
Y -> TE[
    style = invis
]

}
")

# save as PNG
svg <- export_svg(g)
writeLines(svg, file.path(fig_dir, paste0(pollutant, "_", metric, "_", output, "_mediation.svg")))
rsvg_png(charToRaw(svg),
  file = file.path(fig_dir, paste0(pollutant, "_", metric, "_", output, "_mediation.png")),
  width = 9000,
  height = 3000
)

# display in RStudio viewer
g
