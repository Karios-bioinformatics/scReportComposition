# scReportComposition: Cell-type distribution by condition -----------------------
#
# plot_celltype_fraction_by_condition() — boxplot + jitter of fraction per
#   cell type, split by condition.  NULL when no condition.
# plot_celltype_count_by_condition()   — boxplot + jitter of n_cells per
#   cell type, split by condition.  NULL when no condition.


#' Cell-Type Fraction by Condition — Boxplot + Jitter
#'
#' For each cell type, shows the per-sample fraction distribution
#' split by condition.  Uses boxplot with jittered scatter overlay.
#'
#' @param comp_data A data.frame from \code{prepare_composition_data()}
#' @param cond_colors Named colour vector for conditions
#' @return A plotly htmlwidget, or \code{NULL}
#' @keywords internal
plot_celltype_fraction_by_condition <- function(comp_data, cond_colors) {

  if (!"condition" %in% names(comp_data)) return(NULL)

  celltypes  <- levels(comp_data$celltype)
  conditions <- levels(comp_data$condition)
  plot_data  <- comp_data[comp_data$total_cells > 0, ]

  plot_data$hover <- sprintf(
    "<b>%s</b><br>Sample: %s<br>Condition: %s<br>Fraction: %.1f%%",
    plot_data$celltype, plot_data$sample, plot_data$condition,
    plot_data$fraction * 100
  )

  condition_cols <- unname(cond_colors[as.character(conditions)])

  p <- plotly::plot_ly(
    data       = plot_data,
    x          = ~celltype,
    y          = ~fraction,
    color      = ~condition,
    colors     = condition_cols,
    type       = "box",
    hoverinfo  = "none",
    showlegend = TRUE
  )

  p <- plotly::add_trace(
    p,
    data        = plot_data,
    x           = ~celltype,
    y           = ~fraction,
    color       = ~condition,
    colors      = condition_cols,
    type        = "scatter",
    mode        = "markers",
    text        = ~hover,
    hoverinfo   = "text",
    hoverlabel  = list(bgcolor = "#2d3436", font = list(color = "#ffffff")),
    marker      = list(
      size    = 5,
      opacity = 0.55,
      line    = list(width = 0.5, color = "#ffffff")
    ),
    showlegend  = FALSE
  )

  p <- plotly::layout(p,
    title      = list(text = "Cell-Type Fraction by Condition",
                      font = list(size = 14)),
    xaxis      = list(title = "", categoryorder = "array",
                      categoryarray = celltypes),
    yaxis      = list(title = "Fraction", tickformat = ".0%", range = c(0, 1)),
    boxmode    = "group",
    margin     = list(l = 60, r = 30, b = 100, t = 40),
    legend     = list(title = list(text = "Condition")),
    hovermode  = "closest"
  )

  p <- plotly::config(p,
    displayModeBar = TRUE, displaylogo = FALSE,
    modeBarButtons = list(list("toImage", "zoom2d", "pan2d",
                               "resetScale2d", "hoverClosestCartesian")),
    toImageButtonOptions = list(
      format = "png", filename = "celltype_fraction_by_condition",
      height = 600, width = 1000
    )
  )
  p
}


#' Cell-Type Count by Condition — Boxplot + Jitter
#'
#' For each cell type, shows the per-sample cell count distribution
#' split by condition.
#'
#' @param comp_data A data.frame from \code{prepare_composition_data()}
#' @param cond_colors Named colour vector for conditions
#' @return A plotly htmlwidget, or \code{NULL}
#' @keywords internal
plot_celltype_count_by_condition <- function(comp_data, cond_colors) {

  if (!"condition" %in% names(comp_data)) return(NULL)

  celltypes  <- levels(comp_data$celltype)
  conditions <- levels(comp_data$condition)
  plot_data  <- comp_data[comp_data$total_cells > 0, ]

  plot_data$hover <- sprintf(
    "<b>%s</b><br>Sample: %s<br>Condition: %s<br>Cells: %s",
    plot_data$celltype, plot_data$sample, plot_data$condition,
    fmt_num(plot_data$n_cells)
  )

  condition_cols <- unname(cond_colors[as.character(conditions)])

  p <- plotly::plot_ly(
    data       = plot_data,
    x          = ~celltype,
    y          = ~n_cells,
    color      = ~condition,
    colors     = condition_cols,
    type       = "box",
    hoverinfo  = "none",
    showlegend = TRUE
  )

  p <- plotly::add_trace(
    p,
    data        = plot_data,
    x           = ~celltype,
    y           = ~n_cells,
    color       = ~condition,
    colors      = condition_cols,
    type        = "scatter",
    mode        = "markers",
    text        = ~hover,
    hoverinfo   = "text",
    hoverlabel  = list(bgcolor = "#2d3436", font = list(color = "#ffffff")),
    marker      = list(
      size    = 5,
      opacity = 0.55,
      line    = list(width = 0.5, color = "#ffffff")
    ),
    showlegend  = FALSE
  )

  p <- plotly::layout(p,
    title      = list(text = "Cell-Type Count by Condition",
                      font = list(size = 14)),
    xaxis      = list(title = "", categoryorder = "array",
                      categoryarray = celltypes),
    yaxis      = list(title = "Number of Cells"),
    boxmode    = "group",
    margin     = list(l = 80, r = 30, b = 100, t = 40),
    legend     = list(title = list(text = "Condition")),
    hovermode  = "closest"
  )

  p <- plotly::config(p,
    displayModeBar = TRUE, displaylogo = FALSE,
    modeBarButtons = list(list("toImage", "zoom2d", "pan2d",
                               "resetScale2d", "hoverClosestCartesian")),
    toImageButtonOptions = list(
      format = "png", filename = "celltype_count_by_condition",
      height = 600, width = 1000
    )
  )
  p
}
