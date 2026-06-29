# scReportComposition: Composition Plots ----------------------------------------
#
# Three plot functions, each returning a plotly htmlwidget:
#   plot_sample_composition()    — stacked bar by sample
#   plot_condition_composition() — stacked bar by condition (NULL if no condition)
#   plot_celltype_fraction()     — boxplot + jitter per cell type


#' Sample Composition — Stacked Bar Plot
#'
#' Stacked bar plot showing the cell-type composition of every sample.
#' X-axis = sample, Y-axis = fraction, colour = cell type.
#' When a condition column exists, data is aggregated across conditions
#' so each sample appears once.
#'
#' @param comp_data A data.frame from \code{prepare_composition_data()}
#' @param colors Named colour vector for cell types
#' @return A plotly htmlwidget
#' @keywords internal
plot_sample_composition <- function(comp_data, colors) {

  # Aggregate: if condition exists, sum across conditions per sample×celltype
  if ("condition" %in% names(comp_data)) {
    plot_data <- stats::aggregate(
      cbind(n_cells, total_cells) ~ sample + celltype,
      data = comp_data, FUN = sum
    )
    plot_data$fraction <- plot_data$n_cells / plot_data$total_cells
  } else {
    plot_data <- comp_data
  }

  samples       <- levels(plot_data$sample)
  celltypes     <- levels(plot_data$celltype)
  celltype_cols <- unname(colors[celltypes])

  # Build hover text
  plot_data$hover <- sprintf(
    "<b>%s</b><br>Sample: %s<br>Cells: %s / %s<br>Fraction: %.1f%%",
    plot_data$celltype, plot_data$sample,
    fmt_num(plot_data$n_cells), fmt_num(plot_data$total_cells),
    plot_data$fraction * 100
  )

  p <- plotly::plot_ly(
    data        = plot_data,
    x           = ~sample,
    y           = ~fraction,
    color       = ~celltype,
    colors      = celltype_cols,
    type        = "bar",
    text        = ~hover,
    hoverinfo   = "text",
    hoverlabel  = list(bgcolor = "#2d3436", font = list(color = "#ffffff"))
  )

  p <- plotly::layout(p,
    title    = list(text = "Sample Composition", font = list(size = 14)),
    xaxis    = list(title = "", categoryorder = "array",
                    categoryarray = as.character(samples)),
    yaxis    = list(title = "Fraction", tickformat = ".0%", range = c(0, 1)),
    barmode  = "stack",
    bargap   = 0.25,
    margin   = list(l = 60, r = 30, b = 80, t = 40),
    legend   = list(title = list(text = "Cell Type"), traceorder = "normal"),
    hovermode = "closest"
  )

  p <- plotly::config(p,
    displayModeBar = TRUE,
    displaylogo    = FALSE,
    modeBarButtons = list(list("toImage", "zoom2d", "pan2d", "resetScale2d",
                               "hoverClosestCartesian")),
    toImageButtonOptions = list(
      format = "png", filename = "sample_composition",
      height = 600, width = 1000
    )
  )

  p
}


#' Condition Composition — Stacked Bar Plot
#'
#' Stacked bar plot showing mean cell-type composition per condition.
#' When no condition column is available, returns \code{NULL} silently
#' so the HTML assembler can hide the section.
#'
#' @param comp_data A data.frame from \code{prepare_composition_data()}
#' @param colors Named colour vector for cell types
#' @return A plotly htmlwidget, or \code{NULL}
#' @keywords internal
plot_condition_composition <- function(comp_data, colors) {

  if (!"condition" %in% names(comp_data)) {
    return(NULL)
  }

  # Aggregate across samples within each condition×celltype
  plot_data <- stats::aggregate(
    cbind(n_cells, total_cells) ~ condition + celltype,
    data = comp_data, FUN = sum
  )
  plot_data$fraction <- plot_data$n_cells / plot_data$total_cells

  conditions    <- levels(plot_data$condition)
  celltypes     <- levels(plot_data$celltype)
  celltype_cols <- unname(colors[celltypes])

  plot_data$hover <- sprintf(
    "<b>%s</b><br>Condition: %s<br>Cells: %s / %s<br>Fraction: %.1f%%",
    plot_data$celltype, plot_data$condition,
    fmt_num(plot_data$n_cells), fmt_num(plot_data$total_cells),
    plot_data$fraction * 100
  )

  p <- plotly::plot_ly(
    data        = plot_data,
    x           = ~condition,
    y           = ~fraction,
    color       = ~celltype,
    colors      = celltype_cols,
    type        = "bar",
    text        = ~hover,
    hoverinfo   = "text",
    hoverlabel  = list(bgcolor = "#2d3436", font = list(color = "#ffffff"))
  )

  p <- plotly::layout(p,
    title    = list(text = "Condition Composition", font = list(size = 14)),
    xaxis    = list(title = "", categoryorder = "array",
                    categoryarray = as.character(conditions)),
    yaxis    = list(title = "Mean Fraction", tickformat = ".0%", range = c(0, 1)),
    barmode  = "stack",
    bargap   = 0.25,
    margin   = list(l = 60, r = 30, b = 80, t = 40),
    legend   = list(title = list(text = "Cell Type"), traceorder = "normal"),
    hovermode = "closest"
  )

  p <- plotly::config(p,
    displayModeBar = TRUE,
    displaylogo    = FALSE,
    modeBarButtons = list(list("toImage", "zoom2d", "pan2d", "resetScale2d",
                               "hoverClosestCartesian")),
    toImageButtonOptions = list(
      format = "png", filename = "condition_composition",
      height = 600, width = 1000
    )
  )

  p
}


#' Cell-Type Fraction — Boxplot + Jitter
#'
#' For each cell type, displays the distribution of fractions across
#' samples (or sample×condition pairs).  Uses boxplot overlaid with
#' jittered scatter points.
#'
#' When a condition column is present, points are coloured by condition;
#' otherwise they are coloured by cell type.
#'
#' @param comp_data A data.frame from \code{prepare_composition_data()}
#' @param colors Named colour vector for groups
#' @return A plotly htmlwidget
#' @keywords internal
plot_celltype_fraction <- function(comp_data, colors) {

  celltypes     <- levels(comp_data$celltype)
  has_condition <- "condition" %in% names(comp_data)

  # Determine colour mapping
  if (has_condition) {
    conditions       <- levels(comp_data$condition)
    cond_colors      <- celltype_color_map(as.character(conditions))
    color_col        <- "condition"
    color_map        <- unname(cond_colors[as.character(conditions)])
    legend_title     <- "Condition"
    hover_template   <- "<b>%s</b><br>Sample: %s<br>Condition: %s<br>Fraction: %.1f%%"
  } else {
    color_col        <- "celltype"
    color_map        <- unname(colors[celltypes])
    legend_title     <- "Cell Type"
    hover_template   <- "<b>%s</b><br>Sample: %s<br>Fraction: %.1f%%"
  }

  # Build hover text
  if (has_condition) {
    comp_data$hover <- sprintf(
      hover_template,
      comp_data$celltype, comp_data$sample, comp_data$condition,
      comp_data$fraction * 100
    )
  } else {
    comp_data$hover <- sprintf(
      hover_template,
      comp_data$celltype, comp_data$sample,
      comp_data$fraction * 100
    )
  }

  # Filter out rows with zero total_cells (artifact of missing combos)
  plot_data <- comp_data[comp_data$total_cells > 0, ]

  # ---- Boxplot trace ----
  p <- plotly::plot_ly(
    data        = plot_data,
    x           = ~celltype,
    y           = ~fraction,
    color       = as.formula(paste0("~", color_col)),
    colors      = color_map,
    type        = "box",
    hoverinfo   = "none",
    showlegend  = TRUE
  )

  # ---- Jitter scatter overlay ----
  p <- plotly::add_trace(
    p,
    data        = plot_data,
    x           = ~celltype,
    y           = ~fraction,
    color       = as.formula(paste0("~", color_col)),
    colors      = color_map,
    type        = "scatter",
    mode        = "markers",
    text        = ~hover,
    hoverinfo   = "text",
    hoverlabel  = list(bgcolor = "#2d3436", font = list(color = "#ffffff")),
    marker      = list(
      size    = 6,
      opacity = 0.55,
      line    = list(width = 0.5, color = "#ffffff")
    ),
    showlegend  = FALSE
  )

  p <- plotly::layout(p,
    title      = list(text = "Cell-Type Fraction Distribution",
                      font = list(size = 14)),
    xaxis      = list(title = "", categoryorder = "array",
                      categoryarray = celltypes),
    yaxis      = list(title = "Fraction", tickformat = ".0%", range = c(0, 1),
                      zeroline = TRUE),
    boxmode    = "group",
    margin     = list(l = 60, r = 30, b = 100, t = 40),
    legend     = list(title = list(text = legend_title)),
    hovermode  = "closest"
  )

  p <- plotly::config(p,
    displayModeBar = TRUE,
    displaylogo    = FALSE,
    modeBarButtons = list(list("toImage", "zoom2d", "pan2d", "resetScale2d",
                               "hoverClosestCartesian")),
    toImageButtonOptions = list(
      format = "png", filename = "celltype_fraction",
      height = 600, width = 1000
    )
  )

  p
}
