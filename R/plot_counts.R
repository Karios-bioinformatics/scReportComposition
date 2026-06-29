# scReportComposition: Count-based bar plots ------------------------------------
#
# plot_sample_count_composition()   — sample × celltype cell counts (stacked)
# plot_condition_count_composition() — condition × celltype cell counts (stacked)
#                                     NULL when no condition


#' Sample Composition — Count Stacked Bar
#'
#' Stacked bar plot showing absolute cell counts per sample × cell type.
#'
#' @param comp_data A data.frame from \code{prepare_composition_data()}
#' @param colors Named colour vector for cell types
#' @return A plotly htmlwidget
#' @keywords internal
plot_sample_count_composition <- function(comp_data, colors) {

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

  plot_data$hover <- sprintf(
    "<b>%s</b><br>Sample: %s<br>Cells: %s / %s<br>Fraction: %.1f%%",
    plot_data$celltype, plot_data$sample,
    fmt_num(plot_data$n_cells), fmt_num(plot_data$total_cells),
    plot_data$fraction * 100
  )

  p <- plotly::plot_ly(
    data       = plot_data,
    x          = ~sample,
    y          = ~n_cells,
    color      = ~celltype,
    colors     = celltype_cols,
    type       = "bar",
    text       = ~hover,
    hoverinfo  = "text",
    hoverlabel = list(bgcolor = "#2d3436", font = list(color = "#ffffff"))
  )

  p <- plotly::layout(p,
    title    = list(text = "Sample Composition — Cell Counts",
                    font = list(size = 14)),
    xaxis    = list(title = "", categoryorder = "array",
                    categoryarray = as.character(samples)),
    yaxis    = list(title = "Number of Cells"),
    barmode  = "stack",
    bargap   = 0.25,
    margin   = list(l = 80, r = 30, b = 80, t = 40),
    legend   = list(title = list(text = "Cell Type"), traceorder = "normal"),
    hovermode = "closest"
  )

  p <- plotly::config(p,
    displayModeBar = TRUE, displaylogo = FALSE,
    modeBarButtons = list(list("toImage", "zoom2d", "pan2d",
                               "resetScale2d", "hoverClosestCartesian")),
    toImageButtonOptions = list(
      format = "png", filename = "sample_count_composition",
      height = 600, width = 1000
    )
  )
  p
}


#' Condition Composition — Count Stacked Bar
#'
#' Stacked bar showing absolute cell counts per condition × cell type.
#' Returns \code{NULL} when no condition column is available.
#'
#' @param comp_data A data.frame from \code{prepare_composition_data()}
#' @param colors Named colour vector for cell types
#' @return A plotly htmlwidget, or \code{NULL}
#' @keywords internal
plot_condition_count_composition <- function(comp_data, colors) {

  if (!"condition" %in% names(comp_data)) return(NULL)

  plot_data <- summarise_condition_composition(comp_data)
  if (is.null(plot_data)) return(NULL)

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
    data       = plot_data,
    x          = ~condition,
    y          = ~n_cells,
    color      = ~celltype,
    colors     = celltype_cols,
    type       = "bar",
    text       = ~hover,
    hoverinfo  = "text",
    hoverlabel = list(bgcolor = "#2d3436", font = list(color = "#ffffff"))
  )

  p <- plotly::layout(p,
    title    = list(text = "Condition Composition — Cell Counts",
                    font = list(size = 14)),
    xaxis    = list(title = "", categoryorder = "array",
                    categoryarray = as.character(conditions)),
    yaxis    = list(title = "Number of Cells"),
    barmode  = "stack",
    bargap   = 0.25,
    margin   = list(l = 80, r = 30, b = 80, t = 40),
    legend   = list(title = list(text = "Cell Type"), traceorder = "normal"),
    hovermode = "closest"
  )

  p <- plotly::config(p,
    displayModeBar = TRUE, displaylogo = FALSE,
    modeBarButtons = list(list("toImage", "zoom2d", "pan2d",
                               "resetScale2d", "hoverClosestCartesian")),
    toImageButtonOptions = list(
      format = "png", filename = "condition_count_composition",
      height = 600, width = 1000
    )
  )
  p
}
