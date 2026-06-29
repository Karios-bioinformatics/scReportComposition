# scReportComposition: Heatmaps ------------------------------------------------
#
# plot_sample_celltype_heatmap()    — sample × celltype fraction heatmap
# plot_condition_celltype_heatmap() — condition × celltype fraction heatmap
#                                     NULL when no condition


#' Sample × Cell-Type Fraction Heatmap
#'
#' Plotly heatmap with samples as rows, cell types as columns,
#' coloured by fraction.
#'
#' @param comp_data A data.frame from \code{prepare_composition_data()}
#' @return A plotly htmlwidget
#' @keywords internal
plot_sample_celltype_heatmap <- function(comp_data) {

  mat <- build_composition_matrix(comp_data)
  if (is.null(mat) || nrow(mat) == 0 || ncol(mat) == 0) return(NULL)

  samples   <- rownames(mat)
  celltypes <- colnames(mat)

  p <- plotly::plot_ly(
    x          = celltypes,
    y          = samples,
    z          = mat,
    type       = "heatmap",
    colorscale = list(c(0, "#f8f9fc"), c(1, "#00b894")),
    hovertemplate = paste0(
      "Sample: %{y}<br>",
      "Cell Type: %{x}<br>",
      "Fraction: %{z:.1%}<br>",
      "<extra></extra>"
    ),
    colorbar   = list(
      title     = "Fraction",
      tickformat = ".0%"
    )
  )

  p <- plotly::layout(p,
    title  = list(text = "Sample × Cell-Type Composition",
                  font = list(size = 14)),
    xaxis  = list(title = "Cell Type", side = "bottom",
                  tickangle = -45),
    yaxis  = list(title = "Sample", autorange = "reversed"),
    margin = list(l = 120, r = 60, b = 120, t = 40)
  )

  p <- plotly::config(p,
    displayModeBar = TRUE, displaylogo = FALSE,
    modeBarButtons = list(list("toImage", "zoom2d", "pan2d",
                               "resetScale2d", "hoverClosestCartesian")),
    toImageButtonOptions = list(
      format = "png", filename = "sample_celltype_heatmap",
      height = 600, width = 1000
    )
  )
  p
}


#' Condition × Cell-Type Fraction Heatmap
#'
#' Plotly heatmap with conditions as rows, cell types as columns,
#' coloured by mean fraction.  Returns \code{NULL} when no condition
#' column is available.
#'
#' @param comp_data A data.frame from \code{prepare_composition_data()}
#' @return A plotly htmlwidget, or \code{NULL}
#' @keywords internal
plot_condition_celltype_heatmap <- function(comp_data) {

  if (!"condition" %in% names(comp_data)) return(NULL)

  cond_summary <- summarise_condition_composition(comp_data)
  if (is.null(cond_summary)) return(NULL)

  conditions <- levels(cond_summary$condition)
  celltypes  <- levels(cond_summary$celltype)

  mat <- matrix(
    0, nrow = length(conditions), ncol = length(celltypes),
    dimnames = list(conditions, celltypes)
  )

  for (i in seq_len(nrow(cond_summary))) {
    c <- as.character(cond_summary$condition[i])
    t <- as.character(cond_summary$celltype[i])
    mat[c, t] <- cond_summary$fraction[i]
  }

  p <- plotly::plot_ly(
    x          = celltypes,
    y          = conditions,
    z          = mat,
    type       = "heatmap",
    colorscale = list(c(0, "#f8f9fc"), c(1, "#00b894")),
    hovertemplate = paste0(
      "Condition: %{y}<br>",
      "Cell Type: %{x}<br>",
      "Fraction: %{z:.1%}<br>",
      "<extra></extra>"
    ),
    colorbar   = list(
      title     = "Fraction",
      tickformat = ".0%"
    )
  )

  p <- plotly::layout(p,
    title  = list(text = "Condition × Cell-Type Composition",
                  font = list(size = 14)),
    xaxis  = list(title = "Cell Type", side = "bottom",
                  tickangle = -45),
    yaxis  = list(title = "Condition", autorange = "reversed"),
    margin = list(l = 120, r = 60, b = 120, t = 40)
  )

  p <- plotly::config(p,
    displayModeBar = TRUE, displaylogo = FALSE,
    modeBarButtons = list(list("toImage", "zoom2d", "pan2d",
                               "resetScale2d", "hoverClosestCartesian")),
    toImageButtonOptions = list(
      format = "png", filename = "condition_celltype_heatmap",
      height = 600, width = 1000
    )
  )
  p
}
