# scReportComposition: Data helpers ----------------------------------------------
#
# Utility functions that transform the composition table into
# shapes needed by specific visualisations.


#' Summarise composition to condition-level aggregates
#'
#' Aggregates sample-level composition data to condition-level means
#' and totals.  Returns \code{NULL} when no condition column is present.
#'
#' @param comp_data A data.frame from \code{prepare_composition_data()}
#' @return A data.frame with columns \code{condition}, \code{celltype},
#'   \code{n_cells}, \code{total_cells}, \code{fraction}, or \code{NULL}
#' @keywords internal
summarise_condition_composition <- function(comp_data) {
  if (!"condition" %in% names(comp_data)) return(NULL)

  plot_data <- stats::aggregate(
    cbind(n_cells, total_cells) ~ condition + celltype,
    data = comp_data, FUN = sum
  )
  plot_data$fraction <- plot_data$n_cells / plot_data$total_cells
  plot_data$condition <- factor(
    plot_data$condition,
    levels = levels(comp_data$condition)
  )
  plot_data
}


#' Build a sample × celltype fraction matrix
#'
#' Pivots the composition table to a matrix with samples as rows
#' and cell types as columns, with fraction values.
#'
#' @param comp_data A data.frame from \code{prepare_composition_data()}
#' @return A numeric matrix (samples × celltypes), or \code{NULL} if
#'   no data
#' @keywords internal
build_composition_matrix <- function(comp_data) {
  # Aggregate across conditions if present
  if ("condition" %in% names(comp_data)) {
    plot_data <- stats::aggregate(
      cbind(n_cells, total_cells) ~ sample + celltype,
      data = comp_data, FUN = sum
    )
    plot_data$fraction <- plot_data$n_cells / plot_data$total_cells
  } else {
    plot_data <- comp_data
  }

  samples   <- levels(plot_data$sample)
  celltypes <- levels(plot_data$celltype)

  mat <- matrix(
    0, nrow = length(samples), ncol = length(celltypes),
    dimnames = list(samples, celltypes)
  )

  for (i in seq_len(nrow(plot_data))) {
    s <- as.character(plot_data$sample[i])
    c <- as.character(plot_data$celltype[i])
    mat[s, c] <- plot_data$fraction[i]
  }

  mat
}
