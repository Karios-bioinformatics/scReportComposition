# scReportComposition: build_summary --------------------------------------------
#
# Computes summary statistics from the composition table.
# Returns a named list consumed by the HTML assembler to
# render the Summary Cards at the top of the report.


#' Build Summary Statistics from Composition Data
#'
#' Computes key summary metrics from the composition table for
#' display in the report's Summary Card section.
#'
#' @param comp_data A data.frame returned by \code{prepare_composition_data()}
#' @param condition_col Optional condition column name (for labelling)
#' @return A named list with elements:
#'   \itemize{
#'     \item \code{total_cells} — overall cell count
#'     \item \code{n_samples} — number of unique samples
#'     \item \code{n_celltypes} — number of unique cell types
#'     \item \code{n_conditions} — number of unique conditions (NA if none)
#'     \item \code{cells_per_sample} — named numeric vector
#'     \item \code{celltypes_detected} — character vector of cell type names
#'   }
#' @keywords internal
build_summary <- function(comp_data, condition_col = NULL) {

  total_cells  <- sum(comp_data$n_cells)
  samples      <- levels(comp_data$sample)
  celltypes    <- levels(comp_data$celltype)

  # cells_per_sample: aggregate across cell types
  cps <- stats::aggregate(
    n_cells ~ sample, data = comp_data, FUN = sum
  )
  cells_per_sample <- setNames(cps$n_cells, as.character(cps$sample))

  has_condition <- !is.null(condition_col) &&
                   condition_col %in% names(comp_data)

  n_conditions <- if (has_condition) {
    length(levels(comp_data$condition))
  } else {
    NA_integer_
  }

  list(
    total_cells       = total_cells,
    n_samples         = length(samples),
    n_celltypes       = length(celltypes),
    n_conditions      = n_conditions,
    cells_per_sample  = cells_per_sample,
    celltypes_detected = as.character(celltypes),
    has_condition     = has_condition,
    samples           = as.character(samples)
  )
}
