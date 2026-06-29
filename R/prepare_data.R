# scReportComposition: prepare_composition_data ----------------------------------
#
# Core data structure: builds a standardised composition table from a Seurat
# object.  All downstream visualisations depend on this table.
#
# Output columns: sample, celltype, n_cells, total_cells, fraction
#   plus condition (when condition_col is provided)


#' Prepare Composition Data from a Seurat Object
#'
#' Builds a standardised composition table from cell-level metadata.
#' This is the foundational data structure that all downstream
#' visualisations depend on.
#'
#' The function:
#' \itemize{
#'   \item Groups cells by sample, cell type, and (optionally) condition
#'   \item Counts cells per group
#'   \item Computes total cells per sample (× condition when applicable)
#'   \item Calculates fraction = n_cells / total_cells
#'   \item Fills missing combinations with 0
#'   \item Returns a tidy data.frame with natural sort ordering
#' }
#'
#' @param seurat_obj A Seurat object
#' @param sample_col   Name of the sample column in \code{seurat_obj@meta.data}
#' @param celltype_col Name of the cell-type column in metadata
#' @param condition_col Optional name of the condition column.  When \code{NULL}
#'   (the default), no condition grouping is applied.
#' @return A data.frame with columns \code{sample}, \code{celltype},
#'   \code{n_cells}, \code{total_cells}, \code{fraction}, and
#'   \code{condition} (when \code{condition_col} is provided).
#'   All character/factor columns use natural sort ordering.
#' @export
#'
#' @examples
#' \dontrun{
#' comp <- prepare_composition_data(
#'   seurat_obj,
#'   sample_col   = "orig.ident",
#'   celltype_col = "cell_type",
#'   condition_col = "condition"
#' )
#' head(comp)
#' }
prepare_composition_data <- function(seurat_obj,
                                      sample_col,
                                      celltype_col,
                                      condition_col = NULL) {

  # ---- Extract metadata ----
  meta <- seurat_obj@meta.data

  cols_needed <- c(sample_col, celltype_col)
  if (!is.null(condition_col)) {
    cols_needed <- c(cols_needed, condition_col)
  }

  missing <- setdiff(cols_needed, colnames(meta))
  if (length(missing) > 0) {
    stop(
      "seurat_obj@meta.data is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  # ---- Build grouping key ----
  meta$..sample   <- as.character(meta[[sample_col]])
  meta$..celltype <- as.character(meta[[celltype_col]])

  if (!is.null(condition_col)) {
    meta$..condition <- as.character(meta[[condition_col]])
  }

  # ---- Count cells per group ----
  if (is.null(condition_col)) {
    # sample × celltype
    counts <- as.data.frame(
      table(meta$..sample, meta$..celltype),
      stringsAsFactors = FALSE
    )
    names(counts) <- c("sample", "celltype", "n_cells")
  } else {
    # sample × condition × celltype
    counts <- as.data.frame(
      table(meta$..sample, meta$..condition, meta$..celltype),
      stringsAsFactors = FALSE
    )
    names(counts) <- c("sample", "condition", "celltype", "n_cells")
  }

  # ---- Compute total_cells ----
  if (is.null(condition_col)) {
    totals <- stats::aggregate(n_cells ~ sample, data = counts, FUN = sum)
    names(totals)[2] <- "total_cells"
    counts <- merge(counts, totals, by = "sample", all.x = TRUE)
  } else {
    totals <- stats::aggregate(
      n_cells ~ sample + condition, data = counts, FUN = sum
    )
    names(totals)[3] <- "total_cells"
    counts <- merge(counts, totals, by = c("sample", "condition"), all.x = TRUE)
  }

  # ---- Fill missing combinations with 0 ----
  all_samples   <- natural_sort(unique(meta$..sample))
  all_celltypes <- natural_sort(unique(meta$..celltype))

  if (is.null(condition_col)) {
    grid <- expand.grid(
      sample   = all_samples,
      celltype = all_celltypes,
      stringsAsFactors = FALSE
    )
    counts <- merge(grid, counts, by = c("sample", "celltype"), all.x = TRUE)
    counts$n_cells[is.na(counts$n_cells)] <- 0

    # Fill total_cells from known totals
    for (s in unique(counts$sample)) {
      mask <- counts$sample == s & is.na(counts$total_cells)
      if (any(mask)) {
        counts$total_cells[mask] <- totals$total_cells[totals$sample == s]
      }
    }
  } else {
    all_conditions <- natural_sort(unique(meta$..condition))
    grid <- expand.grid(
      sample    = all_samples,
      condition = all_conditions,
      celltype  = all_celltypes,
      stringsAsFactors = FALSE
    )
    counts <- merge(
      grid, counts,
      by = c("sample", "condition", "celltype"),
      all.x = TRUE
    )
    counts$n_cells[is.na(counts$n_cells)] <- 0

    for (i in seq_len(nrow(totals))) {
      mask <- counts$sample == totals$sample[i] &
              counts$condition == totals$condition[i] &
              is.na(counts$total_cells)
      if (any(mask)) {
        counts$total_cells[mask] <- totals$total_cells[i]
      }
    }
  }

  # ---- fraction ----
  counts$fraction <- ifelse(
    counts$total_cells > 0,
    counts$n_cells / counts$total_cells,
    0
  )

  # ---- Natural sort factor ordering ----
  counts$sample   <- factor(counts$sample,   levels = all_samples)
  counts$celltype <- factor(counts$celltype, levels = all_celltypes)
  if (!is.null(condition_col)) {
    counts$condition <- factor(counts$condition, levels = all_conditions)
  }

  # ---- Clean up internal columns from meta ----
  meta$..sample    <- NULL
  meta$..celltype  <- NULL
  if (!is.null(condition_col)) meta$..condition <- NULL

  # ---- Restore column types ----
  counts$n_cells      <- as.integer(counts$n_cells)
  counts$total_cells  <- as.integer(counts$total_cells)

  message(
    "Composition table built: ", nrow(counts), " rows (",
    length(all_samples), " samples × ",
    length(all_celltypes), " cell types",
    if (!is.null(condition_col))
      paste0(" × ", length(all_conditions), " conditions"),
    ")"
  )

  return(counts)
}
