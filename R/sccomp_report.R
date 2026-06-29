# scReportComposition: Main API -------------------------------------------------
#
# sccomp_report() is the single entry point.
# It orchestrates: prepare → summarise → plot → assemble → write.
# Accepts either a Seurat object or a plain data.frame of metadata.


#' Generate a Cell Composition HTML Report
#'
#' Takes a Seurat object (or a \code{data.frame} of cell-level metadata)
#' and user-specified column names, builds a composition table, creates
#' interactive Plotly visualisations, and writes a self-contained HTML
#' report file.
#'
#' The report includes:
#' \itemize{
#'   \item Summary Cards: total cells, samples, cell types, conditions
#'   \item Sample Composition: stacked bar plot (every sample)
#'   \item Condition Composition: stacked bar plot (if \code{condition_col}
#'         is provided; hidden otherwise)
#'   \item Cell-Type Fraction: boxplot + jitter per cell type
#' }
#'
#' @param seurat_obj A Seurat object.  Ignored when \code{meta_data} is provided.
#' @param meta_data A \code{data.frame} of cell-level metadata.  When provided,
#'   it takes precedence over \code{seurat_obj}.
#' @param sample_col   Name of the sample column
#'   (e.g. \code{"orig.ident"})
#' @param celltype_col Name of the cell-type column
#'   (e.g. \code{"cell_type"})
#' @param condition_col Optional name of the condition column
#'   (e.g. \code{"condition"}).  When \code{NULL}, the Condition
#'   Composition section is omitted.
#' @param output Path to the output HTML file.
#'   Default: \code{"scReportComposition.html"} in the current directory.
#' @param title Report title shown in the header.
#'   Default: \code{"scReportComposition"}.
#'
#' @return Invisibly, the path to the generated HTML file.
#' @export
#'
#' @examples
#' \dontrun{
#' library(scReportComposition)
#' library(Seurat)
#'
#' # From a Seurat object
#' obj <- readRDS("my_seurat.rds")
#' sccomp_report(
#'   obj,
#'   sample_col   = "orig.ident",
#'   celltype_col = "cell_type",
#'   condition_col = "condition"
#' )
#'
#' # From a plain data.frame (no Seurat required)
#' meta <- read.csv("cell_metadata.csv")
#' sccomp_report(
#'   meta_data    = meta,
#'   sample_col   = "sample",
#'   celltype_col = "cell_type",
#'   condition_col = "condition"
#' )
#' }
sccomp_report <- function(seurat_obj   = NULL,
                           meta_data    = NULL,
                           sample_col,
                           celltype_col,
                           condition_col = NULL,
                           output = "scReportComposition.html",
                           title  = "scReportComposition") {

  # ---- 1. Prepare composition table ----
  comp_data <- prepare_composition_data(
    seurat_obj    = seurat_obj,
    meta_data     = meta_data,
    sample_col    = sample_col,
    celltype_col  = celltype_col,
    condition_col = condition_col
  )

  # ---- 2. Summary ----
  summary <- build_summary(comp_data, condition_col = condition_col)

  # ---- 3. Colour map ----
  all_ct      <- as.character(levels(comp_data$celltype))
  ct_colors   <- celltype_color_map(all_ct)

  # ---- 4. Plots ----
  message("Generating plots...")
  p_sample    <- plot_sample_composition(comp_data, ct_colors)
  p_condition <- plot_condition_composition(comp_data, ct_colors)
  p_celltype  <- plot_celltype_fraction(comp_data, ct_colors)

  # ---- 5. Assemble & write HTML ----
  message("Assembling HTML report...")
  build_html(
    summary        = summary,
    plot_sample    = p_sample,
    plot_condition = p_condition,
    plot_celltype  = p_celltype,
    output         = output,
    title          = title
  )

  invisible(output)
}
