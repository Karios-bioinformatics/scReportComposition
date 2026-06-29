# scReportComposition: Main API -------------------------------------------------
#
# sccomp_report() is the single entry point.
# Orchestrates: prepare → summarise → plot (10 modules) → assemble → write.
# Accepts either a Seurat object or a plain data.frame of metadata.


#' Generate a Full Cell Composition HTML Report
#'
#' Takes a Seurat object (or a \code{data.frame} of cell-level metadata)
#' and user-specified column names, builds a composition table, creates
#' interactive Plotly visualisations, and writes a self-contained HTML
#' report with left-side navigation covering all 10 modules.
#'
#' Report modules:
#' \itemize{
#'   \item Overview — summary cards (total cells, samples, cell types,
#'         conditions, cells/samples per condition)
#'   \item Sample Composition — cell counts + fractions per sample
#'         (stacked bars)
#'   \item Condition Composition — cell counts + fractions per condition
#'         (stacked bars; hidden when no condition column)
#'   \item Cell Type Distribution — fraction + count boxplots by condition
#'         (hidden when no condition column)
#'   \item Heatmaps — sample × cell type + condition × cell type fraction
#'         heatmaps
#'   \item Table — scrollable composition table
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
#'   (e.g. \code{"condition"}).  When \code{NULL}, condition-dependent
#'   sections are omitted.
#' @param output Path to the output HTML file.
#'   Default: \code{"scReportComposition.html"}.
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

  # ---- 3. Colour maps ----
  all_ct   <- as.character(levels(comp_data$celltype))
  ct_cols  <- celltype_color_map(all_ct)

  has_cond <- !is.na(summary$n_conditions)
  cond_cols <- NULL
  if (has_cond) {
    conds     <- levels(comp_data$condition)
    cond_cols <- condition_color_map(as.character(conds))
  }

  # ---- 4. Generate all plots ----
  message("Generating plots...")

  plots <- list()

  # Sample composition (counts + fractions)
  message("  - Sample count & fraction bars...")
  plots$p_sample_count <- plot_sample_count_composition(comp_data, ct_cols)
  plots$p_sample_frac  <- plot_sample_composition(comp_data, ct_cols)

  # Condition composition
  if (has_cond) {
    message("  - Condition count & fraction bars...")
    plots$p_cond_count <- plot_condition_count_composition(comp_data, ct_cols)
    plots$p_cond_frac  <- plot_condition_composition(comp_data, ct_cols)

    # Cell type distribution by condition
    message("  - Cell-type distribution by condition...")
    plots$p_ct_frac_by_cond  <- plot_celltype_fraction_by_condition(
      comp_data, cond_cols
    )
    plots$p_ct_count_by_cond <- plot_celltype_count_by_condition(
      comp_data, cond_cols
    )
  } else {
    plots$p_cond_count      <- NULL
    plots$p_cond_frac       <- NULL
    plots$p_ct_frac_by_cond <- NULL
    plots$p_ct_count_by_cond <- NULL
  }

  # Heatmaps
  message("  - Heatmaps...")
  plots$p_heatmap_sample <- plot_sample_celltype_heatmap(comp_data)
  plots$p_heatmap_cond   <- if (has_cond) {
    plot_condition_celltype_heatmap(comp_data)
  } else NULL

  # Composition table
  message("  - Composition table...")
  comp_table <- render_composition_table(comp_data, condition_col)

  # ---- 5. Assemble & write HTML ----
  message("Assembling HTML report...")
  build_html(
    summary    = summary,
    plots      = plots,
    comp_table = comp_table,
    output     = output,
    title      = title
  )

  invisible(output)
}
