# scReportComposition: build_screport_composition.R — Main Orchestrator ---------
#
# Main entry point for the composition audit report.
# Orchestrates: validate → tables → warnings → plots → HTML → return list.
#
# Produces a self-contained interactive HTML report with top tab navigation
# covering: Overview, Warnings, Metadata Audit, Sample-Level Composition,
# Group-Level Descriptive, Sample Dominance, Methods/Notes.


#' Build an Interactive Composition Audit Report
#'
#' Generates a self-contained HTML report that audits cell-type/cluster
#' composition across samples and groups. The report focuses on sample-level
#' proportions, group-level descriptive summaries, and sample-dominance
#' detection — without performing differential composition statistics.
#'
#' Seven core plots are generated:
#' \enumerate{
#'   \item Sample total cell count (barplot, coloured by group)
#'   \item Identity composition by sample (stacked barplot)
#'   \item Identity proportion heatmap (sample x identity)
#'   \item Sample contribution within each identity (heatmap)
#'   \item Maximum sample contribution per identity (horizontal bar)
#'   \item Descriptive identity composition by group (stacked bar)
#'   \item Sample-level identity proportions by group (faceted jitter)
#' }
#'
#' An automatic warning system flags:
#' \itemize{
#'   \item Groups with fewer than 2 samples
#'   \item Samples below minimum cell count threshold
#'   \item Identities below minimum cell count threshold
#'   \item Identities dominated by a single sample
#'   \item Group-batch confounding
#' }
#'
#' @param seurat_obj A Seurat object. Only \code{@@meta.data} is read;
#'   the object is never modified.
#' @param sample_col Name of the sample column in metadata (required).
#' @param group_col  Name of the group/condition column in metadata (required).
#' @param cluster_col Name of the cluster column. Used if \code{identity_col}
#'   and \code{celltype_col} are both \code{NULL}.
#' @param celltype_col Name of the cell-type column. Priority over
#'   \code{cluster_col}.
#' @param batch_col Optional batch column. When provided, enables
#'   group-batch confounding detection.
#' @param identity_col Explicit identity column. Highest priority override.
#' @param output_file Output HTML filename. Default:
#'   \code{"scReport_composition_audit.html"}.
#' @param out_dir Output directory. Default: \code{"."}.
#' @param title Report title. Default: \code{"scReportComposition Audit Report"}.
#' @param min_cells_per_sample Minimum cells per sample before warning.
#' @param min_cells_per_identity Minimum cells per identity before warning.
#' @param dominance_threshold Threshold for sample-dominance warning
#'   (0–1). Default: \code{0.8}.
#' @param top_n_identity If set, limit to the top N identities by
#'   total cell count. \code{NULL} means all identities.
#' @param interactive If \code{TRUE} (default), generates a full
#'   interactive HTML report. If \code{FALSE}, returns the list of
#'   intermediate tables and plot objects without writing HTML.
#'
#' @return Invisibly, a named list with elements:
#'   \item{meta}{Cell-level metadata (comp_meta)}
#'   \item{count_table}{Sample x identity cell counts}
#'   \item{prop_table}{Sample x identity proportions}
#'   \item{sample_total}{Per-sample cell totals}
#'   \item{group_summary}{Group-level descriptive stats}
#'   \item{identity_sample_contribution}{Reverse contribution table}
#'   \item{dominance_table}{Max sample contribution per identity}
#'   \item{warning_table}{All generated warnings}
#'   \item{plots}{Named list of 7 plotly htmlwidgets}
#'   \item{output_file}{Path to the generated HTML (NULL if interactive=FALSE)}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(scReportComposition)
#' library(Seurat)
#'
#' obj <- readRDS("my_seurat.rds")
#' result <- build_screport_composition(
#'   seurat_obj   = obj,
#'   sample_col   = "sample",
#'   group_col    = "condition",
#'   cluster_col  = "cluster"
#' )
#'
#' # Inspect warnings
#' print(result$warning_table)
#'
#' # Access dominance data
#' head(result$dominance_table)
#' }
build_screport_composition <- function(seurat_obj,
                                        sample_col,
                                        group_col,
                                        cluster_col    = NULL,
                                        celltype_col   = NULL,
                                        batch_col      = NULL,
                                        identity_col   = NULL,
                                        output_file    = "scReport_composition_audit.html",
                                        out_dir        = ".",
                                        title          = "scReportComposition Audit Report",
                                        min_cells_per_sample  = 500,
                                        min_cells_per_identity = 50,
                                        dominance_threshold    = 0.8,
                                        top_n_identity  = NULL,
                                        interactive     = TRUE) {

  # ---- 0. Validate inputs ----
  if (!inherits(seurat_obj, "Seurat")) {
    stop("seurat_obj must be a Seurat object. Other formats are not yet supported.",
         call. = FALSE)
  }

  if (missing(sample_col) || !is.character(sample_col) || nchar(sample_col) == 0) {
    stop("sample_col is required and must be a non-empty character string.",
         call. = FALSE)
  }

  if (missing(group_col) || !is.character(group_col) || nchar(group_col) == 0) {
    stop("group_col is required and must be a non-empty character string.",
         call. = FALSE)
  }

  meta <- seurat_obj@meta.data

  if (!sample_col %in% colnames(meta)) {
    stop("sample_col '", sample_col, "' not found in Seurat metadata.",
         call. = FALSE)
  }

  if (!group_col %in% colnames(meta)) {
    stop("group_col '", group_col, "' not found in Seurat metadata.",
         call. = FALSE)
  }

  if (!is.null(batch_col) && !batch_col %in% colnames(meta)) {
    stop("batch_col '", batch_col, "' not found in Seurat metadata.",
         call. = FALSE)
  }

  # ---- 1. Resolve identity column ----
  identity_col_used <- resolve_identity_col(
    seurat_obj, identity_col, celltype_col, cluster_col
  )

  message("Using identity column: ", identity_col_used)

  # ---- 2. Build all intermediate tables ----
  tables <- build_all_composition_tables(
    seurat_obj, sample_col, group_col, identity_col_used, batch_col
  )

  # ---- 3. Top N identity filter (disabled) ----
  # NOTE: top_n_identity is accepted but not yet implemented.
  # Filtering and rebuilding dependent tables would change the denominator
  # of sample-level proportions (total_cells would exclude non-top identities),
  # violating the semantic contract that proportion = n_cells / sample_total_cells.
  # Future implementation: if top_n_identity is set, merge non-top identities
  # into an "Other" category so sample total_cells stays intact.
  if (!is.null(top_n_identity)) {
    message("top_n_identity is not yet implemented and will be ignored. ",
            "All identities are retained.")
  }

  # ---- 4. Build warnings ----
  warning_table <- build_warning_table(
    tables,
    min_cells_per_sample  = min_cells_per_sample,
    min_cells_per_identity = min_cells_per_identity,
    dominance_threshold    = dominance_threshold,
    batch_col              = batch_col
  )

  # ---- 5. Build colour maps ----
  all_ids <- as.character(levels(tables$prop_table$identity))
  identity_colors <- celltype_color_map(all_ids)

  all_groups <- natural_sort(as.character(unique(tables$sample_total$group)))
  group_colors <- condition_color_map(all_groups)

  # ---- 6. Build report parameters ----
  params <- list(
    n_cells            = nrow(tables$comp_meta),
    n_samples          = length(unique(tables$comp_meta$sample)),
    n_groups           = length(unique(tables$comp_meta$group)),
    n_identities       = length(all_ids),
    sample_col         = sample_col,
    group_col          = group_col,
    identity_col_used  = identity_col_used,
    dominance_threshold = dominance_threshold
  )

  # ---- 7. Build plots ----
  plots <- build_all_composition_plots(
    tables,
    group_colors    = group_colors,
    identity_colors = identity_colors,
    warning_table   = warning_table,
    dominance_threshold = dominance_threshold
  )

  # ---- 8. Build HTML report ----
  output_path <- NULL
  if (interactive) {
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE)
    }
    output_path <- file.path(out_dir, output_file)

    build_comp_audit_html(
      output        = output_path,
      title         = title,
      params        = params,
      warning_table = warning_table,
      tables        = tables,
      plots         = plots,
      batch_col     = batch_col
    )
  } else {
    message("interactive = FALSE: skipping HTML generation. Plots are returned as plotly objects.")
  }

  # ---- 9. Assemble return list ----
  result <- list(
    meta                            = tables$comp_meta,
    count_table                     = tables$count_table,
    prop_table                      = tables$prop_table,
    sample_total                    = tables$sample_total,
    group_summary                   = tables$group_summary,
    identity_sample_contribution     = tables$identity_sample_contribution,
    dominance_table                 = tables$dominance_table,
    warning_table                   = warning_table,
    plots                           = plots,
    output_file                     = if (interactive) output_path else NULL
  )

  message("\n=== Composition audit report complete ===")
  if (interactive) {
    message("HTML report: ", normalizePath(output_path, mustWork = FALSE))
  }
  message(sprintf(
    "Summary: %s cells, %s samples, %s groups, %s identities, %s warnings",
    params$n_cells, params$n_samples, params$n_groups, params$n_identities,
    nrow(warning_table)
  ))

  invisible(result)
}
