# scReportComposition: build_comp_audit_data.R — Intermediate Tables -----------
#
# Generates all intermediate tables from Seurat metadata for the
# composition audit report. No plotting — pure data transformation.
#
# Tables: comp_meta, count_table, prop_table, sample_total,
#         group_summary, identity_sample_contribution, dominance_table


# ---- Identity column resolution ----------------------------------------------

#' Resolve the identity column from user inputs
#'
#' Priority: identity_col > celltype_col > cluster_col.
#' Errors if none are provided and usable.
#'
#' @param seurat_obj A Seurat object
#' @param identity_col Explicit identity column name (or NULL)
#' @param celltype_col Cell-type column name (or NULL)
#' @param cluster_col  Cluster column name (or NULL)
#' @return The resolved column name (character)
#' @keywords internal
resolve_identity_col <- function(seurat_obj, identity_col, celltype_col, cluster_col) {
  meta <- seurat_obj@meta.data

  if (!is.null(identity_col)) {
    if (!identity_col %in% colnames(meta)) {
      stop("identity_col '", identity_col, "' not found in Seurat metadata", call. = FALSE)
    }
    return(identity_col)
  }
  if (!is.null(celltype_col)) {
    if (!celltype_col %in% colnames(meta)) {
      stop("celltype_col '", celltype_col, "' not found in Seurat metadata", call. = FALSE)
    }
    return(celltype_col)
  }
  if (!is.null(cluster_col)) {
    if (!cluster_col %in% colnames(meta)) {
      stop("cluster_col '", cluster_col, "' not found in Seurat metadata", call. = FALSE)
    }
    return(cluster_col)
  }

  stop(
    "No identity column specified. Provide identity_col, celltype_col, or cluster_col.",
    call. = FALSE
  )
}


# ---- Identity sort helper ----------------------------------------------------

#' Sort identity labels for display
#'
#' If all identities match C0, C1, ..., C19, sorts numerically.
#' Otherwise sorts by descending total cell count (frequency), then alphabetically.
#'
#' @param identities Character vector of identity names
#' @param count_table Optional; if provided, sorts by frequency descending
#' @return Sorted character vector
#' @keywords internal
identity_display_order <- function(identities, count_table = NULL) {
  if (all(grepl("^C\\d+$", identities))) {
    nums <- as.integer(gsub("^C", "", identities))
    return(identities[order(nums)])
  }

  if (!is.null(count_table)) {
    freq <- stats::aggregate(n_cells ~ identity, data = count_table, FUN = sum)
    freq <- freq[order(freq$n_cells, decreasing = TRUE), ]
    ordered_ids <- intersect(as.character(freq$identity), identities)
    remaining   <- setdiff(identities, ordered_ids)
    return(c(ordered_ids, sort(remaining)))
  }

  sort(identities)
}


# ---- Table 1: comp_meta -------------------------------------------------------

#' Build comp_meta: cell-level metadata table
#'
#' Extracts per-cell sample, group, identity, and optionally batch
#' from Seurat metadata. Does not modify the Seurat object.
#'
#' @param seurat_obj A Seurat object
#' @param sample_col  Sample column name
#' @param group_col   Group column name
#' @param identity_col Resolved identity column name
#' @param batch_col   Optional batch column name
#' @return data.frame with columns: cell_id, sample, group, identity, [batch]
#' @keywords internal
build_comp_meta <- function(seurat_obj, sample_col, group_col, identity_col, batch_col = NULL) {
  meta <- seurat_obj@meta.data

  required <- c(sample_col, group_col, identity_col)
  missing  <- setdiff(required, colnames(meta))
  if (length(missing) > 0) {
    stop("Missing required column(s) in Seurat metadata: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  out <- data.frame(
    cell_id  = colnames(seurat_obj),
    sample   = as.character(meta[[sample_col]]),
    group    = as.character(meta[[group_col]]),
    identity = as.character(meta[[identity_col]]),
    stringsAsFactors = FALSE
  )

  if (!is.null(batch_col)) {
    if (!batch_col %in% colnames(meta)) {
      stop("batch_col '", batch_col, "' not found in Seurat metadata", call. = FALSE)
    }
    out$batch <- as.character(meta[[batch_col]])
  }

  message(sprintf(
    "comp_meta built: %d cells, %d samples, %d groups, %d identities%s",
    nrow(out),
    length(unique(out$sample)),
    length(unique(out$group)),
    length(unique(out$identity)),
    if (!is.null(batch_col)) paste0(", ", length(unique(out$batch)), " batches") else ""
  ))

  out
}


# ---- Table 2: count_table -----------------------------------------------------

#' Build count_table: sample × identity cell counts
#'
#' Counts cells per sample × group × identity, then completes
#' all sample × identity combinations with n_cells = 0 for missing pairs.
#'
#' @param comp_meta data.frame from \code{build_comp_meta()}
#' @return data.frame with columns: sample, group, identity, n_cells
#' @keywords internal
build_count_table <- function(comp_meta) {
  counts <- as.data.frame(
    table(comp_meta$sample, comp_meta$group, comp_meta$identity),
    stringsAsFactors = FALSE
  )
  names(counts) <- c("sample", "group", "identity", "n_cells")
  counts$n_cells <- as.integer(counts$n_cells)

  all_samples   <- natural_sort(unique(comp_meta$sample))
  all_identities <- natural_sort(unique(comp_meta$identity))

  sample_group <- unique(comp_meta[, c("sample", "group"), drop = FALSE])

  grid <- expand.grid(
    sample   = all_samples,
    identity = all_identities,
    stringsAsFactors = FALSE
  )
  grid <- merge(grid, sample_group, by = "sample", all.x = TRUE)

  counts <- merge(grid, counts, by = c("sample", "group", "identity"), all.x = TRUE)
  counts$n_cells[is.na(counts$n_cells)] <- 0L

  counts$sample   <- factor(counts$sample,   levels = all_samples)
  counts$identity <- factor(counts$identity, levels = all_identities)

  message(sprintf("count_table built: %d rows (%d samples × %d identities)",
                  nrow(counts), length(all_samples), length(all_identities)))

  counts
}


# ---- Table 3: prop_table ------------------------------------------------------

#' Build prop_table: sample × identity proportions
#'
#' Computes proportion = n_cells / total_cells within each sample.
#'
#' @param count_table data.frame from \code{build_count_table()}
#' @return data.frame with columns: sample, group, identity, n_cells,
#'   total_cells, proportion
#' @keywords internal
build_prop_table <- function(count_table) {
  totals <- stats::aggregate(
    n_cells ~ sample + group, data = count_table, FUN = sum
  )
  names(totals)[names(totals) == "n_cells"] <- "total_cells"

  out <- merge(count_table, totals, by = c("sample", "group"), all.x = TRUE)

  out$proportion <- ifelse(
    out$total_cells > 0,
    out$n_cells / out$total_cells,
    0
  )

  out <- out[order(out$sample, out$identity), ]
  rownames(out) <- NULL

  message(sprintf("prop_table built: %d rows", nrow(out)))
  out
}


# ---- Table 4: sample_total ----------------------------------------------------

#' Build sample_total: per-sample cell totals
#'
#' @param prop_table data.frame from \code{build_prop_table()}
#' @param batch_col  Optional batch column name
#' @param comp_meta  comp_meta for batch lookup (required if batch_col present)
#' @return data.frame with columns: sample, group, total_cells, [batch]
#' @keywords internal
build_sample_total <- function(prop_table, batch_col = NULL, comp_meta = NULL) {
  out <- unique(prop_table[, c("sample", "group", "total_cells"), drop = FALSE])
  out <- out[order(out$sample), ]
  rownames(out) <- NULL

  if (!is.null(batch_col) && !is.null(comp_meta)) {
    batch_lookup <- unique(comp_meta[, c("sample", "batch"), drop = FALSE])
    out <- merge(out, batch_lookup, by = "sample", all.x = TRUE)
  }

  message(sprintf("sample_total built: %d samples", nrow(out)))
  out
}


# ---- Table 5: group_summary ---------------------------------------------------

#' Build group_summary: group-level descriptive composition statistics
#'
#' Computes mean and sd of sample-level proportions per group × identity.
#' This is descriptive only — no statistical testing.
#' sd is NA when n_samples < 2.
#'
#' @param prop_table data.frame from \code{build_prop_table()}
#' @return data.frame with columns: group, identity, mean_proportion,
#'   sd_proportion, n_samples, total_cells
#' @keywords internal
build_group_summary <- function(prop_table) {
  agg_mean <- stats::aggregate(
    proportion ~ group + identity, data = prop_table, FUN = mean
  )
  names(agg_mean)[names(agg_mean) == "proportion"] <- "mean_proportion"

  agg_sd <- stats::aggregate(
    proportion ~ group + identity, data = prop_table, FUN = sd
  )
  names(agg_sd)[names(agg_sd) == "proportion"] <- "sd_proportion"

  agg_n <- stats::aggregate(
    proportion ~ group + identity, data = prop_table, FUN = length
  )
  names(agg_n)[names(agg_n) == "proportion"] <- "n_samples"

  agg_cells <- stats::aggregate(
    n_cells ~ group + identity, data = prop_table, FUN = sum
  )
  names(agg_cells)[names(agg_cells) == "n_cells"] <- "total_cells"

  out <- merge(agg_mean, agg_sd,     by = c("group", "identity"), all = TRUE)
  out <- merge(out,      agg_n,      by = c("group", "identity"), all = TRUE)
  out <- merge(out,      agg_cells,  by = c("group", "identity"), all = TRUE)

  out$sd_proportion[out$n_samples < 2] <- NA_real_

  out <- out[order(out$group, out$identity), ]
  rownames(out) <- NULL

  message(sprintf("group_summary built: %d rows", nrow(out)))
  out
}


# ---- Table 6: identity_sample_contribution ------------------------------------

#' Build identity_sample_contribution: reverse contribution table
#'
#' For each identity, computes what fraction of its total cells
#' come from each sample: sample_contribution = n_cells / identity_total_cells.
#'
#' @param count_table data.frame from \code{build_count_table()}
#' @return data.frame with columns: identity, sample, group, n_cells,
#'   identity_total_cells, sample_contribution
#' @keywords internal
build_identity_sample_contribution <- function(count_table) {
  id_totals <- stats::aggregate(
    n_cells ~ identity, data = count_table, FUN = sum
  )
  names(id_totals)[names(id_totals) == "n_cells"] <- "identity_total_cells"

  out <- merge(count_table, id_totals, by = "identity", all.x = TRUE)

  out$sample_contribution <- ifelse(
    out$identity_total_cells > 0,
    out$n_cells / out$identity_total_cells,
    0
  )

  out <- out[out$n_cells > 0, ]
  out <- out[order(out$identity, out$sample), ]
  rownames(out) <- NULL

  message(sprintf("identity_sample_contribution built: %d rows", nrow(out)))
  out
}


# ---- Table 7: dominance_table -------------------------------------------------

#' Build dominance_table: maximum sample contribution per identity
#'
#' For each identity, identifies the sample that contributes the most cells.
#' Sorted by max_sample_contribution descending.
#'
#' @param identity_sample_contribution data.frame from
#'   \code{build_identity_sample_contribution()}
#' @return data.frame with columns: identity, dominant_sample,
#'   dominant_group, dominant_n_cells, identity_total_cells,
#'   max_sample_contribution
#' @keywords internal
build_dominance_table <- function(identity_sample_contribution) {
  ids <- unique(identity_sample_contribution$identity)
  out <- do.call(rbind, lapply(ids, function(id) {
    sub <- identity_sample_contribution[identity_sample_contribution$identity == id, ]
    top <- sub[which.max(sub$sample_contribution), ]
    data.frame(
      identity                = id,
      dominant_sample         = top$sample,
      dominant_group          = top$group,
      dominant_n_cells        = top$n_cells,
      identity_total_cells    = top$identity_total_cells,
      max_sample_contribution  = top$sample_contribution,
      stringsAsFactors = FALSE
    )
  }))

  out <- out[order(out$max_sample_contribution, decreasing = TRUE), ]
  rownames(out) <- NULL

  message(sprintf("dominance_table built: %d identities", nrow(out)))
  out
}


# ---- Master orchestrator ------------------------------------------------------

#' Build All Composition Audit Tables
#'
#' Orchestrates the construction of all 7 intermediate tables from a
#' Seurat object.
#'
#' @param seurat_obj   A Seurat object
#' @param sample_col   Sample column name
#' @param group_col    Group column name
#' @param identity_col Resolved identity column name
#' @param batch_col    Optional batch column name
#' @return A named list of all intermediate tables
#' @keywords internal
build_all_composition_tables <- function(seurat_obj, sample_col, group_col,
                                          identity_col, batch_col = NULL) {
  message("Building composition audit tables...")

  tables <- list()

  tables$comp_meta <- build_comp_meta(
    seurat_obj, sample_col, group_col, identity_col, batch_col
  )

  tables$count_table <- build_count_table(tables$comp_meta)

  tables$prop_table <- build_prop_table(tables$count_table)

  tables$sample_total <- build_sample_total(
    tables$prop_table, batch_col, tables$comp_meta
  )

  tables$group_summary <- build_group_summary(tables$prop_table)

  tables$identity_sample_contribution <- build_identity_sample_contribution(
    tables$count_table
  )

  tables$dominance_table <- build_dominance_table(
    tables$identity_sample_contribution
  )

  message("All composition audit tables built successfully.")
  tables
}
