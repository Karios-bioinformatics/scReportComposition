# scReportComposition: build_comp_audit_warnings.R — Warning Detection ----------
#
# Generates warning_table from intermediate composition tables.
# 8 warning types, each with severity (high/medium/low).
# All warnings are descriptive — no statistical inference.


#' Build Warning Table
#'
#' Scans all intermediate tables and generates warnings for:
#' missing metadata, sample-group inconsistency, low sample/identity cell counts,
#' group size < 2, sample dominance, group-batch confounding, and a
#' descriptive-only flag.
#'
#' @param tables Named list from \code{build_all_composition_tables()}
#' @param min_cells_per_sample   Threshold for low-sample warning
#' @param min_cells_per_identity Threshold for low-identity warning
#' @param dominance_threshold    Threshold for sample-dominance warning
#' @param batch_col              Optional batch column name
#' @return data.frame with columns: warning_type, severity, target, message.
#'   Returns 0-row data.frame if no warnings.
#' @keywords internal
build_warning_table <- function(tables,
                                 min_cells_per_sample   = 500,
                                 min_cells_per_identity  = 50,
                                 dominance_threshold     = 0.8,
                                 batch_col               = NULL) {

  warnings <- list()

  comp_meta             <- tables$comp_meta
  sample_total          <- tables$sample_total
  count_table           <- tables$count_table
  prop_table            <- tables$prop_table
  dominance_table       <- tables$dominance_table

  # ---- 1. Missing metadata (NA values) ----
  na_fields <- c("sample", "group", "identity")
  if (!is.null(batch_col) && "batch" %in% names(comp_meta)) {
    na_fields <- c(na_fields, "batch")
  }
  for (field in na_fields) {
    if (field %in% names(comp_meta)) {
      n_na <- sum(is.na(comp_meta[[field]]))
      if (n_na > 0) {
        warnings[[length(warnings) + 1]] <- data.frame(
          warning_type = "missing_metadata",
          severity     = "high",
          target       = field,
          message      = sprintf(
            "Metadata column '%s' contains %d missing value(s).",
            field, n_na
          ),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # ---- 2. Inconsistent sample-group mapping ----
  sg_map <- unique(comp_meta[, c("sample", "group"), drop = FALSE])
  sg_counts <- table(sg_map$sample)
  multi_group_samples <- names(sg_counts[sg_counts > 1])
  if (length(multi_group_samples) > 0) {
    for (s in multi_group_samples) {
      groups_for_sample <- unique(
        sg_map$group[sg_map$sample == s]
      )
      warnings[[length(warnings) + 1]] <- data.frame(
        warning_type = "inconsistent_sample_group",
        severity     = "high",
        target       = s,
        message      = sprintf(
          "Sample %s is associated with multiple groups (%s). Sample-level composition may be invalid.",
          s, paste(groups_for_sample, collapse = ", ")
        ),
        stringsAsFactors = FALSE
      )
    }
  }

  # ---- 3. Low sample cell count ----
  low_samples <- sample_total$sample[
    sample_total$total_cells < min_cells_per_sample
  ]
  if (length(low_samples) > 0) {
    for (s in low_samples) {
      n <- sample_total$total_cells[sample_total$sample == s]
      warnings[[length(warnings) + 1]] <- data.frame(
        warning_type = "low_sample_cell_count",
        severity     = "medium",
        target       = s,
        message      = sprintf(
          "Sample %s has low total cell count (%s cells). Composition estimates may be unstable.",
          s, format(n, big.mark = ",")
        ),
        stringsAsFactors = FALSE
      )
    }
  }

  # ---- 4. Low identity cell count ----
  id_totals <- stats::aggregate(
    n_cells ~ identity, data = count_table, FUN = sum
  )
  low_ids <- id_totals$identity[id_totals$n_cells < min_cells_per_identity]
  if (length(low_ids) > 0) {
    for (i in low_ids) {
      n <- id_totals$n_cells[id_totals$identity == i]
      warnings[[length(warnings) + 1]] <- data.frame(
        warning_type = "low_identity_cell_count",
        severity     = "medium",
        target       = i,
        message      = sprintf(
          "Identity %s has low total cell count (%s cells). Proportion estimates may be unstable.",
          i, format(n, big.mark = ",")
        ),
        stringsAsFactors = FALSE
      )
    }
  }

  # ---- 5. Group n < 2 ----
  group_n <- stats::aggregate(
    sample ~ group,
    data = unique(prop_table[, c("sample", "group")]),
    FUN = length
  )
  names(group_n)[names(group_n) == "sample"] <- "n_samples"

  small_groups <- group_n$group[group_n$n_samples < 2]
  if (length(small_groups) > 0) {
    for (g in small_groups) {
      n <- group_n$n_samples[group_n$group == g]
      warnings[[length(warnings) + 1]] <- data.frame(
        warning_type = "group_n_less_than_2",
        severity     = "high",
        target       = g,
        message      = sprintf(
          "Group %s has fewer than 2 samples (n=%s). Group-level composition should be interpreted descriptively.",
          g, n
        ),
        stringsAsFactors = FALSE
      )
    }
  }

  # ---- 6. Sample dominance ----
  for (i in seq_len(nrow(dominance_table))) {
    row <- dominance_table[i, ]
    if (row$max_sample_contribution >= dominance_threshold) {
      pct <- round(row$max_sample_contribution * 100, 1)
      warnings[[length(warnings) + 1]] <- data.frame(
        warning_type = "sample_dominance",
        severity     = "high",
        target       = row$identity,
        message      = sprintf(
          "Identity %s is dominated by sample %s (%s%% of cells), suggesting a sample-specific composition pattern. Interpret with caution.",
          row$identity, row$dominant_sample, pct
        ),
        stringsAsFactors = FALSE
      )
    }
  }

  # ---- 7. Group-batch confounding ----
  if (!is.null(batch_col) && "batch" %in% names(sample_total)) {
    gb <- unique(sample_total[, c("group", "batch"), drop = FALSE])

    for (g in unique(gb$group)) {
      batches_in_group <- unique(gb$batch[gb$group == g])
      for (b in batches_in_group) {
        groups_in_batch <- unique(gb$group[gb$batch == b])
        if (length(groups_in_batch) == 1 && length(batches_in_group) == 1) {
          warnings[[length(warnings) + 1]] <- data.frame(
            warning_type = "group_batch_confounding",
            severity     = "high",
            target       = paste(g, b, sep = " x "),
            message      = sprintf(
              "Group %s and batch %s appear to be confounded. Group-level composition may reflect batch rather than biological differences.",
              g, b
            ),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  # ---- 8. Descriptive-only banner ----
  if (length(small_groups) > 0) {
    warnings[[length(warnings) + 1]] <- data.frame(
      warning_type = "descriptive_only",
      severity     = "high",
      target       = "global",
      message      = paste(
        "Some groups have fewer than 2 samples.",
        "This report provides descriptive composition summaries only;",
        "no inferential differential composition is performed."
      ),
      stringsAsFactors = FALSE
    )
  }

  # ---- Assemble ----
  if (length(warnings) == 0) {
    return(data.frame(
      warning_type = character(),
      severity     = character(),
      target       = character(),
      message      = character(),
      stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, warnings)

  sev_order <- c("high" = 1, "medium" = 2, "low" = 3)
  out <- out[order(sev_order[out$severity]), ]
  rownames(out) <- NULL

  message(sprintf("warning_table built: %d warnings (%d high, %d medium, %d low)",
                  nrow(out),
                  sum(out$severity == "high"),
                  sum(out$severity == "medium"),
                  sum(out$severity == "low")))

  out
}
