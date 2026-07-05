# scReportComposition: GSE123013 Acceptance Test ---------------------------------
#
# Run this in R from the scReportComposition project root.
# Expects a Seurat object `sce` to be loaded in the environment.
#
# Usage:
#   devtools::load_all(".")
#   source("inst/test_composition_audit.R")
#
# Expected results:
#   - warning_table catches: group_n_less_than_2 (Sample_gl2, Sample_rhd6)
#   - sample_dominance catches: C14, C15, C17, C8, C11
#   - descriptive_only banner present
#   - No p-values anywhere in report
#   - 7 plots generated
#   - Identity ordering: C0-C19 in natural order


cat("=== scReportComposition Composition Audit Acceptance Test ===\n\n")

# ---- 1. Verify sce exists ----
if (!exists("sce")) {
  stop("Seurat object 'sce' not found. Please load your GSE123013 data first.")
}

cat("1. Seurat object found:", ncol(sce), "cells,", ncol(sce@meta.data), "metadata columns\n")

# ---- 2. Run build_screport_composition ----
cat("\n2. Running build_screport_composition()...\n")

result <- build_screport_composition(
  seurat_obj   = sce,
  sample_col   = "sample",
  group_col    = "condition",
  cluster_col  = "cluster",
  output_file  = "scReport_composition_audit.html",
  out_dir      = ".",
  interactive  = TRUE
)

cat("\n=== Results ===\n")

# ---- 3. Check return structure ----
cat("\n3. Return object structure:\n")
expected_fields <- c("meta", "count_table", "prop_table", "sample_total",
                     "group_summary", "identity_sample_contribution",
                     "dominance_table", "warning_table", "plots", "output_file")
for (f in expected_fields) {
  if (f %in% names(result)) {
    cat(sprintf("   [OK] $%s\n", f))
  } else {
    cat(sprintf("   [FAIL] $%s MISSING\n", f))
  }
}

# ---- 4. Check intermediate tables ----
cat("\n4. Intermediate tables:\n")
cat(sprintf("   comp_meta: %d rows\n", nrow(result$meta)))
cat(sprintf("   count_table: %d rows\n", nrow(result$count_table)))
cat(sprintf("   prop_table: %d rows\n", nrow(result$prop_table)))
cat(sprintf("   sample_total: %d samples\n", nrow(result$sample_total)))
cat(sprintf("   group_summary: %d rows\n", nrow(result$group_summary)))
cat(sprintf("   identity_sample_contribution: %d rows\n",
            nrow(result$identity_sample_contribution)))
cat(sprintf("   dominance_table: %d identities\n", nrow(result$dominance_table)))

# ---- 5. Check identity ordering ----
cat("\n5. Identity ordering (should be C0-C19):\n")
ids <- as.character(levels(result$prop_table$identity))
cat("   ", paste(ids, collapse = ", "), "\n")
is_ordered <- all(ids == paste0("C", seq(0, length(ids) - 1)))
if (is_ordered) {
  cat("   [OK] Natural C-order preserved\n")
} else {
  cat("   [WARN] Order may not be natural\n")
}

# ---- 6. Check warning_table ----
cat("\n6. Warning table:\n")
wt <- result$warning_table
cat(sprintf("   Total warnings: %d\n", nrow(wt)))

# group_n_less_than_2
gn2 <- wt[wt$warning_type == "group_n_less_than_2", ]
cat(sprintf("   group_n_less_than_2: %d\n", nrow(gn2)))
if (nrow(gn2) > 0) {
  for (i in seq_len(nrow(gn2))) {
    cat(sprintf("     - %s: %s\n", gn2$target[i], gn2$message[i]))
  }
  # Check for expected groups
  expected_small <- c("Sample_gl2", "Sample_rhd6")
  found_small <- gn2$target
  for (g in expected_small) {
    if (g %in% found_small) {
      cat(sprintf("     [OK] %s found\n", g))
    } else {
      cat(sprintf("     [FAIL] %s NOT found\n", g))
    }
  }
}

# sample_dominance
sd <- wt[wt$warning_type == "sample_dominance", ]
cat(sprintf("\n   sample_dominance: %d\n", nrow(sd)))
expected_dom <- c("C14", "C15", "C17", "C8", "C11")
if (nrow(sd) > 0) {
  for (i in seq_len(nrow(sd))) {
    cat(sprintf("     - %s: %s\n", sd$target[i], sd$message[i]))
  }
  for (id in expected_dom) {
    if (id %in% sd$target) {
      cat(sprintf("     [OK] %s found\n", id))
    } else {
      cat(sprintf("     [WARN] %s NOT found in dominance warnings\n", id))
    }
  }
}

# descriptive_only
do <- wt[wt$warning_type == "descriptive_only", ]
if (nrow(do) > 0) {
  cat(sprintf("\n   [OK] descriptive_only banner present: %s\n", do$message[1]))
} else {
  cat("\n   [FAIL] descriptive_only banner MISSING\n")
}

# ---- 7. Check no p-values in warnings ----
cat("\n7. Checking for forbidden outputs:\n")
forbidden <- c("p_value", "p_adj", "p.value", "p_val", "significant",
               "enrichment", "batch effect")
found_forbidden <- character()
for (term in forbidden) {
  in_warnings <- any(grepl(term, wt$message, ignore.case = TRUE))
  if (in_warnings) {
    found_forbidden <- c(found_forbidden, term)
  }
}
if (length(found_forbidden) == 0) {
  cat("   [OK] No p-values, significance claims, or batch-effect language found\n")
} else {
  cat("   [FAIL] Found forbidden terms:", paste(found_forbidden, collapse = ", "), "\n")
}

# ---- 8. Check plots ----
cat("\n8. Plot list:\n")
expected_plots <- c("sample_total", "composition_by_sample", "proportion_heatmap",
                    "sample_contribution_heatmap", "max_sample_contribution",
                    "composition_by_group", "sample_level_by_group")
for (pname in expected_plots) {
  if (pname %in% names(result$plots)) {
    cat(sprintf("   [OK] %s\n", pname))
  } else {
    cat(sprintf("   [FAIL] %s MISSING\n", pname))
  }
}

# ---- 9. Check HTML output ----
cat("\n9. HTML output:\n")
if (!is.null(result$output_file) && file.exists(result$output_file)) {
  size <- file.info(result$output_file)$size
  cat(sprintf("   [OK] %s (%s bytes)\n", result$output_file, format(size, big.mark = ",")))
} else {
  cat("   [FAIL] HTML file not found\n")
}

# ---- 10. Verify old sccomp_report still works ----
cat("\n10. Legacy sccomp_report() check:\n")
legacy_ok <- exists("sccomp_report", mode = "function")
if (legacy_ok) {
  cat("   [OK] sccomp_report() still exported and available\n")
} else {
  cat("   [FAIL] sccomp_report() not found\n")
}

# ---- Summary ----
cat("\n=== Test Complete ===\n")
cat(sprintf("Warnings captured: %d total (%d high, %d medium)\n",
            nrow(wt), sum(wt$severity == "high"), sum(wt$severity == "medium")))
cat(sprintf("Plots generated: %d/7\n", length(result$plots)))
cat(sprintf("HTML generated: %s\n",
            if (!is.null(result$output_file)) result$output_file else "NONE"))
cat("\nOpen the HTML report in a browser to verify:\n")
cat("  - Top tab navigation works\n")
cat("  - All 7 plots are interactive (hover/zoom/pan)\n")
cat("  - Warnings tab shows all captured warnings\n")
cat("  - Methods tab explains the methodology\n")
cat("  - Group-level section shows descriptive-only notice\n")
