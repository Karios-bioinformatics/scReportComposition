# Smoke test for scReportComposition v0.1.0
# Run from package root: Rscript inst/smoke_test.R

library(Seurat)
library(SeuratObject)
library(plotly)
library(htmltools)
library(jsonlite)

# ---- Source all package files (dynamic path) ----
pkg_root <- normalizePath(".")
if (!file.exists(file.path(pkg_root, "DESCRIPTION"))) {
  # Fallback: try parent dirs (in case run from inst/)
  for (d in c("..", "../..", "../../..")) {
    if (file.exists(file.path(pkg_root, d, "DESCRIPTION"))) {
      pkg_root <- normalizePath(file.path(pkg_root, d))
      break
    }
  }
}
message("Package root: ", pkg_root)

r_dir <- file.path(pkg_root, "R")
stopifnot(dir.exists(r_dir))
for (f in list.files(r_dir, full.names = TRUE, pattern = "\\.R$")) {
  message("Sourcing: ", basename(f))
  source(f)
}

# ---- Build mock data ----
set.seed(42)

n_cells   <- 2000
samples   <- paste0("Sample_", 1:4)
celltypes <- c("T_cell", "B_cell", "Macrophage", "NK_cell", "Monocyte", "Dendritic")
conditions <- c("Control", "Treatment")

sample_vec    <- sample(samples, n_cells, replace = TRUE)
condition_vec <- character(n_cells)
celltype_vec  <- character(n_cells)

condition_vec[sample_vec %in% c("Sample_1", "Sample_2")] <- "Control"
condition_vec[sample_vec %in% c("Sample_3", "Sample_4")] <- "Treatment"

probs_control   <- c(0.10, 0.25, 0.20, 0.15, 0.20, 0.10)
probs_treatment <- c(0.30, 0.10, 0.20, 0.15, 0.15, 0.10)

for (i in seq_len(n_cells)) {
  if (condition_vec[i] == "Control") {
    celltype_vec[i] <- sample(celltypes, 1, prob = probs_control)
  } else {
    celltype_vec[i] <- sample(celltypes, 1, prob = probs_treatment)
  }
}

# metadata data.frame (can be used standalone)
meta <- data.frame(
  orig.ident = sample_vec,
  condition  = condition_vec,
  cell_type  = celltype_vec,
  row.names  = paste0("Cell_", seq_len(n_cells)),
  stringsAsFactors = FALSE
)

# Seurat object (for Seurat-based tests)
counts <- matrix(
  rpois(50 * n_cells, lambda = 2),
  nrow = 50,
  dimnames = list(paste0("Gene_", 1:50), rownames(meta))
)
obj <- CreateSeuratObject(counts = counts, meta.data = meta)
message("Mock Seurat object: ", ncol(obj), " cells, ", nrow(obj), " genes")

# ---- Test 1: prepare_composition_data from Seurat (with condition) ----
message("\n=== Test 1: prepare_composition_data (Seurat, with condition) ===")
comp <- prepare_composition_data(
  seurat_obj    = obj,
  sample_col    = "orig.ident",
  celltype_col  = "cell_type",
  condition_col = "condition"
)
stopifnot(nrow(comp) == 48)
stopifnot(all(c("sample", "condition", "celltype", "n_cells", "total_cells", "fraction") %in% colnames(comp)))
comp_pop <- comp[comp$total_cells > 0, ]
agg <- aggregate(fraction ~ sample + condition, data = comp_pop, FUN = sum)
stopifnot(all(abs(agg$fraction - 1) < 0.001))
message("PASS: 48 rows, fraction sums = 1")

# ---- Test 2: prepare_composition_data from metadata (with condition) ----
message("\n=== Test 2: prepare_composition_data (metadata, with condition) ===")
comp_meta <- prepare_composition_data(
  meta_data     = meta,
  sample_col    = "orig.ident",
  celltype_col  = "cell_type",
  condition_col = "condition"
)
stopifnot(identical(nrow(comp_meta), nrow(comp)))
stopifnot(identical(colnames(comp_meta), colnames(comp)))
stopifnot(all(comp_meta$fraction == comp$fraction))
message("PASS: metadata output identical to Seurat output")

# ---- Test 3: prepare_composition_data (metadata only, no condition) ----
message("\n=== Test 3: prepare_composition_data (metadata, no condition) ===")
comp_nocond <- prepare_composition_data(
  meta_data    = meta,
  sample_col   = "orig.ident",
  celltype_col = "cell_type"
)
stopifnot(!"condition" %in% names(comp_nocond))
stopifnot(ncol(comp_nocond) == 5)
stopifnot(nrow(comp_nocond) == 24)  # 4 samples × 6 celltypes
agg2 <- aggregate(fraction ~ sample, data = comp_nocond, FUN = sum)
stopifnot(all(abs(agg2$fraction - 1) < 0.001))
message("PASS: 24 rows, no condition column, fraction sums = 1")

# ---- Test 4: build_summary ----
message("\n=== Test 4: build_summary ===")
s <- build_summary(comp, condition_col = "condition")
stopifnot(s$total_cells == n_cells)
stopifnot(s$n_samples == 4)
stopifnot(s$n_celltypes == 6)
stopifnot(s$n_conditions == 2)
message("PASS: summary stats correct")

# ---- Test 5: plots ----
message("\n=== Test 5: plots ===")
colors <- celltype_color_map(as.character(levels(comp$celltype)))
p_sample <- plot_sample_composition(comp, colors)
stopifnot(inherits(p_sample, "plotly"))
p_cond <- plot_condition_composition(comp, colors)
stopifnot(inherits(p_cond, "plotly"))
p_ct <- plot_celltype_fraction(comp, colors)
stopifnot(inherits(p_ct, "plotly"))
message("PASS: all 3 plot types generated")

# ---- Test 6: sccomp_report from Seurat (with condition) ----
message("\n=== Test 6: sccomp_report (Seurat, with condition) ===")
out <- sccomp_report(
  seurat_obj    = obj,
  sample_col    = "orig.ident",
  celltype_col  = "cell_type",
  condition_col = "condition",
  output        = file.path(pkg_root, "test_report.html"),
  title         = "scReportComposition - Smoke Test"
)
stopifnot(file.exists(out))
message("Report: ", basename(out), " (", file.info(out)$size, " bytes)")

# ---- Test 7: sccomp_report from Seurat (no condition) ----
message("\n=== Test 7: sccomp_report (Seurat, no condition) ===")
out2 <- sccomp_report(
  seurat_obj    = obj,
  sample_col    = "orig.ident",
  celltype_col  = "cell_type",
  output        = file.path(pkg_root, "test_report_nocond.html"),
  title         = "scReportComposition - No Condition"
)
stopifnot(file.exists(out2))
# Condition section should be absent
html_lines <- readLines(out2, warn = FALSE)
stopifnot(!any(grepl("Condition Composition", html_lines)))
message("Report: ", basename(out2), " (", file.info(out2)$size, " bytes) — condition section hidden")

# ---- Test 8: sccomp_report from metadata (with condition) ----
message("\n=== Test 8: sccomp_report (metadata, with condition) ===")
out3 <- sccomp_report(
  meta_data     = meta,
  sample_col    = "orig.ident",
  celltype_col  = "cell_type",
  condition_col = "condition",
  output        = file.path(pkg_root, "test_report_metadata.html"),
  title         = "scReportComposition - Metadata Only"
)
stopifnot(file.exists(out3))
message("Report: ", basename(out3), " (", file.info(out3)$size, " bytes)")

# ---- Test 9: sccomp_report from metadata (no condition) ----
message("\n=== Test 9: sccomp_report (metadata, no condition) ===")
out4 <- sccomp_report(
  meta_data     = meta,
  sample_col    = "orig.ident",
  celltype_col  = "cell_type",
  output        = file.path(pkg_root, "test_report_metadata_nocond.html"),
  title         = "scReportComposition - Metadata No Condition"
)
stopifnot(file.exists(out4))
html_lines4 <- readLines(out4, warn = FALSE)
stopifnot(!any(grepl("Condition Composition", html_lines4)))
message("Report: ", basename(out4), " (", file.info(out4)$size, " bytes) — condition section hidden")

# ---- Test 10: plot_condition_composition returns NULL without condition ----
message("\n=== Test 10: plot_condition_composition(NULL) ===")
p_null <- plot_condition_composition(comp_nocond, colors)
stopifnot(is.null(p_null))
message("PASS: returns NULL correctly")

# ---- Test 11: error on missing column ----
message("\n=== Test 11: error on missing column ===")
err <- tryCatch(
  prepare_composition_data(
    meta_data    = meta,
    sample_col   = "nonexistent",
    celltype_col = "cell_type"
  ),
  error = function(e) e
)
stopifnot(inherits(err, "error"))
message("PASS: error — ", conditionMessage(err))

# ---- Test 12: error when neither seurat_obj nor meta_data provided ----
message("\n=== Test 12: error when no data source ===")
err2 <- tryCatch(
  prepare_composition_data(
    sample_col   = "sample",
    celltype_col = "cell_type"
  ),
  error = function(e) e
)
stopifnot(inherits(err2, "error"))
message("PASS: error — ", conditionMessage(err2))

# ---- Test 13: meta_data must be a data.frame ----
message("\n=== Test 13: meta_data must be a data.frame ===")
err3 <- tryCatch(
  prepare_composition_data(
    meta_data    = "not_a_df",
    sample_col   = "sample",
    celltype_col = "cell_type"
  ),
  error = function(e) e
)
stopifnot(inherits(err3, "error"))
message("PASS: error — ", conditionMessage(err3))

# ---- Test 14: no NAs in output ----
message("\n=== Test 14: no NA values in output ===")
stopifnot(!anyNA(comp$fraction))
stopifnot(!anyNA(comp$n_cells))
stopifnot(!anyNA(comp$total_cells))
message("PASS: zero NAs across all columns")

# ---- Test 15: meta_data takes precedence over seurat_obj ----
message("\n=== Test 15: meta_data takes precedence ===")
# Create a meta with different cell counts by duplicating rows
meta_alt <- rbind(meta, meta)
comp_prec <- prepare_composition_data(
  seurat_obj    = obj,       # 2000 cells
  meta_data     = meta_alt,  # 4000 cells (duplicated) → should win
  sample_col    = "orig.ident",
  celltype_col  = "cell_type",
  condition_col = "condition"
)
stopifnot(sum(comp_prec$n_cells) == 4000)
message("PASS: meta_data (4000 cells) took precedence over seurat_obj (2000 cells)")

message("\n=== ALL 15 TESTS PASSED ===")
message("Output files:")
message("  test_report.html")
message("  test_report_nocond.html")
message("  test_report_metadata.html")
message("  test_report_metadata_nocond.html")
