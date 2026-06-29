# Smoke test for scReportComposition v0.1.0
# Creates a mock Seurat object with known composition data

library(Seurat)
library(SeuratObject)
library(plotly)
library(htmltools)
library(jsonlite)

# ---- Source all package files ----
pkg_root <- normalizePath("D:/karios-archive-vault/项目/scReportComposition")
for (f in list.files(file.path(pkg_root, "R"), full.names = TRUE, pattern = "\\.R$")) {
  message("Sourcing: ", basename(f))
  source(f)
}

# ---- Build mock Seurat object ----
set.seed(42)

n_cells   <- 2000
samples   <- paste0("Sample_", 1:4)
celltypes <- c("T_cell", "B_cell", "Macrophage", "NK_cell", "Monocyte", "Dendritic")
conditions <- c("Control", "Treatment")

sample_vec    <- sample(samples, n_cells, replace = TRUE)
condition_vec <- character(n_cells)
celltype_vec  <- character(n_cells)

# Assign conditions: samples 1-2 = Control, 3-4 = Treatment
condition_vec[sample_vec %in% c("Sample_1", "Sample_2")] <- "Control"
condition_vec[sample_vec %in% c("Sample_3", "Sample_4")] <- "Treatment"

# Assign cell types with condition bias
probs_control   <- c(0.10, 0.25, 0.20, 0.15, 0.20, 0.10)
probs_treatment <- c(0.30, 0.10, 0.20, 0.15, 0.15, 0.10)

for (i in seq_len(n_cells)) {
  if (condition_vec[i] == "Control") {
    celltype_vec[i] <- sample(celltypes, 1, prob = probs_control)
  } else {
    celltype_vec[i] <- sample(celltypes, 1, prob = probs_treatment)
  }
}

# Build metadata
meta <- data.frame(
  orig.ident = sample_vec,
  condition  = condition_vec,
  cell_type  = celltype_vec,
  row.names  = paste0("Cell_", seq_len(n_cells)),
  stringsAsFactors = FALSE
)

# Create a minimal expression matrix (50 genes × 2000 cells)
counts <- matrix(
  rpois(50 * n_cells, lambda = 2),
  nrow = 50,
  dimnames = list(paste0("Gene_", 1:50), rownames(meta))
)

obj <- CreateSeuratObject(counts = counts, meta.data = meta)
message("Mock Seurat object: ", ncol(obj), " cells, ", nrow(obj), " genes")

# ---- Test 1: prepare_composition_data (with condition) ----
message("\n=== Test 1: prepare_composition_data (with condition) ===")
comp <- prepare_composition_data(
  obj,
  sample_col    = "orig.ident",
  celltype_col  = "cell_type",
  condition_col = "condition"
)
message("Rows: ", nrow(comp))
message("Columns: ", paste(colnames(comp), collapse = ", "))
print(head(comp, 12))

# Check: fraction sums to 1 per sample×condition (only populated combos)
comp_pop <- comp[comp$total_cells > 0, ]
agg <- aggregate(fraction ~ sample + condition, data = comp_pop, FUN = sum)
message("\nFraction sum per sample×condition (populated only):")
print(agg)
stopifnot(all(abs(agg$fraction - 1) < 0.001))
message("PASS: all populated combos sum to 1")

# ---- Test 2: build_summary ----
message("\n=== Test 2: build_summary ===")
s <- build_summary(comp, condition_col = "condition")
stopifnot(s$total_cells == n_cells)
stopifnot(s$n_samples == 4)
stopifnot(s$n_celltypes == 6)
stopifnot(s$n_conditions == 2)
message("PASS: summary stats correct")

# ---- Test 3: plots ----
message("\n=== Test 3: plots ===")
colors <- celltype_color_map(as.character(levels(comp$celltype)))

p_sample <- plot_sample_composition(comp, colors)
stopifnot(inherits(p_sample, "plotly"))
message("plot_sample_composition: OK")

p_cond <- plot_condition_composition(comp, colors)
stopifnot(inherits(p_cond, "plotly"))
message("plot_condition_composition: OK")

p_ct <- plot_celltype_fraction(comp, colors)
stopifnot(inherits(p_ct, "plotly"))
message("plot_celltype_fraction: OK")

# ---- Test 4: full report (with condition) ----
message("\n=== Test 4: sccomp_report (with condition) ===")
out <- sccomp_report(
  obj,
  sample_col    = "orig.ident",
  celltype_col  = "cell_type",
  condition_col = "condition",
  output        = file.path(pkg_root, "test_report.html"),
  title         = "scReportComposition - Smoke Test"
)
stopifnot(file.exists(out))
message("Report: ", out)
message("File size: ", file.info(out)$size, " bytes")
message("PASS: report generated")

# ---- Test 5: full report (no condition → condition section hidden) ----
message("\n=== Test 5: sccomp_report (no condition) ===")
out2 <- sccomp_report(
  obj,
  sample_col    = "orig.ident",
  celltype_col  = "cell_type",
  output        = file.path(pkg_root, "test_report_nocond.html"),
  title         = "scReportComposition - No Condition"
)
stopifnot(file.exists(out2))
message("Report: ", out2)
message("File size: ", file.info(out2)$size, " bytes")
message("PASS: no-condition report generated")

# ---- Test 6: plot_condition_composition returns NULL when no condition ----
message("\n=== Test 6: plot_condition_composition(NULL) ===")
comp_nocond <- prepare_composition_data(
  obj,
  sample_col    = "orig.ident",
  celltype_col  = "cell_type"
)
p_null <- plot_condition_composition(comp_nocond, colors)
stopifnot(is.null(p_null))
message("PASS: returns NULL correctly")

# ---- Test 7: missing column error ----
message("\n=== Test 7: error on missing column ===")
err <- tryCatch(
  prepare_composition_data(obj, sample_col = "nonexistent", celltype_col = "cell_type"),
  error = function(e) e
)
stopifnot(inherits(err, "error"))
message("PASS: error on missing column — ", conditionMessage(err))

# ---- Test 8: prepare_composition_data without condition ----
message("\n=== Test 8: prepare_composition_data (no condition) ===")
comp2 <- prepare_composition_data(obj, sample_col = "orig.ident", celltype_col = "cell_type")
stopifnot(!"condition" %in% names(comp2))
stopifnot(ncol(comp2) == 5)  # sample, celltype, n_cells, total_cells, fraction
message("PASS: no condition column in output")

# ---- Test 9: no zero-division (empty combinations get 0) ----
message("\n=== Test 9: missing combinations filled with 0 ===")
stopifnot(!anyNA(comp$fraction))
stopifnot(!anyNA(comp$n_cells))
stopifnot(!anyNA(comp$total_cells))
message("PASS: no NA values in composition table")

message("\n=== ALL 9 TESTS PASSED ===")
message("Output files:")
message("  ", file.path(pkg_root, "test_report.html"))
message("  ", file.path(pkg_root, "test_report_nocond.html"))
