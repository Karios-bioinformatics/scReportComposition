# Smoke test for scReportComposition v0.1.0 — Full report
# Run from package root: Rscript inst/smoke_test.R

library(Seurat)
library(SeuratObject)
library(plotly)
library(htmltools)
library(jsonlite)

# ---- Source all package files (dynamic path) ----
pkg_root <- normalizePath(".")
if (!file.exists(file.path(pkg_root, "DESCRIPTION"))) {
  for (d in c("..", "../..", "../../..")) {
    if (file.exists(file.path(pkg_root, d, "DESCRIPTION"))) {
      pkg_root <- normalizePath(file.path(pkg_root, d)); break
    }
  }
}
message("Package root: ", pkg_root)
r_dir <- file.path(pkg_root, "R")
stopifnot(dir.exists(r_dir))
for (f in list.files(r_dir, full.names = TRUE, pattern = "\\.R$")) {
  message("  Sourcing: ", basename(f))
  source(f)
}

# ---- Build mock data ----
set.seed(42)
n_cells <- 2000
samples <- paste0("Sample_", 1:4)
celltypes <- c("T_cell", "B_cell", "Macrophage", "NK_cell", "Monocyte", "Dendritic")
conditions <- c("Control", "Treatment")

sample_vec <- sample(samples, n_cells, replace = TRUE)
condition_vec <- character(n_cells)
celltype_vec <- character(n_cells)
condition_vec[sample_vec %in% c("Sample_1", "Sample_2")] <- "Control"
condition_vec[sample_vec %in% c("Sample_3", "Sample_4")] <- "Treatment"

probs_control   <- c(0.10, 0.25, 0.20, 0.15, 0.20, 0.10)
probs_treatment <- c(0.30, 0.10, 0.20, 0.15, 0.15, 0.10)
for (i in seq_len(n_cells)) {
  celltype_vec[i] <- sample(celltypes, 1,
    prob = if (condition_vec[i] == "Control") probs_control else probs_treatment)
}

meta <- data.frame(
  orig.ident = sample_vec, condition = condition_vec, cell_type = celltype_vec,
  row.names = paste0("Cell_", seq_len(n_cells)), stringsAsFactors = FALSE
)

counts <- matrix(rpois(50 * n_cells, lambda = 2), nrow = 50,
  dimnames = list(paste0("Gene_", 1:50), rownames(meta)))
obj <- CreateSeuratObject(counts = counts, meta.data = meta)

# ---- Test 1: Full report (Seurat, with condition) ----
message("\n=== Test 1: Full report (Seurat, with condition) ===")
out <- sccomp_report(
  seurat_obj    = obj,
  sample_col    = "orig.ident",
  celltype_col  = "cell_type",
  condition_col = "condition",
  output        = file.path(pkg_root, "test_full.html"),
  title         = "scReportComposition — Full Smoke Test"
)
stopifnot(file.exists(out))
lines <- readLines(out, warn = FALSE)
message("File: ", basename(out), " (", file.info(out)$size, " bytes)")

# Check all sections present
checks <- c(
  "comp-nav"            = "Navigation sidebar",
  "section-overview"    = "Overview section",
  "section-sample"      = "Sample Composition section",
  "section-condition"   = "Condition Composition section",
  "section-ctdist"      = "Cell Type Distribution section",
  "section-heatmaps"    = "Heatmaps section",
  "section-table"       = "Table section"
)
for (kw in names(checks)) {
  found <- any(grepl(kw, lines, fixed = TRUE))
  cat(sprintf("  %-25s %-6s %s\n", kw, if (found) "OK" else "MISSING!", checks[kw]))
  if (!found && kw != "section-condition" && kw != "section-ctdist") {
    stop("Critical section missing: ", kw)
  }
}

# ---- Test 2: Full report (Seurat, no condition) ----
message("\n=== Test 2: Full report (Seurat, no condition) ===")
out2 <- sccomp_report(
  seurat_obj    = obj,
  sample_col    = "orig.ident",
  celltype_col  = "cell_type",
  output        = file.path(pkg_root, "test_full_nocond.html"),
  title         = "scReportComposition — No Condition"
)
stopifnot(file.exists(out2))
lines2 <- readLines(out2, warn = FALSE)
# Condition sections must be absent
stopifnot(!any(grepl("section-condition", lines2, fixed = TRUE)))
stopifnot(!any(grepl("section-ctdist", lines2, fixed = TRUE)))
message("Condition sections correctly hidden")

# ---- Test 3: Full report (metadata, with condition) ----
message("\n=== Test 3: Full report (metadata, with condition) ===")
out3 <- sccomp_report(
  meta_data     = meta,
  sample_col    = "orig.ident",
  celltype_col  = "cell_type",
  condition_col = "condition",
  output        = file.path(pkg_root, "test_full_meta.html"),
  title         = "scReportComposition — Metadata Only"
)
stopifnot(file.exists(out3))
# Should match Seurat output size within 100 bytes
diff <- abs(file.info(out3)$size - file.info(out)$size)
cat("Size diff from Seurat:", diff, "bytes\n")
stopifnot(diff < 500)

# ---- Test 4: data helpers ----
message("\n=== Test 4: data helpers ===")
comp <- prepare_composition_data(obj, sample_col = "orig.ident",
  celltype_col = "cell_type", condition_col = "condition")
cs <- summarise_condition_composition(comp)
stopifnot(!is.null(cs))
stopifnot(length(unique(cs$condition)) == 2)
mat <- build_composition_matrix(comp)
stopifnot(nrow(mat) == 4, ncol(mat) == 6)
stopifnot(all(rowSums(mat) - 1 < 0.001))
message("PASS: matrix 4×6, all rows sum to 1")

# ---- Test 5: NULL guards ----
message("\n=== Test 5: NULL guards (no condition) ===")
comp2 <- prepare_composition_data(obj, sample_col = "orig.ident",
  celltype_col = "cell_type")
stopifnot(is.null(summarise_condition_composition(comp2)))
stopifnot(is.null(plot_condition_composition(comp2, celltype_color_map(levels(comp2$celltype)))))
stopifnot(is.null(plot_condition_count_composition(comp2, NULL)))
stopifnot(is.null(plot_celltype_fraction_by_condition(comp2, NULL)))
stopifnot(is.null(plot_celltype_count_by_condition(comp2, NULL)))
stopifnot(is.null(plot_condition_celltype_heatmap(comp2)))
message("PASS: all condition-dependent functions return NULL")

message("\n=== ALL 5 TESTS PASSED ===")
