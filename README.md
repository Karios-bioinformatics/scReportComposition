# scReportComposition

<p align="center">
  <strong>Descriptive composition audit reports for single-cell metadata.</strong>
</p>

<p align="center"><img src="https://img.shields.io/badge/Version-v0.2.0-blue" alt="Version"> <img src="https://img.shields.io/badge/Status-MVP%20candidate-success" alt="Status"> <img src="https://img.shields.io/badge/Layer-scReport%20Module-lightgrey" alt="Layer"> <img src="https://img.shields.io/badge/Focus-Composition%20Audit-purple" alt="Focus"> <a href="https://doi.org/10.5281/zenodo.21278329"><img src="https://zenodo.org/badge/DOI/10.5281/zenodo.21278329.svg" alt="DOI"></a> <img src="https://img.shields.io/badge/License-MIT-yellow" alt="License"></p>

## Overview

`scReportComposition` generates an interactive HTML audit report for cell identity composition in single-cell data.

The current v0.2.0 workflow is centered on `build_screport_composition()`. It summarizes sample-level and group-level composition, highlights sample dominance patterns, and reports automated data-quality warnings. It is descriptive only: no differential composition statistics, p-values, or causal claims are performed.

The older `sccomp_report()` viewer remains exported for compatibility, but new reports should use `build_screport_composition()`.

## What The Report Shows

| Module | Purpose |
|---|---|
| Overview | Total cells, samples, groups, identities, selected metadata columns, and report parameters |
| Warnings | Automated audit flags such as low sample size, low identity counts, group size below two samples, sample dominance, and possible batch/group confounding |
| Metadata Audit | Intermediate tables for sample totals, group mapping, and identity counts |
| Sample-Level Composition | Identity composition by sample, shown as interactive Plotly summaries |
| Group-Level Descriptive Composition | Group-level descriptive summaries without inferential testing |
| Sample Dominance / Outlier | Per-identity sample contribution and dominance checks |
| Tables / Methods | Intermediate tables and generation metadata for traceability |

All plots are Plotly-interactive, with hover, zoom, pan, and download support.

## Quick Start

```r
library(scReportComposition)
library(Seurat)

obj <- readRDS("my_seurat.rds")

res <- build_screport_composition(
  seurat_obj   = obj,
  sample_col   = "sample",
  group_col    = "condition",
  cluster_col  = "seurat_clusters",
  batch_col    = "batch",
  output_file  = "composition_audit.html",
  title        = "Composition Audit Report"
)
```

The return value includes the standardized metadata, count/proportion tables, dominance table, warning table, Plotly widgets, and the output HTML path.

```r
names(res)
# meta, count_table, prop_table, sample_total, group_summary,
# identity_sample_contribution, dominance_table, warning_table,
# plots, output_file
```

## Main API

```r
build_screport_composition(
  seurat_obj,
  sample_col,
  group_col,
  cluster_col    = NULL,
  celltype_col   = NULL,
  batch_col      = NULL,
  identity_col   = NULL,
  output_file    = "scReport_composition_audit.html",
  out_dir        = ".",
  title          = "scReportComposition Audit Report",
  min_cells_per_sample   = 500,
  min_cells_per_identity = 50,
  dominance_threshold    = 0.8,
  top_n_identity = NULL,
  interactive    = TRUE
)
```

### Identity Column Selection

The identity column is resolved from these inputs:

1. `identity_col`
2. `celltype_col`
3. `cluster_col`
4. Seurat identities, when available

Use `cluster_col` for cluster-level composition and `celltype_col` for annotation-level composition.

### About `top_n_identity`

`top_n_identity` is currently accepted but not implemented. All identities are retained.

The reason is semantic: simply dropping non-top identities would change sample denominators and make sample-level proportions misleading. A future implementation should merge non-top identities into an `Other` category so totals remain valid.

## Warning Logic

The audit warning table is descriptive. It can flag:

- Missing metadata values
- Samples mapped to multiple groups
- Low total cells per sample
- Low total cells per identity
- Groups with fewer than two samples
- Identities dominated by one sample
- Possible group/batch confounding
- Global descriptive-only warnings when inference would be inappropriate

These warnings are not test results. They are prompts for quality control and interpretation.

## Legacy Viewer

`sccomp_report()` is still exported for the original composition viewer workflow. It accepts a Seurat object or metadata table and produces a simpler cell composition report.

For v0.2.0 audit reports, prefer `build_screport_composition()`.

## Input Requirements

The primary workflow expects a Seurat object with metadata columns for:

- Sample identifier, such as `sample`, `orig.ident`, or `patient`
- Group or condition, such as `condition`, `group`, or `treatment`
- Identity annotation, such as `seurat_clusters`, `cell_type`, or `annotation`
- Optional batch column

No metadata column names are hardcoded; pass the column names explicitly.

## Out Of Scope

- Differential composition testing
- p-values or significance markers
- propeller, scCODA, GLMM, beta regression, or Dirichlet regression
- Gene expression, marker genes, PCA, UMAP, and feature diagnostics

Use `scReportLite` for expression/embedding reports and `scReportDE` for differential expression reports.

## Design Principles

- Audit after analysis, without recomputing identities
- Keep all composition summaries descriptive
- Preserve denominators and make sample-level proportions interpretable
- Surface warnings when sample/group structure limits interpretation
- Keep the report lightweight and traceable through intermediate tables

## Citation

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21278329.svg)](https://doi.org/10.5281/zenodo.21278329)

```text
10.5281/zenodo.21278329
```

## License

MIT
