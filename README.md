# scReportComposition

<p align="center">
  <strong>A lightweight, table-driven cell composition reporting module for single-cell bioinformatics.</strong>
</p>

<p align="center">
 <img src="https://img.shields.io/badge/Version-v0.1.0-blue" alt="Version">
 <img src="https://img.shields.io/badge/Status-Functional%20Alpha-success" alt="Status">
 <img src="https://img.shields.io/badge/Layer-scReport%20Module-lightgrey" alt="Layer">
 <img src="https://img.shields.io/badge/Focus-Cell%20Composition-purple" alt="Focus">
 <a href="https://doi.org/10.5281/zenodo.20955461"><img src="https://zenodo.org/badge/1280164558.svg" alt="DOI"></a><img src="https://img.shields.io/badge/License-MIT-yellow" alt="License"></p>

## Overview

**scReportComposition** is a lightweight reporting module in the **scReport** ecosystem.

It focuses on the visualisation and organisation of cell composition results in single-cell bioinformatics. The core idea is simple:

> Convert cell-level metadata into a sample-level composition table, then generate an interactive report for composition exploration.

**Key design principle:** scReportComposition does **not** perform statistical analysis. It takes pre-computed cell-type annotations and produces an interactive, shareable HTML report. Statistical methods (propeller, scCODA, GLMM, etc.) are planned for future versions.

## v0.1.0 — Composition Visualisation

The first functional release focuses exclusively on **composition visualisation**.

### Features

- **Composition Table** — standardised tidy data structure (sample × celltype × condition → n_cells, fraction)
- **Summary Cards** — total cells, samples, cell types, conditions, cells per sample, cell types detected
- **Sample Composition** — interactive stacked bar plot (Plotly: hover, legend, zoom, download)
- **Condition Composition** — stacked bar by condition (auto-hidden when no condition column)
- **Cell-Type Fraction** — boxplot + jitter distribution per cell type
- **Self-contained HTML** — single file, no server, no Shiny, no database

### Quick Start

```r
library(scReportComposition)
library(Seurat)

obj <- readRDS("my_seurat.rds")

sccomp_report(
  obj,
  sample_col   = "orig.ident",
  celltype_col = "cell_type",
  condition_col = "condition"    # optional — omit if no conditions
)
```

This produces `scReportComposition.html` — open it in any browser.

**Without Seurat** — pass a plain data.frame of cell-level metadata:

```r
meta <- read.csv("cell_metadata.csv")  # columns: sample, cell_type, condition

sccomp_report(
  meta_data    = meta,
  sample_col   = "sample",
  celltype_col = "cell_type",
  condition_col = "condition"
)
```

### API

```r
sccomp_report(
  seurat_obj,
  sample_col,       # column name for sample/origin
  celltype_col,     # column name for cell type annotation
  condition_col = NULL,  # optional condition column
  output = "scReportComposition.html",
  title  = "scReportComposition"
)
```

### Internal Functions

| Function | Purpose |
|---|---|
| `prepare_composition_data()` | Build standardised composition table from Seurat object |
| `build_summary()` | Compute summary statistics for display cards |
| `plot_sample_composition()` | Stacked bar — sample-level composition |
| `plot_condition_composition()` | Stacked bar — condition-level (NULL if no condition) |
| `plot_celltype_fraction()` | Boxplot + jitter — per-cell-type fraction distribution |
| `build_html()` | Assemble self-contained HTML with CSS + JS |
| `sccomp_report()` | Orchestrator — the only function most users need |

### Input

Seurat object with metadata columns for:
- Sample identifier (e.g. `orig.ident`, `sample`, `patient`)
- Cell-type annotation (e.g. `cell_type`, `cluster_annotation`)
- Optional condition (e.g. `condition`, `group`, `treatment`)

Columns are **not** hardcoded — the user specifies them.

### Out of Scope for v0.1.0

- Statistical testing (propeller, scCODA, GLMM, Beta regression, Dirichlet regression)
- Differential composition analysis
- P-values, significance markers
- Machine learning or AI-based interpretation
- Auto-generated conclusions or rich-text explanations

These are reserved for future versions.

### Output Report Layout

```
┌──────────────────────────────────────────────┐
│  scReportComposition  |  v0.1.0              │  ← Header
├──────────────────────────────────────────────┤
│  ┌────────┐ ┌────────┐ ┌────────┐ ┌───────┐ │
│  │ Total  │ │Samples │ │Cell    │ │Cond.  │ │  ← Summary Cards
│  │ Cells  │ │        │ │Types   │ │       │ │
│  └────────┘ └────────┘ └────────┘ └───────┘ │
├──────────────────────────────────────────────┤
│  Sample Composition (Stacked Bar)            │  ← Plotly interactive
├──────────────────────────────────────────────┤
│  Condition Composition (Stacked Bar)         │  ← Hidden if no condition
├──────────────────────────────────────────────┤
│  Cell-Type Fraction (Boxplot + Jitter)       │  ← Plotly interactive
└──────────────────────────────────────────────┘
```

## Position in the scReport Ecosystem

- **scReportLite** — cell-level QC, PCA, UMAP, feature plots, marker tables
- **scReportComposition** — sample-level / group-level cell composition (this package)
- Future modules: differential expression, enrichment, trajectory, cell communication

## Design Principles

- Report after analysis — does not re-compute
- Lightweight static HTML output
- Table-driven visualisation
- Modular, consistent with scReportLite
- One output: one self-contained HTML file

## Citation

If you use scReportComposition, please cite the corresponding Zenodo record:

[![DOI](https://zenodo.org/badge/1280164558.svg)](https://doi.org/10.5281/zenodo.20955461)

```text
10.5281/zenodo.20955461
```

## License

MIT
