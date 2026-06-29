# scReportComposition

<p align="center">
  <strong>A full-featured cell composition reporting module for single-cell bioinformatics.</strong>
</p>

<p align="center"><img src="https://img.shields.io/badge/Version-v0.1.0-blue" alt="Version"> <img src="https://img.shields.io/badge/Status-Functional-success" alt="Status"> <img src="https://img.shields.io/badge/Layer-scReport%20Module-lightgrey" alt="Layer"> <img src="https://img.shields.io/badge/Focus-Cell%20Composition-purple" alt="Focus"> <a href="https://doi.org/10.5281/zenodo.20955461"><img src="https://zenodo.org/badge/1280164558.svg" alt="DOI"></a> <img src="https://img.shields.io/badge/License-MIT-yellow" alt="License"></p>

## Overview

**scReportComposition** generates a self-contained interactive HTML report for cell composition analysis from single-cell metadata.

> Build a composition table → generate a full interactive report with navigation.

**Key principle:** No statistical analysis. This package takes pre-computed annotations and visualises composition — no propeller, no scCODA, no GLMM.

## Report Modules

| # | Module | Description |
|---|---|---|
| 1 | Overview | Summary cards: total cells, samples, cell types, conditions, cells/samples per condition |
| 2 | Sample Composition | Cell counts + fractions per sample (stacked bars) |
| 3 | Condition Composition | Cell counts + fractions per condition (hidden if no condition) |
| 4 | Cell Type Distribution | Fraction + count boxplots by cell type × condition |
| 5 | Heatmaps | Sample × cell type + Condition × cell type fraction heatmaps |
| 6 | Composition Table | Full scrollable composition table |

The report uses a left-side navigation bar to switch between modules. All plots are Plotly-interactive (hover, zoom, pan, download).

## Quick Start

```r
library(scReportComposition)
library(Seurat)

obj <- readRDS("my_seurat.rds")

sccomp_report(
  obj,
  sample_col   = "orig.ident",
  celltype_col = "cell_type",
  condition_col = "condition"
)
```

**Without Seurat** — pass a plain data.frame:

```r
meta <- read.csv("cell_metadata.csv")
sccomp_report(
  meta_data    = meta,
  sample_col   = "sample",
  celltype_col = "cell_type",
  condition_col = "condition"
)
```

## API

```r
sccomp_report(
  seurat_obj   = NULL,
  meta_data    = NULL,
  sample_col,          # column name for sample/origin
  celltype_col,        # column name for cell type annotation
  condition_col = NULL, # optional condition column
  output = "scReportComposition.html",
  title  = "scReportComposition"
)
```

## Internal Functions

| Function | Purpose |
|---|---|
| `prepare_composition_data()` | Build standardised composition table |
| `build_summary()` | Compute summary statistics |
| `summarise_condition_composition()` | Aggregate to condition level |
| `build_composition_matrix()` | Pivot to sample × celltype matrix |
| `plot_sample_count_composition()` | Stacked bar — sample cell counts |
| `plot_sample_composition()` | Stacked bar — sample fractions |
| `plot_condition_count_composition()` | Stacked bar — condition cell counts |
| `plot_condition_composition()` | Stacked bar — condition fractions |
| `plot_celltype_fraction_by_condition()` | Boxplot — fraction by condition |
| `plot_celltype_count_by_condition()` | Boxplot — count by condition |
| `plot_sample_celltype_heatmap()` | Heatmap — sample × celltype |
| `plot_condition_celltype_heatmap()` | Heatmap — condition × celltype |
| `render_composition_table()` | HTML composition table |
| `build_html()` | Assemble self-contained HTML |
| `sccomp_report()` | Orchestrator |

## Input

Seurat object or data.frame with metadata columns:
- Sample identifier (e.g. `orig.ident`, `sample`, `patient`)
- Cell-type annotation (e.g. `cell_type`, `cluster`)
- Optional condition (e.g. `condition`, `group`, `treatment`)

No columns are hardcoded — the user specifies them.

## Out of Scope

- Statistical testing (propeller, scCODA, GLMM, Beta/Dirichlet regression)
- Differential composition analysis
- P-values, significance markers
- Machine learning or AI-based interpretation
- UMAP, PCA, marker genes, gene expression — these belong to scReportLite

## Design Principles

- Report after analysis — no re-computation
- Self-contained single HTML file
- Table-driven visualisation
- Left navigation, section-based content
- Same green theme as scReportLite

## Citation

[![DOI](https://zenodo.org/badge/1280164558.svg)](https://doi.org/10.5281/zenodo.20955461)

```text
10.5281/zenodo.20955461
```

## License

MIT
