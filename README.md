# scReportComposition

<p align="center"> <strong>A lightweight table-driven composition reporting module for single-cell bioinformatics.</strong> </p>

<p align="center"> <img src="https://img.shields.io/badge/Status-Early%20Development-blue" alt="Status"> <img src="https://img.shields.io/badge/Focus-Cell%20Composition-green" alt="Focus"> <img src="https://img.shields.io/badge/Type-Interactive%20Report-lightgrey" alt="Type"> <img src="https://img.shields.io/badge/License-MIT-yellow" alt="License"> </p>

## Overview

`scReportComposition` is a lightweight reporting module in the **scReport** ecosystem.

It focuses on the visualization and organization of cell composition results in single-cell bioinformatics. The core idea is simple:

> Convert cell-level metadata into a sample-level composition table, then generate an interactive report for composition exploration.

Unlike cell-level visualization modules that render thousands or millions of single-cell points, `scReportComposition` is designed to be **table-driven**. Its main input is a summarized composition table, making it lightweight, fast, and suitable for static HTML reporting.

## Position in the scReport ecosystem

`scReportComposition` is one module of the broader `scReport` ecosystem.

The long-term goal of `scReport` is not only to connect different stage-specific reports, but also to support **cell-centric global tracking** across analysis modules.

In this design:

- `scReportLite` focuses on cell-level views such as QC, PCA, UMAP, feature plots, and marker tables.
- `scReportComposition` focuses on sample-level and group-level cell composition.
- Future modules may cover differential expression, enrichment analysis, trajectory analysis, cell communication, and other downstream analyses.

`scReportComposition` is therefore intended to answer questions such as:

- What cell types are present in each sample?
- What proportion of each sample is made up of a given cell type?
- Which cell types dominate specific samples?
- How does composition change across groups or conditions?
- Which composition patterns should be linked back to cell-level views in the full scReport system?

## Core concept

The central object of `scReportComposition` is the composition table.

A typical composition table contains:

|sample|group|cell_type|cell_count|total_cells|proportion|percent|
|---|---|---|---|---|---|---|
|Sample_1|Control|T cell|1200|5000|0.240|24.0|
|Sample_1|Control|Macrophage|800|5000|0.160|16.0|
|Sample_2|Disease|T cell|900|4800|0.188|18.8|

The `group` column is optional in early versions. When group information is unavailable, the report focuses on sample-level composition.

## Planned v0.1.0 scope

The first version of `scReportComposition` focuses on **sample-level composition reporting**.

Planned features for `v0.1.0`:

- Build a composition table from cell-level metadata.
- Summarize cell counts and proportions by sample and annotation.
- Generate a single-page HTML composition report.
- Provide stacked bar plots for sample-level composition.
- Provide heatmap views of cell type proportions.
- Provide bubble plots showing both cell count and proportion.
- Provide total cell count summaries by sample.
- Provide total cell count summaries by cell type.
- Provide interactive composition tables.

## Out of scope for v0.1.0

The following features are intentionally not included in the first version:

- Differential composition testing.
- Group-level statistical comparison.
- scCODA, propeller, Milo, or beta-binomial modeling.
- UMAP rendering.
- Large-scale single-cell point rendering.
- Cross-module cell locking.
- Full integration with the main `scReport` package.

These features may be added in future versions.

## Input data

`scReportComposition` is designed to work with cell-level metadata such as:

|   |   |   |   |
|---|---|---|---|
|cell_id|sample|cluster|cell_type|
|Cell_001|Sample_1|0|T cell|
|Cell_002|Sample_1|1|Macrophage|
|Cell_003|Sample_2|0|T cell|

Minimum required columns:

- `sample`
- `cell_type` or another annotation column

Optional columns:

- `cell_id`
- `cluster`
- `group`
- `condition`
- `annotation_level1`
- `annotation_level2`

## Planned usage

The planned core workflow is:

```
library(scReportComposition)

composition_result <- build_composition_table(
  meta_df,
  sample_col = "sample",
  annotation_col = "cell_type"
)

build_composition_report(
  composition_result,
  output = "scReportComposition.html"
)
```

Or directly:

```
build_composition_report(
  meta_df = meta_df,
  sample_col = "sample",
  annotation_col = "cell_type",
  output = "scReportComposition.html"
)
```

## Example report panels

A typical `scReportComposition` report may include:

1. Overview summary cards
2. Sample-level stacked bar plot
3. Cell type proportion heatmap
4. Cell type composition bubble plot
5. Total cells by sample
6. Total cells by cell type
7. Composition data table
8. Optional annotation mapping table

## Design principles

`scReportComposition` follows the general design principles of the scReport ecosystem:

- Reporting after analysis, not replacing analysis.
- Lightweight static HTML output.
- Clear separation between analysis results and report generation.
- Table-driven visualization.
- Modular design.
- Compatibility with future cell-centric global tracking in scReport.

## Roadmap

### v0.1.0

Sample-level cell composition report.

### v0.2.0

Group-level composition comparison.

Potential additions:

- Group mean composition stacked bar plot.
- Cell type proportion boxplot by group.
- Delta composition plot.

### v0.3.0

Differential composition result display.

Potential additions:

- External statistical result table support.
- Volcano-like differential composition plot.
- Significant cell type summary.

### Future

Integration with the main `scReport` ecosystem.

Potential directions:

- Cell-centric global tracking.
- Linking selected cells to composition bins.
- Connecting composition results with UMAP, PCA, DE, enrichment, and trajectory modules.

## Relationship to scReport

`scReportComposition` is developed as part of the broader `scReport` ecosystem.

The broader vision of `scReport` is to build an interactive reporting system for single-cell bioinformatics results, where a cell can keep its identity across different analysis stages.

A guiding principle is:

> A cell should not lose its identity when the user moves across analysis modules.

## Citation

If you use `scReportComposition`, please cite the corresponding Zenodo record.

DOI:

```
To be added after release.
```

## License

This project is released under the MIT License.