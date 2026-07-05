# scReportComposition: build_comp_audit_html.R — HTML Report Assembly ----------
#
# Builds a self-contained interactive HTML report with top tab navigation.
# 7 tabs: Overview | Warnings | Metadata Audit | Sample-level | Group-level |
#         Sample Dominance | Methods
#
# All plots are embedded as plotly htmlwidgets.
# Design follows scReportLite/scReportDE visual conventions.


# ---- CSS ----------------------------------------------------------------------

comp_audit_css <- function() {
'/* === scReportComposition Audit v0.2.0 === */

:root {
  --sr-accent: #00b894;
  --sr-accent-dark: #00997a;
  --sr-accent-soft: rgba(0, 184, 148, 0.08);
  --sr-accent-border: rgba(0, 184, 148, 0.3);
  --sr-border: #dfe6e9;
  --sr-text: #2d3436;
  --sr-muted: #636e72;
  --sr-light-muted: #b2bec3;
  --sr-radius-sm: 6px;
  --sr-radius-md: 10px;
  --sr-radius-lg: 14px;
  --sr-shadow: 0 1px 4px rgba(0,0,0,0.06);
  --danger: #d63031;
  --danger-bg: #fff5f5;
  --warning: #e17055;
  --warning-bg: #fff8f5;
  --medium-warn: #fdcb6e;
  --medium-bg: #fffdf5;
}

* { box-sizing: border-box; margin: 0; padding: 0; }

body {
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto,
               "Helvetica Neue", Arial, sans-serif;
  background: #f5f6fa;
  color: var(--sr-text);
  line-height: 1.6;
}

.container {
  max-width: 1140px;
  margin: 0 auto;
  padding: 0 20px 40px;
}

/* ---- Header ---- */
.report-header {
  background: linear-gradient(135deg, var(--sr-accent), var(--sr-accent-dark));
  color: #fff;
  padding: 28px 32px 22px;
  margin: 20px 0 0;
  border-radius: var(--sr-radius-lg);
}

.report-header h1 {
  font-size: 1.45em; font-weight: 700; margin-bottom: 4px;
}

.report-header .header-meta {
  font-size: 0.8em; opacity: 0.9; margin-top: 12px;
  display: flex; flex-wrap: wrap; gap: 6px 20px;
}

.report-header .header-meta span { white-space: nowrap; }

/* ---- Top Tab Navigation ---- */
.tab-nav {
  display: flex; flex-wrap: wrap; gap: 0;
  background: #fff; border-radius: var(--sr-radius-md);
  box-shadow: var(--sr-shadow); margin-top: 14px;
  overflow: hidden;
}

.tab-btn {
  flex: 1 1 auto; min-width: 90px;
  padding: 10px 14px; text-align: center;
  font-size: 0.8em; font-weight: 600; color: var(--sr-muted);
  cursor: pointer; border: none; background: transparent;
  border-bottom: 3px solid transparent;
  transition: all 0.2s; white-space: nowrap;
  user-select: none;
}

.tab-btn:hover {
  color: var(--sr-text);
  background: var(--sr-accent-soft);
}

.tab-btn.active {
  color: var(--sr-accent-dark);
  border-bottom-color: var(--sr-accent);
}

.tab-btn.has-warn {
  color: var(--danger);
}

/* ---- Tab Content ---- */
.tab-content { display: none; }

.tab-content.visible { display: block; }

/* ---- Section ---- */
.report-section {
  background: #fff; border-radius: var(--sr-radius-md);
  box-shadow: var(--sr-shadow); padding: 24px 28px; margin-top: 14px;
}

.section-title {
  font-size: 1.05em; font-weight: 700; color: var(--sr-text);
  margin-bottom: 16px; padding-bottom: 8px;
  border-bottom: 2px solid var(--sr-accent-soft);
  display: flex; align-items: center; gap: 8px;
}

.section-icon {
  display: inline-flex; align-items: center; justify-content: center;
  width: 28px; height: 28px; background: var(--sr-accent-soft);
  color: var(--sr-accent-dark); border-radius: 6px;
  font-size: 0.85em; font-weight: 700;
}

/* ---- Summary Cards ---- */
.summary-cards {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
  gap: 14px; margin-bottom: 20px;
}

.summary-card {
  background: #fff; border-radius: var(--sr-radius-md);
  padding: 16px 18px;
  box-shadow: var(--sr-shadow);
  border-top: 3px solid var(--sr-accent);
}

.summary-card-value {
  font-size: 1.7em; font-weight: 700; color: var(--sr-text); line-height: 1.1;
}

.summary-card-label {
  font-size: 0.72em; font-weight: 600; color: var(--sr-muted);
  text-transform: uppercase; letter-spacing: 0.5px; margin-top: 4px;
}

.summary-card-detail {
  font-size: 0.78em; color: var(--sr-muted); margin-top: 6px; line-height: 1.5;
  word-break: break-all;
}

/* ---- Warning Badges ---- */
.warning-summary-badges {
  display: flex; flex-wrap: wrap; gap: 10px; margin-bottom: 16px;
}

.warning-badge {
  padding: 6px 14px; border-radius: 20px; font-size: 0.78em;
  font-weight: 600; display: inline-flex; align-items: center; gap: 6px;
}

.warning-badge.severity-high {
  background: var(--danger-bg); color: var(--danger);
  border: 1px solid #fab1a0;
}
.warning-badge.severity-medium {
  background: var(--medium-bg); color: #b8860b;
  border: 1px solid #ffeaa7;
}

.descriptive-only-banner {
  background: #fff5f5; border: 1px solid #fab1a0;
  border-left: 4px solid var(--danger); color: #c0392b;
  padding: 12px 18px; border-radius: var(--sr-radius-sm);
  font-size: 0.9em; font-weight: 500; margin-bottom: 16px;
}

/* ---- Warning Table ---- */
.warning-table-wrapper {
  border: 1px solid var(--sr-border); border-radius: var(--sr-radius-sm);
  overflow: hidden;
}

.warning-table {
  width: 100%; border-collapse: collapse; font-size: 0.85em;
}

.warning-table thead { background: #f8f9fc; }

.warning-table th {
  text-align: left; padding: 10px 14px; font-weight: 600;
  color: var(--sr-muted); font-size: 0.85em;
  border-bottom: 2px solid var(--sr-border); white-space: nowrap;
}

.warning-table td {
  padding: 9px 14px; border-bottom: 1px solid #f0f1f5; vertical-align: top;
}

.warning-table tr.row-high    { background: #fffbfb; }
.warning-table tr.row-medium  { background: #fffffb; }

.sev-tag {
  display: inline-block; padding: 2px 8px; border-radius: 4px;
  font-size: 0.78em; font-weight: 700; text-transform: uppercase;
  letter-spacing: 0.3px;
}

.sev-tag.high   { background: #ffe0e0; color: #c0392b; }
.sev-tag.medium { background: #ffeaa7; color: #856404; }
.sev-tag.low    { background: #eee; color: #636e72; }

.no-warnings {
  text-align: center; color: var(--sr-muted); padding: 20px; font-style: italic;
}

/* ---- Metadata Audit ---- */
.audit-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(340px, 1fr));
  gap: 18px;
}

.audit-block {
  border: 1px solid var(--sr-border); border-radius: var(--sr-radius-sm);
  overflow: hidden;
}

.audit-block-title {
  background: #f8f9fc; padding: 8px 14px;
  font-weight: 600; font-size: 0.82em; color: var(--sr-muted);
  text-transform: uppercase; letter-spacing: 0.4px;
  border-bottom: 1px solid var(--sr-border);
}

.audit-table {
  width: 100%; border-collapse: collapse; font-size: 0.82em;
}

.audit-table th {
  text-align: left; padding: 7px 12px; border-bottom: 1px solid var(--sr-border);
  font-weight: 600; color: var(--sr-muted); font-size: 0.88em;
}

.audit-table td {
  padding: 6px 12px; border-bottom: 1px solid #f0f1f5;
}

.audit-table tbody tr:hover { background: #f8f9fc; }

/* ---- Plot Blocks ---- */
.plot-block { margin-bottom: 28px; }
.plot-block:last-child { margin-bottom: 0; }

.plot-title {
  font-size: 0.9em; font-weight: 600; color: var(--sr-text); margin-bottom: 8px;
}

.plot-body .html-widget,
.plot-body .plotly,
.plot-body .js-plotly-plot {
  width: 100% !important;
}

/* ---- Dominance Table ---- */
.dominance-table-wrapper {
  border: 1px solid var(--sr-border); border-radius: var(--sr-radius-sm);
  overflow: auto; max-height: 520px; background: #fff;
}

.dominance-table {
  width: 100%; border-collapse: collapse; font-size: 0.82em;
}

.dominance-table thead {
  position: sticky; top: 0; z-index: 1; background: #f8f9fc;
}

.dominance-table th {
  text-align: left; padding: 8px 12px; border-bottom: 2px solid var(--sr-border);
  font-weight: 600; color: var(--sr-muted); font-size: 0.85em; white-space: nowrap;
}

.dominance-table td {
  padding: 6px 12px; border-bottom: 1px solid #f0f1f5;
}

.dominance-table .cell-highlight {
  background: #fff5f5; font-weight: 600; color: var(--danger);
}

/* ---- Methods / Notes ---- */
.methods-list {
  font-size: 0.9em; line-height: 1.8;
  color: var(--sr-text); padding-left: 20px;
}

.methods-list li {
  margin-bottom: 8px;
  padding-left: 4px;
}

.methods-list li::marker {
  color: var(--sr-accent); font-weight: 700;
}

/* ---- Footer ---- */
.report-footer {
  text-align: center; padding: 20px 0 30px;
  font-size: 0.75em; color: var(--sr-light-muted);
}

/* ---- No-data ---- */
.no-data {
  color: var(--sr-light-muted); font-style: italic;
  padding: 20px 0; text-align: center;
}

/* ---- Print ---- */
@media print {
  body { background: #fff; }
  .report-section { box-shadow: none; border: 1px solid #ddd; }
  .tab-nav { display: none; }
  .tab-content { display: block !important; margin-top: 14px; }
}
'
}


# ---- JavaScript ---------------------------------------------------------------

comp_audit_js <- function() {
'
// === scReportComposition Audit v0.2.0 ===
// Top tab navigation + Plotly resize

function switchTab(tabId) {
  document.querySelectorAll(".tab-btn").forEach(function(btn) {
    btn.classList.remove("active");
  });

  var target = document.getElementById("tab-btn-" + tabId);
  if (target) target.classList.add("active");

  document.querySelectorAll(".tab-content").forEach(function(section) {
    section.classList.remove("visible");
  });

  var targetSection = document.getElementById("tab-" + tabId);
  if (targetSection) {
    targetSection.classList.add("visible");
  }

  // Resize Plotly plots in the newly visible tab
  setTimeout(function() {
    if (targetSection) {
      var plots = targetSection.querySelectorAll(".js-plotly-plot");
      plots.forEach(function(el) {
        try { Plotly.Plots.resize(el); } catch(e) {}
      });
    }
  }, 100);
}

window.addEventListener("resize", function() {
  if (!window.Plotly) return;
  var visible = document.querySelector(".tab-content.visible");
  if (!visible) return;
  var plots = visible.querySelectorAll(".js-plotly-plot");
  plots.forEach(function(el) {
    try { Plotly.Plots.resize(el); } catch(e) {}
  });
});
'
}


# ---- HTML Builders ------------------------------------------------------------

#' Build Summary Cards
#' @keywords internal
build_summary_cards <- function(params) {
  cards <- list(
    htmltools::tags$div(class = "summary-card",
      htmltools::tags$div(class = "summary-card-value", fmt_num(params$n_cells)),
      htmltools::tags$div(class = "summary-card-label", "Total Cells")
    ),
    htmltools::tags$div(class = "summary-card",
      htmltools::tags$div(class = "summary-card-value", params$n_samples),
      htmltools::tags$div(class = "summary-card-label", "Samples")
    ),
    htmltools::tags$div(class = "summary-card",
      htmltools::tags$div(class = "summary-card-value", params$n_groups),
      htmltools::tags$div(class = "summary-card-label", "Groups")
    ),
    htmltools::tags$div(class = "summary-card",
      htmltools::tags$div(class = "summary-card-value", params$n_identities),
      htmltools::tags$div(class = "summary-card-label", "Identities")
    )
  )

  # Avg cells per sample
  avg <- round(params$n_cells / params$n_samples)
  cards <- c(cards, list(
    htmltools::tags$div(class = "summary-card",
      htmltools::tags$div(class = "summary-card-value", fmt_num(avg)),
      htmltools::tags$div(class = "summary-card-label", "Avg Cells / Sample"),
      htmltools::tags$div(
        class = "summary-card-detail",
        paste0("identity: ", params$identity_col_used)
      )
    )
  ))

  cards
}


#' Build Warning Badges + Table
#' @keywords internal
build_warning_section <- function(warning_table) {
  items <- list()

  if (nrow(warning_table) == 0) {
    items[[length(items) + 1]] <- htmltools::tags$div(
      class = "no-warnings",
      "\u2714 No warnings detected. All checks passed."
    )
    return(items)
  }

  sev_counts <- table(warning_table$severity)

  badges <- list()
  if ("high" %in% names(sev_counts)) {
    badges[[length(badges) + 1]] <- htmltools::tags$span(
      class = "warning-badge severity-high",
      "\u26A0 ", sev_counts[["high"]], " high"
    )
  }
  if ("medium" %in% names(sev_counts)) {
    badges[[length(badges) + 1]] <- htmltools::tags$span(
      class = "warning-badge severity-medium",
      "\u25B2 ", sev_counts[["medium"]], " medium"
    )
  }

  # Descriptive-only banner
  has_desc <- any(warning_table$warning_type == "descriptive_only")
  if (has_desc) {
    desc_msg <- warning_table$message[
      warning_table$warning_type == "descriptive_only"
    ][1]
    items[[length(items) + 1]] <- htmltools::tags$div(
      class = "descriptive-only-banner",
      "\u26A0 ", desc_msg
    )
  }

  items[[length(items) + 1]] <- htmltools::tags$div(
    class = "warning-summary-badges", badges
  )

  # Warning table
  tbl_rows <- lapply(seq_len(nrow(warning_table)), function(i) {
    row <- warning_table[i, ]
    htmltools::tags$tr(
      class = paste0("row-", row$severity),
      htmltools::tags$td(htmltools::tags$span(
        class = paste0("sev-tag ", row$severity), row$severity
      )),
      htmltools::tags$td(row$warning_type),
      htmltools::tags$td(row$target),
      htmltools::tags$td(row$message)
    )
  })

  items[[length(items) + 1]] <- htmltools::tags$div(
    class = "warning-table-wrapper",
    htmltools::tags$table(
      class = "warning-table",
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tags$th("Severity"),
        htmltools::tags$th("Type"),
        htmltools::tags$th("Target"),
        htmltools::tags$th("Message")
      )),
      htmltools::tags$tbody(tbl_rows)
    )
  )

  items
}


#' Build Metadata Audit Section
#' @keywords internal
build_audit_html <- function(tables, batch_col = NULL) {
  sample_total <- tables$sample_total
  prop_table   <- tables$prop_table

  blocks <- list()

  # Sample totals table
  st_cols <- if ("batch" %in% names(sample_total)) {
    c("sample", "group", "batch", "total_cells")
  } else {
    c("sample", "group", "total_cells")
  }
  st_data <- sample_total[, st_cols, drop = FALSE]

  st_rows <- lapply(seq_len(nrow(st_data)), function(i) {
    cells <- lapply(st_cols, function(col) {
      val <- st_data[i, col]
      htmltools::tags$td(if (is.numeric(val)) fmt_num(val) else as.character(val))
    })
    htmltools::tags$tr(cells)
  })

  blocks[[length(blocks) + 1]] <- htmltools::tags$div(
    class = "audit-block",
    htmltools::tags$div(class = "audit-block-title",
      sprintf("Sample Totals (%d samples)", nrow(st_data))),
    htmltools::tags$table(
      class = "audit-table",
      htmltools::tags$thead(htmltools::tags$tr(
        lapply(st_cols, function(h) htmltools::tags$th(h))
      )),
      htmltools::tags$tbody(st_rows)
    )
  )

  # Group sample counts
  group_n <- stats::aggregate(
    sample ~ group,
    data = unique(prop_table[, c("sample", "group")]),
    FUN = length
  )
  names(group_n)[names(group_n) == "sample"] <- "n_samples"

  gn_rows <- lapply(seq_len(nrow(group_n)), function(i) {
    htmltools::tags$tr(
      htmltools::tags$td(as.character(group_n$group[i])),
      htmltools::tags$td(as.character(group_n$n_samples[i]))
    )
  })

  blocks[[length(blocks) + 1]] <- htmltools::tags$div(
    class = "audit-block",
    htmltools::tags$div(class = "audit-block-title", "Group Sample Counts"),
    htmltools::tags$table(
      class = "audit-table",
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tags$th("Group"), htmltools::tags$th("N Samples")
      )),
      htmltools::tags$tbody(gn_rows)
    )
  )

  # Group x Batch
  if (!is.null(batch_col) && "batch" %in% names(sample_total)) {
    gb <- unique(sample_total[, c("group", "batch"), drop = FALSE])
    gb <- gb[order(gb$group, gb$batch), ]

    gb_rows <- lapply(seq_len(nrow(gb)), function(i) {
      htmltools::tags$tr(
        htmltools::tags$td(as.character(gb$group[i])),
        htmltools::tags$td(as.character(gb$batch[i]))
      )
    })

    blocks[[length(blocks) + 1]] <- htmltools::tags$div(
      class = "audit-block",
      htmltools::tags$div(class = "audit-block-title",
                          "Group \u00d7 Batch Mapping"),
      htmltools::tags$table(
        class = "audit-table",
        htmltools::tags$thead(htmltools::tags$tr(
          htmltools::tags$th("Group"), htmltools::tags$th("Batch")
        )),
        htmltools::tags$tbody(gb_rows)
      )
    )
  }

  htmltools::tags$div(class = "audit-grid", blocks)
}


#' Build Dominance HTML Table
#' @keywords internal
build_dominance_html_table <- function(dominance_table, dominance_threshold = 0.8) {
  rows <- lapply(seq_len(nrow(dominance_table)), function(i) {
    row <- dominance_table[i, ]
    is_dom <- row$max_sample_contribution >= dominance_threshold
    pct <- sprintf("%.1f%%", row$max_sample_contribution * 100)

    htmltools::tags$tr(
      htmltools::tags$td(row$identity),
      htmltools::tags$td(row$dominant_sample),
      htmltools::tags$td(row$dominant_group),
      htmltools::tags$td(fmt_num(row$dominant_n_cells)),
      htmltools::tags$td(fmt_num(row$identity_total_cells)),
      htmltools::tags$td(
        class = if (is_dom) "cell-highlight" else "",
        pct
      )
    )
  })

  htmltools::tags$div(
    class = "dominance-table-wrapper",
    htmltools::tags$table(
      class = "dominance-table",
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tags$th("Identity"),
        htmltools::tags$th("Dominant Sample"),
        htmltools::tags$th("Dominant Group"),
        htmltools::tags$th("Dominant N Cells"),
        htmltools::tags$th("Identity Total Cells"),
        htmltools::tags$th("Max Contribution")
      )),
      htmltools::tags$tbody(rows)
    )
  )
}


#' Build the 5 Methods / Notes bullets
#' @keywords internal
build_methods_section <- function() {
  methods <- c(
    "Proportion is calculated within each sample (n_cells / total_cells).",
    "Group-level summary is the mean of sample-level proportions. Cells are not treated as independent biological replicates.",
    "Groups with fewer than 2 samples are descriptive only. No inferential differential composition (p-values, t-test, Wilcoxon) is performed.",
    "Sample dominance indicates sample-specific composition patterns and requires further QC, batch, and marker validation. It is not automatically interpreted as a batch effect.",
    "This report is a descriptive composition audit tool. It does not replace formal differential composition analysis (e.g., scCODA, Milo, Dirichlet-multinomial models)."
  )

  htmltools::tags$ul(
    class = "methods-list",
    lapply(methods, function(m) htmltools::tags$li(m))
  )
}


# ---- Main HTML Assembly -------------------------------------------------------

#' Assemble the Full Composition Audit HTML Report
#'
#' @param output Path to output HTML file
#' @param title  Report title
#' @param params Named list of report parameters
#' @param warning_table data.frame of warnings
#' @param tables  Named list of intermediate tables
#' @param plots   Named list of plotly htmlwidgets
#' @param batch_col Optional batch column name
#' @return Invisibly, the output path
#' @keywords internal
build_comp_audit_html <- function(output, title, params,
                                   warning_table, tables, plots,
                                   batch_col = NULL) {

  n_warn <- nrow(warning_table)
  n_high <- sum(warning_table$severity == "high")

  # ---- Tab buttons ----
  tab_btn <- function(id, label, has_warn = FALSE) {
    htmltools::tags$button(
      class = paste("tab-btn", if (has_warn) "has-warn" else ""),
      id    = paste0("tab-btn-", id),
      onclick = paste0("switchTab(", shQuote(id), ")"),
      label
    )
  }

  # ---- Tab 1: Overview ----
  overview_tab <- htmltools::tags$div(
    class = "tab-content",
    id    = "tab-overview",
    htmltools::tags$div(
      class = "report-section",
      htmltools::tags$div(class = "section-title",
        htmltools::tags$span(class = "section-icon", "\u2139"),
        "Overview"
      ),
      htmltools::tags$div(class = "summary-cards",
                           build_summary_cards(params)),
      htmltools::tags$div(
        class = "summary-card-detail",
        style = "background:#fff; padding:14px 18px; border-radius:8px;
                 box-shadow:var(--sr-shadow); margin-top:10px; font-size:0.88em;",
        htmltools::tags$strong("Columns used: "),
        sprintf("sample = %s  |  group = %s  |  identity = %s",
                params$sample_col, params$group_col, params$identity_col_used),
        if (!is.null(batch_col))
          sprintf("  |  batch = %s", batch_col)
      )
    )
  )

  # ---- Tab 2: Warnings ----
  warning_tab <- htmltools::tags$div(
    class = "tab-content",
    id    = "tab-warnings",
    htmltools::tags$div(
      class = "report-section",
      htmltools::tags$div(class = "section-title",
        htmltools::tags$span(class = "section-icon", "\u26A0"),
        sprintf("Warnings (%d total, %d high)", n_warn, n_high)
      ),
      build_warning_section(warning_table)
    )
  )

  # ---- Tab 3: Metadata Audit ----
  audit_tab <- htmltools::tags$div(
    class = "tab-content",
    id    = "tab-audit",
    htmltools::tags$div(
      class = "report-section",
      htmltools::tags$div(class = "section-title",
        htmltools::tags$span(class = "section-icon", "\uD83D\uDCCA"),
        "Metadata Audit"
      ),
      build_audit_html(tables, batch_col)
    )
  )

  # ---- Tab 4: Sample-level Composition (Plots 1, 2, 3) ----
  sample_plots <- list()
  if (!is.null(plots$sample_total)) {
    sample_plots[[length(sample_plots) + 1]] <- htmltools::tags$div(
      class = "plot-block",
      htmltools::tags$div(class = "plot-body",
                          htmltools::as.tags(plots$sample_total))
    )
  }
  if (!is.null(plots$composition_by_sample)) {
    sample_plots[[length(sample_plots) + 1]] <- htmltools::tags$div(
      class = "plot-block",
      htmltools::tags$div(class = "plot-body",
                          htmltools::as.tags(plots$composition_by_sample))
    )
  }
  if (!is.null(plots$proportion_heatmap)) {
    sample_plots[[length(sample_plots) + 1]] <- htmltools::tags$div(
      class = "plot-block",
      htmltools::tags$div(class = "plot-body",
                          htmltools::as.tags(plots$proportion_heatmap))
    )
  }

  sample_tab <- htmltools::tags$div(
    class = "tab-content",
    id    = "tab-sample",
    htmltools::tags$div(
      class = "report-section",
      htmltools::tags$div(class = "section-title",
        htmltools::tags$span(class = "section-icon", "\uD83D\uDCC8"),
        "Sample-Level Composition"
      ),
      sample_plots
    )
  )

  # ---- Tab 5: Group-level (Plots 6, 7) ----
  group_plots <- list()
  if (!is.null(plots$composition_by_group)) {
    group_plots[[length(group_plots) + 1]] <- htmltools::tags$div(
      class = "plot-block",
      htmltools::tags$div(class = "plot-body",
                          htmltools::as.tags(plots$composition_by_group))
    )
  }
  if (!is.null(plots$sample_level_by_group)) {
    group_plots[[length(group_plots) + 1]] <- htmltools::tags$div(
      class = "plot-block",
      htmltools::tags$div(class = "plot-body",
                          htmltools::as.tags(plots$sample_level_by_group))
    )
  }

  group_tab <- htmltools::tags$div(
    class = "tab-content",
    id    = "tab-group",
    htmltools::tags$div(
      class = "report-section",
      htmltools::tags$div(class = "section-title",
        htmltools::tags$span(class = "section-icon", "\uD83D\uDCCA"),
        "Group-Level Descriptive Composition"
      ),
      group_plots
    )
  )

  # ---- Tab 6: Sample Dominance (Plots 4, 5) ----
  dom_items <- list()
  if (!is.null(plots$sample_contribution_heatmap)) {
    dom_items[[length(dom_items) + 1]] <- htmltools::tags$div(
      class = "plot-block",
      htmltools::tags$div(class = "plot-body",
        htmltools::as.tags(plots$sample_contribution_heatmap))
    )
  }
  if (!is.null(plots$max_sample_contribution)) {
    dom_items[[length(dom_items) + 1]] <- htmltools::tags$div(
      class = "plot-block",
      htmltools::tags$div(class = "plot-body",
        htmltools::as.tags(plots$max_sample_contribution))
    )
  }
  # Dominance table
  dom_items[[length(dom_items) + 1]] <- htmltools::tags$div(
    class = "plot-block",
    htmltools::tags$div(class = "plot-title", "Dominance Table"),
    build_dominance_html_table(tables$dominance_table, params$dominance_threshold)
  )
  # Dominance-specific warnings
  dom_warns <- warning_table[
    warning_table$warning_type == "sample_dominance",
  ]
  if (nrow(dom_warns) > 0) {
    dom_items[[length(dom_items) + 1]] <- htmltools::tags$div(
      class = "descriptive-only-banner",
      style = "margin-top:16px;",
      htmltools::tags$strong("Sample Dominance Warnings:"),
      htmltools::tags$ul(
        style = "margin:6px 0 0 20px; font-size:0.85em;",
        lapply(dom_warns$message, function(m) htmltools::tags$li(m))
      )
    )
  }

  dom_tab <- htmltools::tags$div(
    class = "tab-content",
    id    = "tab-dominance",
    htmltools::tags$div(
      class = "report-section",
      htmltools::tags$div(class = "section-title",
        htmltools::tags$span(class = "section-icon", "\uD83D\uDD0D"),
        "Sample Dominance / Outlier"
      ),
      dom_items
    )
  )

  # ---- Tab 7: Methods / Notes ----
  methods_tab <- htmltools::tags$div(
    class = "tab-content",
    id    = "tab-methods",
    htmltools::tags$div(
      class = "report-section",
      htmltools::tags$div(class = "section-title",
        htmltools::tags$span(class = "section-icon", "\uD83D\uDCDD"),
        "Methods / Notes"
      ),
      build_methods_section()
    )
  )

  # ---- Header ----
  header <- htmltools::tags$header(
    class = "report-header",
    htmltools::tags$h1(title),
    htmltools::tags$div(class = "header-meta",
      htmltools::tags$span(sprintf("Cells: %s", fmt_num(params$n_cells))),
      htmltools::tags$span(sprintf("Samples: %s", params$n_samples)),
      htmltools::tags$span(sprintf("Groups: %s", params$n_groups)),
      htmltools::tags$span(sprintf("Identities: %s", params$n_identities)),
      htmltools::tags$span(sprintf("identity_col: %s", params$identity_col_used))
    )
  )

  # ---- Tab Navigation (after header) ----
  tab_nav <- htmltools::tags$div(
    class = "tab-nav",
    tab_btn("overview", "Overview"),
    tab_btn("warnings", sprintf("Warnings (%d)", n_warn), has_warn = n_high > 0),
    tab_btn("audit", "Metadata Audit"),
    tab_btn("sample", "Sample-Level"),
    tab_btn("group", "Group-Level"),
    tab_btn("dominance", "Dominance"),
    tab_btn("methods", "Methods")
  )

  # ---- Footer ----
  footer <- htmltools::tags$footer(
    class = "report-footer",
    sprintf(
      "Generated by scReportComposition v0.2.0  |  %s  |  scReport Ecosystem",
      format(Sys.time(), "%Y-%m-%d %H:%M")
    )
  )

  # ---- Full page ----
  page <- htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "UTF-8"),
      htmltools::tags$meta(
        name    = "viewport",
        content = "width=device-width, initial-scale=1.0"
      ),
      htmltools::tags$title(title),
      htmltools::tags$style(htmltools::HTML(comp_audit_css())),
      htmltools::tags$script(
        src = "https://cdn.plot.ly/plotly-latest.min.js"
      )
    ),
    htmltools::tags$body(
      htmltools::tags$div(class = "container",
        header,
        tab_nav,
        overview_tab,
        warning_tab,
        audit_tab,
        sample_tab,
        group_tab,
        dom_tab,
        methods_tab,
        footer
      ),
      htmltools::tags$script(htmltools::HTML(comp_audit_js())),
      # Activate first tab (Overview) and warnings tab if there are warnings
      htmltools::tags$script(htmltools::HTML(sprintf(
        'document.getElementById("tab-btn-overview").classList.add("active");
         document.getElementById("tab-overview").classList.add("visible");
         %s',
        if (n_high > 0) {
          'document.getElementById("tab-btn-warnings").style.animation = "none";'
        } else ""
      ))
    )
  )

  htmltools::save_html(page, file = output)

  message("Composition audit report written to: ",
          normalizePath(output, mustWork = FALSE))
  invisible(output)
}
