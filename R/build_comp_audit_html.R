# scReportComposition: build_comp_audit_html.R — HTML Report Assembly ----------
#
# Builds a self-contained interactive HTML report with left sidebar navigation
# and section-switching content panels. Design follows scReportLite conventions.
#
# 7 sections: Overview | Warnings | Metadata Audit | Sample-Level |
#             Group-Level | Sample Dominance | Tables / Methods


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
  --sr-radius-md: 8px;
  --sr-radius-lg: 14px;
  --sr-shadow: 0 1px 3px rgba(0,0,0,0.05);
  --danger: #d63031;
  --danger-bg: #fff5f5;
  --warning-amber: #e17055;
}

* { box-sizing: border-box; margin: 0; padding: 0; }

body {
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto,
               "Helvetica Neue", Arial, sans-serif;
  background: #f5f6fa; color: var(--sr-text); line-height: 1.5;
  height: 100vh; overflow: hidden;
}

.container {
  height: 100vh; display: flex; flex-direction: column;
}

/* ---- Header ---- */
.report-header {
  display: flex; align-items: center; justify-content: space-between;
  background: linear-gradient(135deg, var(--sr-accent), var(--sr-accent-dark));
  color: #fff; padding: 14px 24px; flex-shrink: 0;
}
.report-header h1 { font-size: 1.2em; font-weight: 700; }
.report-meta { font-size: 0.75em; opacity: 0.9; }
.report-meta span { margin-left: 16px; white-space: nowrap; }

/* ---- Body layout ---- */
.report-body {
  flex: 1; min-height: 0;
  display: grid; grid-template-columns: 200px minmax(0, 1fr);
  overflow: hidden;
}

/* ---- Left Sidebar ---- */
.comp-nav {
  background: #fff; border-right: 1px solid var(--sr-border);
  overflow-y: auto; padding: 10px 6px;
  display: flex; flex-direction: column; gap: 2px;
}
.comp-nav-label {
  font-size: 0.68em; font-weight: 700; color: var(--sr-light-muted);
  text-transform: uppercase; letter-spacing: 0.5px;
  padding: 4px 8px; margin-top: 10px;
}
.comp-nav-label:first-child { margin-top: 0; }
.comp-nav-item {
  display: flex; align-items: center; padding: 6px 8px;
  cursor: pointer; border-radius: var(--sr-radius-sm);
  font-size: 0.8em; color: var(--sr-muted);
  transition: background 0.15s, color 0.15s;
  user-select: none; border-left: 3px solid transparent; gap: 6px;
}
.comp-nav-item:hover { background: var(--sr-accent-soft); }
.comp-nav-item.active {
  background: var(--sr-accent-soft);
  border-left-color: var(--sr-accent); font-weight: 600; color: var(--sr-text);
}
.comp-nav-dot {
  width: 6px; height: 6px; border-radius: 50%; flex-shrink: 0;
  background: var(--sr-accent); opacity: 0; transition: opacity 0.15s;
}
.comp-nav-item.active .comp-nav-dot { opacity: 1; }
.comp-nav-item.has-warn { color: var(--danger); }

/* ---- Content area ---- */
.comp-content {
  min-width: 0; min-height: 0;
  overflow-y: auto; padding: 20px 24px 40px;
}
.comp-section { display: none; }
.comp-section.comp-visible { display: block; }

/* ---- Summary Cards ---- */
.summary-cards {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(170px, 1fr));
  gap: 12px; margin-bottom: 16px;
}
.summary-card {
  background: #fff; border-radius: var(--sr-radius-md);
  padding: 14px 16px;
  box-shadow: var(--sr-shadow);
  border-top: 3px solid var(--sr-accent);
}
.summary-card-value {
  font-size: 1.5em; font-weight: 700; color: var(--sr-text); line-height: 1.1;
}
.summary-card-label {
  font-size: 0.7em; font-weight: 600; color: var(--sr-muted);
  text-transform: uppercase; letter-spacing: 0.4px; margin-top: 3px;
}
.summary-card-detail {
  font-size: 0.76em; color: var(--sr-muted); margin-top: 4px; line-height: 1.5;
}

/* ---- Warning elements ---- */
.warning-badges {
  display: flex; flex-wrap: wrap; gap: 10px; margin-bottom: 14px;
}
.warning-badge {
  padding: 5px 12px; border-radius: 16px; font-size: 0.76em;
  font-weight: 600; display: inline-flex; align-items: center; gap: 5px;
}
.warning-badge.severity-high {
  background: var(--danger-bg); color: var(--danger); border: 1px solid #fab1a0;
}
.warning-badge.severity-medium {
  background: #fffdf5; color: #b8860b; border: 1px solid #ffeaa7;
}

.descriptive-only-banner {
  background: var(--danger-bg); border: 1px solid #fab1a0;
  border-left: 4px solid var(--danger); color: #c0392b;
  padding: 10px 16px; border-radius: var(--sr-radius-sm);
  font-size: 0.88em; font-weight: 500; margin-bottom: 14px;
}

.warning-table-wrapper {
  border: 1px solid var(--sr-border); border-radius: var(--sr-radius-sm);
  overflow: hidden; background: #fff;
}
.warning-table {
  width: 100%; border-collapse: collapse; font-size: 0.83em;
}
.warning-table thead { background: #f8f9fc; }
.warning-table th {
  text-align: left; padding: 9px 12px; font-weight: 600; color: var(--sr-muted);
  font-size: 0.85em; border-bottom: 2px solid var(--sr-border); white-space: nowrap;
}
.warning-table td {
  padding: 8px 12px; border-bottom: 1px solid #f0f1f5; vertical-align: top;
}
.warning-table tr.row-high    { background: #fffbfb; }
.warning-table tr.row-medium  { background: #fffffb; }

.sev-tag {
  display: inline-block; padding: 2px 7px; border-radius: 4px;
  font-size: 0.76em; font-weight: 700; text-transform: uppercase;
  letter-spacing: 0.3px;
}
.sev-tag.high   { background: #ffe0e0; color: #c0392b; }
.sev-tag.medium { background: #ffeaa7; color: #856404; }

.no-warnings {
  text-align: center; color: var(--sr-muted); padding: 20px; font-style: italic;
}

/* ---- Audit tables ---- */
.audit-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(320px, 1fr));
  gap: 16px;
}
.audit-block {
  border: 1px solid var(--sr-border); border-radius: var(--sr-radius-sm);
  overflow: hidden; background: #fff;
}
.audit-block-title {
  background: #f8f9fc; padding: 7px 12px;
  font-weight: 600; font-size: 0.78em; color: var(--sr-muted);
  text-transform: uppercase; letter-spacing: 0.4px;
  border-bottom: 1px solid var(--sr-border);
}
.audit-table {
  width: 100%; border-collapse: collapse; font-size: 0.8em;
}
.audit-table th {
  text-align: left; padding: 6px 10px; border-bottom: 1px solid var(--sr-border);
  font-weight: 600; color: var(--sr-muted); font-size: 0.86em;
}
.audit-table td { padding: 5px 10px; border-bottom: 1px solid #f0f1f5; }
.audit-table tbody tr:hover { background: #f8f9fc; }

/* ---- Plot blocks ---- */
.plot-block { background: #fff; border-radius: var(--sr-radius-md);
  padding: 16px; margin-bottom: 16px; box-shadow: var(--sr-shadow);
}
.plot-block:last-child { margin-bottom: 0; }
.plot-body { min-height: 350px; }
.plot-body .html-widget, .plot-body .plotly, .plot-body .js-plotly-plot {
  width: 100% !important;
}

/* ---- Dominance table ---- */
.dominance-table-wrapper {
  border: 1px solid var(--sr-border); border-radius: var(--sr-radius-sm);
  overflow: auto; max-height: 520px; background: #fff;
}
.dominance-table { width: 100%; border-collapse: collapse; font-size: 0.8em; }
.dominance-table thead {
  position: sticky; top: 0; z-index: 1; background: #f8f9fc;
}
.dominance-table th {
  text-align: left; padding: 7px 10px; border-bottom: 2px solid var(--sr-border);
  font-weight: 600; color: var(--sr-muted); font-size: 0.84em; white-space: nowrap;
}
.dominance-table td { padding: 5px 10px; border-bottom: 1px solid #f0f1f5; }
.dominance-table .cell-highlight {
  background: #fff5f5; font-weight: 600; color: var(--danger);
}

/* ---- Collapsible ---- */
.collapsible {
  border: 1px solid var(--sr-border); border-radius: var(--sr-radius-sm);
  margin-bottom: 12px; overflow: hidden; background: #fff;
}
.collapsible-header {
  padding: 9px 14px; background: #f8f9fc; cursor: pointer;
  font-weight: 600; font-size: 0.82em; color: var(--sr-muted);
  text-transform: uppercase; letter-spacing: 0.4px;
  display: flex; align-items: center; justify-content: space-between;
  user-select: none;
}
.collapsible-header:hover { background: #eef0f4; }
.collapsible-header .arrow { transition: transform 0.2s; font-size: 0.75em; }
.collapsible-header.open .arrow { transform: rotate(90deg); }
.collapsible-body { display: none; padding: 0; }
.collapsible-body.open { display: block; }
.collapsible .audit-table thead { background: #f1f3f8; }

/* ---- Methods ---- */
.methods-list { font-size: 0.88em; line-height: 1.9; padding-left: 20px; }
.methods-list li { margin-bottom: 6px; }
.methods-list li::marker { color: var(--sr-accent); font-weight: 700; }

/* ---- Footer ---- */
.report-footer {
  flex-shrink: 0; text-align: center; padding: 8px 0;
  font-size: 0.7em; color: var(--sr-light-muted);
  border-top: 1px solid var(--sr-border);
}

/* ---- Section title ---- */
.comp-section-title {
  font-size: 1em; font-weight: 600; color: var(--sr-text);
  margin-bottom: 16px; padding-bottom: 8px;
  border-bottom: 1px solid var(--sr-border);
}

/* ---- Responsive ---- */
@media (max-width: 800px) {
  .report-body { grid-template-columns: 1fr; }
  .comp-nav { display: none; }
}

@media print {
  body { background: #fff; height: auto; overflow: visible; }
  .comp-nav { display: none; }
  .report-body { display: block; }
  .comp-content { overflow: visible; }
  .comp-section { display: block !important; }
  .plot-block, .summary-card { box-shadow: none; border: 1px solid #ddd; }
}
'
}


# ---- JavaScript ---------------------------------------------------------------

comp_audit_js <- function() {
'
// === scReportComposition Audit v0.2.0 ===
// Left sidebar navigation + Plotly resize + collapsible blocks

function switchSection(name) {
  document.querySelectorAll(".comp-nav-item").forEach(function(el) {
    el.classList.remove("active");
  });
  var target = document.getElementById("nav-" + name);
  if (target) target.classList.add("active");

  document.querySelectorAll(".comp-section").forEach(function(s) {
    s.classList.remove("comp-visible");
  });
  var targetSection = document.getElementById("section-" + name);
  if (targetSection) {
    targetSection.classList.add("comp-visible");
    setTimeout(function() {
      var plots = targetSection.querySelectorAll(".js-plotly-plot");
      plots.forEach(function(p) {
        try { Plotly.Plots.resize(p); } catch(e) {}
      });
    }, 80);
  }
}

function toggleCollapsible(header) {
  var body = header.nextElementSibling;
  var isOpen = body.classList.contains("open");
  if (isOpen) {
    body.classList.remove("open");
    header.classList.remove("open");
  } else {
    body.classList.add("open");
    header.classList.add("open");
    // Resize plots inside newly opened collapsible
    setTimeout(function() {
      var plots = body.querySelectorAll(".js-plotly-plot");
      plots.forEach(function(p) {
        try { Plotly.Plots.resize(p); } catch(e) {}
      });
    }, 100);
  }
}

window.addEventListener("resize", function() {
  if (!window.Plotly) return;
  var visible = document.querySelector(".comp-section.comp-visible");
  if (!visible) return;
  var plots = visible.querySelectorAll(".js-plotly-plot");
  plots.forEach(function(p) {
    try { Plotly.Plots.resize(p); } catch(e) {}
  });
});
'
}


# ---- HTML Builders ------------------------------------------------------------

#' Build Summary Cards
#' @keywords internal
build_overview_cards <- function(params) {
  list(
    card("Total Cells",     fmt_num(params$n_cells)),
    card("Samples",         as.character(params$n_samples)),
    card("Groups",          as.character(params$n_groups)),
    card("Identities",      as.character(params$n_identities)),
    card("Identity Column", params$identity_col_used),
    card("Sample Column",   params$sample_col),
    card("Group Column",    params$group_col)
  )
}

card <- function(label, value) {
  htmltools::tags$div(
    class = "summary-card",
    htmltools::tags$div(class = "summary-card-value", value),
    htmltools::tags$div(class = "summary-card-label", label)
  )
}


#' Build Warning Section Content
#' @keywords internal
build_warning_content <- function(warning_table) {
  if (nrow(warning_table) == 0) {
    return(list(htmltools::tags$div(
      class = "no-warnings", "\u2714 No warnings detected. All checks passed."
    )))
  }

  items <- list()
  sev_counts <- table(warning_table$severity)

  # Descriptive-only banner
  has_desc <- any(warning_table$warning_type == "descriptive_only")
  if (has_desc) {
    desc_msg <- warning_table$message[
      warning_table$warning_type == "descriptive_only"
    ][1]
    items[[length(items) + 1]] <- htmltools::tags$div(
      class = "descriptive-only-banner", "\u26A0 ", desc_msg
    )
  }

  # Badges
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
  items[[length(items) + 1]] <- htmltools::tags$div(
    class = "warning-badges", badges
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


#' Build Metadata Audit Content
#' @keywords internal
build_audit_content <- function(tables, batch_col = NULL) {
  sample_total <- tables$sample_total
  prop_table   <- tables$prop_table

  blocks <- list()

  # Sample totals
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
      sprintf("Sample Totals (%d)", nrow(st_data))),
    htmltools::tags$table(class = "audit-table",
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
    htmltools::tags$table(class = "audit-table",
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
                          "Group \u00d7 Batch"),
      htmltools::tags$table(class = "audit-table",
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
      htmltools::tags$td(class = if (is_dom) "cell-highlight" else "", pct)
    )
  })
  htmltools::tags$div(class = "dominance-table-wrapper",
    htmltools::tags$table(class = "dominance-table",
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tags$th("Identity"),
        htmltools::tags$th("Dominant Sample"),
        htmltools::tags$th("Dominant Group"),
        htmltools::tags$th("Dominant N"),
        htmltools::tags$th("Total Cells"),
        htmltools::tags$th("Max Contribution")
      )),
      htmltools::tags$tbody(rows)
    )
  )
}


#' Build Collapsible Table
#' @keywords internal
collapsible_block <- function(title, data, col_names = NULL) {
  if (is.null(col_names)) col_names <- names(data)
  rows <- lapply(seq_len(min(nrow(data), 50)), function(i) {
    cells <- lapply(col_names, function(cn) {
      val <- data[i, cn]
      htmltools::tags$td(if (is.numeric(val)) fmt_num(val) else as.character(val))
    })
    htmltools::tags$tr(cells)
  })
  ths <- lapply(col_names, function(h) htmltools::tags$th(h))

  header_id <- paste0("coll-", gsub("[^a-zA-Z0-9]", "-", title))
  htmltools::tags$div(
    class = "collapsible",
    htmltools::tags$div(
      class = "collapsible-header",
      id = header_id,
      onclick = paste0("toggleCollapsible(this)"),
      title,
      htmltools::tags$span(class = "arrow", "\u25B8")
    ),
    htmltools::tags$div(
      class = "collapsible-body",
      htmltools::tags$table(class = "audit-table",
        htmltools::tags$thead(htmltools::tags$tr(ths)),
        htmltools::tags$tbody(rows)
      ),
      if (nrow(data) > 50)
        htmltools::tags$div(
          style = "padding: 6px 12px; font-size:0.78em; color:var(--sr-muted);",
          sprintf("Showing first 50 of %d rows", nrow(data))
        )
    )
  )
}


#' Build Methods / Notes Content
#' @keywords internal
build_methods_content <- function() {
  methods <- c(
    "Proportions are calculated within each sample (n_cells / total_cells).",
    "Group-level summaries are calculated as the mean of sample-level proportions. Cells are not treated as independent biological replicates.",
    "Groups with fewer than 2 samples are descriptive only; no inferential differential composition (p-values, t-test, Wilcoxon) is performed.",
    "Sample dominance indicates sample-specific composition patterns and requires further QC, batch, and marker validation. It is not automatically interpreted as a batch effect.",
    "This report is a descriptive composition audit tool. No inferential differential composition test is performed in this version.",
    "Group-level mean_proportion must not be confused with pooled-cell proportion — it is the mean of per-sample proportions."
  )
  htmltools::tags$ul(
    class = "methods-list",
    lapply(methods, function(m) htmltools::tags$li(m))
  )
}


#' Plot block wrapper
#' @keywords internal
plot_block <- function(title, widget) {
  if (is.null(widget)) return(NULL)
  htmltools::tags$div(
    class = "plot-block",
    htmltools::tags$div(class = "comp-section-title", style = "font-size:0.82em; margin-bottom:8px; padding-bottom:6px;", title),
    htmltools::tags$div(class = "plot-body", htmltools::as.tags(widget))
  )
}


# ---- Section builder ----

#' Build a report section
#' @keywords internal
comp_section <- function(id, title, ..., visible = FALSE) {
  htmltools::tags$div(
    class = paste("comp-section", if (visible) "comp-visible" else ""),
    id = paste0("section-", id),
    htmltools::tags$div(class = "comp-section-title", title),
    ...
  )
}

#' Build a nav item
#' @keywords internal
nav_item <- function(id, label, has_warn = FALSE) {
  htmltools::tags$div(
    class = paste("comp-nav-item", if (has_warn) "has-warn" else ""),
    id = paste0("nav-", id),
    onclick = paste0("switchSection(", shQuote(id), ")"),
    htmltools::tags$span(class = "comp-nav-dot"),
    label
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

  n_warn  <- nrow(warning_table)
  n_high  <- sum(warning_table$severity == "high")
  has_warn <- n_high > 0

  # ---- Section 1: Overview ----
  overview <- comp_section("overview", "Overview", visible = TRUE,
    htmltools::tags$div(class = "summary-cards",
                         build_overview_cards(params))
  )

  # ---- Section 2: Warnings ----
  warnings_sec <- comp_section("warnings", "Warnings",
    build_warning_content(warning_table)
  )

  # ---- Section 3: Metadata Audit ----
  audit <- comp_section("audit", "Metadata Audit",
    build_audit_content(tables, batch_col),
    plot_block("Plot 1: Sample total cell count", plots$sample_total)
  )

  # ---- Section 4: Sample-Level Composition ----
  sample_sec <- comp_section("sample", "Sample-Level Composition",
    plot_block("Plot 2: Identity composition by sample",
               plots$composition_by_sample),
    plot_block("Plot 3: Identity proportion heatmap",
               plots$proportion_heatmap)
  )

  # ---- Section 5: Group-Level Descriptive ----
  has_small <- any(warning_table$warning_type == "group_n_less_than_2")
  group_title <- "Group-Level Descriptive Composition"
  group_plots <- list(
    plot_block("Plot 6: Descriptive identity composition by group",
               plots$composition_by_group),
    plot_block("Plot 7: Sample-level identity proportions by group",
               plots$sample_level_by_group)
  )

  group_sec <- comp_section("group", group_title,
    if (has_small) htmltools::tags$div(
      class = "descriptive-only-banner",
      "\u26A0 Caution: one or more groups have fewer than 2 samples; group-level summaries are descriptive only."
    ),
    group_plots
  )

  # ---- Section 6: Sample Dominance ----
  dom_items <- list(
    plot_block("Plot 4: Sample contribution within each identity",
               plots$sample_contribution_heatmap),
    plot_block("Plot 5: Maximum sample contribution per identity",
               plots$max_sample_contribution),
    htmltools::tags$div(
      class = "plot-block",
      htmltools::tags$div(class = "comp-section-title",
        style = "font-size:0.82em; margin-bottom:8px; padding-bottom:6px;",
        "Dominance Table"),
      build_dominance_html_table(tables$dominance_table,
                                  params$dominance_threshold)
    )
  )

  dom_warns <- warning_table[
    warning_table$warning_type == "sample_dominance",
  ]
  if (nrow(dom_warns) > 0) {
    dom_items[[length(dom_items) + 1]] <- htmltools::tags$div(
      class = "descriptive-only-banner",
      style = "margin-top:12px;",
      htmltools::tags$strong("Sample Dominance Warnings:"),
      htmltools::tags$ul(
        style = "margin:4px 0 0 18px; font-size:0.84em;",
        lapply(dom_warns$message, function(m) htmltools::tags$li(m))
      )
    )
  }

  dom_sec <- comp_section("dominance", "Sample Dominance / Outlier", dom_items)

  # ---- Section 7: Tables / Methods ----
  tbls <- tables
  methods_sec <- comp_section("methods", "Tables / Methods",
    htmltools::tags$div(
      class = "comp-section-title",
      style = "font-size:0.88em; margin-bottom:12px; padding-bottom:6px;",
      "Intermediate Tables"
    ),
    collapsible_block("count_table (sample x identity)",
                       tbls$count_table,
                       c("sample", "group", "identity", "n_cells")),
    collapsible_block("group_summary",
                       tbls$group_summary,
                       c("group", "identity", "mean_proportion",
                         "sd_proportion", "n_samples", "total_cells")),
    collapsible_block("dominance_table",
                       tbls$dominance_table,
                       c("identity", "dominant_sample", "dominant_group",
                         "dominant_n_cells", "identity_total_cells",
                         "max_sample_contribution")),
    htmltools::tags$div(
      class = "comp-section-title",
      style = "font-size:0.88em; margin:20px 0 12px; padding-bottom:6px;",
      "Methods / Notes"
    ),
    build_methods_content()
  )

  # ---- Build sidebar ----
  nav_items <- list(
    htmltools::tags$div(class = "comp-nav-label", "Report"),
    nav_item("overview", "Overview"),
    nav_item("warnings",
             sprintf("Warnings (%d)", n_warn),
             has_warn = has_warn),
    nav_item("audit", "Metadata Audit"),
    nav_item("sample", "Sample-Level"),
    nav_item("group", "Group-Level"),
    nav_item("dominance", "Dominance"),
    nav_item("methods", "Tables / Methods")
  )

  # ---- Header ----
  header <- htmltools::tags$header(
    class = "report-header",
    htmltools::tags$div(
      htmltools::tags$h1(title)
    ),
    htmltools::tags$div(class = "report-meta",
      htmltools::tags$span(sprintf("Cells: %s", fmt_num(params$n_cells))),
      htmltools::tags$span(sprintf("Samples: %s", params$n_samples)),
      htmltools::tags$span(sprintf("Groups: %s", params$n_groups)),
      htmltools::tags$span(sprintf("Identities: %s", params$n_identities)),
      htmltools::tags$span(sprintf("identity: %s", params$identity_col_used))
    )
  )

  sidebar <- htmltools::tags$nav(class = "comp-nav", nav_items)
  main    <- htmltools::tags$main(class = "comp-content", list(
    overview, warnings_sec, audit, sample_sec, group_sec, dom_sec, methods_sec
  ))
  footer  <- htmltools::tags$footer(class = "report-footer",
    sprintf("Generated by scReportComposition v0.2.0  |  %s  |  scReport Ecosystem",
            format(Sys.time(), "%Y-%m-%d %H:%M"))
  )

  script_tag <- htmltools::tags$script(htmltools::HTML(comp_audit_js()))

  page <- htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "UTF-8"),
      htmltools::tags$meta(name = "viewport",
                           content = "width=device-width, initial-scale=1.0"),
      htmltools::tags$title(title),
      htmltools::tags$style(htmltools::HTML(comp_audit_css()))
    ),
    htmltools::tags$body(
      htmltools::tags$div(class = "container",
        header,
        htmltools::tags$div(class = "report-body", sidebar, main),
        footer
      ),
      script_tag
    )
  )

  htmltools::save_html(page, file = output)

  message("Composition audit report written to: ",
          normalizePath(output, mustWork = FALSE))
  invisible(output)
}
