# scReportComposition: build_html — CSS + HTML + JS Assembly -------------------
#
# Generates a self-contained HTML report with left-side navigation and
# section-switching content panels.
# Design follows scReportLite visual conventions.


# ---- CSS ----------------------------------------------------------------------

report_css <- function() {
'/* === scReportComposition v0.1.0 === */

:root {
  --sr-accent: #00b894;
  --sr-accent-dark: #00997a;
  --sr-accent-soft: rgba(0, 184, 148, 0.12);
  --sr-accent-border: rgba(0, 184, 148, 0.35);
  --sr-border: #dfe6e9;
  --sr-text: #2d3436;
  --sr-muted: #95a5a6;
  --sr-radius-sm: 6px;
  --sr-radius-md: 8px;
}

* { box-sizing: border-box; margin: 0; padding: 0; }

body {
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto,
               "Helvetica Neue", Arial, sans-serif;
  background: #f5f6fa; color: #2d3436; line-height: 1.5;
  height: 100vh; overflow: hidden;
}

.container {
  height: 100vh; display: flex; flex-direction: column;
}

/* ---- Header ---- */
.report-header {
  display: flex; align-items: center; justify-content: space-between;
  background: #fff; border-bottom: 1px solid var(--sr-border);
  padding: 14px 24px; flex-shrink: 0;
}
.report-title {
  font-size: 1.25em; font-weight: 600; color: var(--sr-text);
}
.report-meta {
  font-size: 0.8em; color: var(--sr-muted);
}

/* ---- Body layout (below header) ---- */
.report-body {
  flex: 1; min-height: 0;
  display: grid; grid-template-columns: 220px minmax(0, 1fr);
  overflow: hidden;
}

/* ---- Left Sidebar ---- */
.comp-nav {
  background: #fff; border-right: 1px solid var(--sr-border);
  overflow-y: auto; padding: 12px 8px;
  display: flex; flex-direction: column; gap: 2px;
}
.comp-nav-label {
  font-size: 0.7em; font-weight: 700; color: #b2bec3;
  text-transform: uppercase; letter-spacing: 0.6px;
  padding: 4px 8px; margin-top: 10px;
}
.comp-nav-label:first-child { margin-top: 0; }
.comp-nav-item {
  display: flex; align-items: center; padding: 5px 8px;
  cursor: pointer; border-radius: var(--sr-radius-sm);
  font-size: 0.8em; color: #636e72;
  transition: background 0.15s, color 0.15s, border-color 0.15s;
  user-select: none; border-left: 3px solid transparent; gap: 6px;
}
.comp-nav-item:hover { background: var(--sr-accent-soft); }
.comp-nav-item.active {
  background: var(--sr-accent-soft);
  border-left-color: var(--sr-accent); font-weight: 600; color: #2d3436;
}
.comp-nav-dot {
  width: 6px; height: 6px; border-radius: 50%; flex-shrink: 0;
  background: var(--sr-accent); opacity: 0; transition: opacity 0.15s;
}
.comp-nav-item.active .comp-nav-dot { opacity: 1; }

/* ---- Content area ---- */
.comp-content {
  min-width: 0; min-height: 0;
  overflow-y: auto; padding: 20px 24px 40px;
}

/* ---- Section ---- */
.comp-section {
  display: none;
}
.comp-section.comp-visible { display: block; }

.comp-section-title {
  font-size: 1.05em; font-weight: 600; color: var(--sr-text);
  margin-bottom: 16px; padding-bottom: 8px;
  border-bottom: 1px solid var(--sr-border);
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
  box-shadow: 0 1px 3px rgba(0,0,0,0.05);
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
  font-size: 0.78em; color: #636e72; margin-top: 6px; line-height: 1.5;
  word-break: break-all;
}

/* ---- Plots ---- */
.comp-plot-block {
  background: #fff; border-radius: var(--sr-radius-md);
  padding: 16px; margin-bottom: 16px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.05);
}
.comp-plot-block .comp-plot-title {
  font-size: 0.78em; font-weight: 600; color: #636e72;
  text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 8px;
}
.comp-plot-body { min-height: 350px; }
.comp-plot-body > *,
.comp-plot-body .html-widget,
.comp-plot-body .plotly,
.comp-plot-body .js-plotly-plot {
  width: 100% !important;
}

/* ---- Composition Table ---- */
.comp-table-wrapper {
  max-height: 520px; overflow: auto;
  border: 1px solid var(--sr-border); border-radius: var(--sr-radius-md);
  background: #fff;
}
.comp-table {
  width: 100%; border-collapse: collapse; font-size: 0.82em;
}
.comp-table thead {
  position: sticky; top: 0; z-index: 1;
  background: #f8f9fc;
}
.comp-table th {
  text-align: left; padding: 8px 12px;
  border-bottom: 2px solid var(--sr-border);
  font-weight: 600; color: #636e72; font-size: 0.88em;
  white-space: nowrap;
}
.comp-table td {
  padding: 6px 12px; border-bottom: 1px solid #f0f1f5;
}
.comp-table tbody tr:hover { background: #f8f9fc; }
.comp-table-footer {
  padding: 6px 12px; font-size: 0.75em; color: var(--sr-muted);
  text-align: right; border-top: 1px solid var(--sr-border);
}

/* ---- Footer ---- */
.report-footer {
  flex-shrink: 0;
  text-align: center; padding: 10px 0;
  font-size: 0.72em; color: var(--sr-muted);
  border-top: 1px solid var(--sr-border);
}

/* ---- No-data placeholder ---- */
.no-data {
  color: var(--sr-muted); font-style: italic; padding: 20px 0;
  text-align: center;
}

/* ---- Responsive ---- */
@media (max-width: 800px) {
  .report-body { grid-template-columns: 1fr; }
  .comp-nav { display: none; }
}
'
}


# ---- JavaScript ---------------------------------------------------------------

report_js <- function() {
'
// === scReportComposition v0.1.0 ===
// Section navigation + Plotly resize

function switchSection(name) {
  var items = document.querySelectorAll(".comp-nav-item");
  items.forEach(function(el) { el.classList.remove("active"); });

  var target = document.getElementById("nav-" + name);
  if (target) target.classList.add("active");

  var sections = document.querySelectorAll(".comp-section");
  sections.forEach(function(s) { s.classList.remove("comp-visible"); });

  var targetSection = document.getElementById("section-" + name);
  if (targetSection) {
    targetSection.classList.add("comp-visible");
  }

  setTimeout(function() {
    var plots = targetSection
      ? targetSection.querySelectorAll(".js-plotly-plot")
      : [];
    plots.forEach(function(p) {
      try { Plotly.Plots.resize(p); } catch(e) {}
    });
  }, 50);
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


# ---- Summary Cards HTML -------------------------------------------------------

build_summary_cards <- function(summary) {
  cards <- list(
    card("Total Cells",     fmt_num(summary$total_cells)),
    card("Samples",         as.character(summary$n_samples)),
    card("Cell Types",      as.character(summary$n_celltypes))
  )

  if (!is.na(summary$n_conditions)) {
    cards <- c(cards, list(
      card("Conditions", as.character(summary$n_conditions))
    ))
  }

  # Cells per sample detail
  cps <- paste(
    names(summary$cells_per_sample),
    fmt_num(summary$cells_per_sample),
    sep = ": ", collapse = "  |  "
  )
  cards <- c(cards, list(card(
    "Avg Cells / Sample",
    fmt_num(mean(summary$cells_per_sample)),
    cps
  )))

  # Cells per condition
  if (!is.null(summary$cells_per_condition)) {
    cpc <- paste(
      names(summary$cells_per_condition),
      fmt_num(summary$cells_per_condition),
      sep = ": ", collapse = "  |  "
    )
    cards <- c(cards, list(card(
      "Cells / Condition", "", cpc
    )))
  }

  # Samples per condition
  if (!is.null(summary$samples_per_condition)) {
    spc <- paste(
      names(summary$samples_per_condition),
      summary$samples_per_condition,
      sep = ": ", collapse = "  |  "
    )
    cards <- c(cards, list(card(
      "Samples / Condition", "", spc
    )))
  }

  cards
}

card <- function(label, value, detail = NULL) {
  children <- list(
    htmltools::tags$div(class = "summary-card-value", value),
    htmltools::tags$div(class = "summary-card-label", label)
  )
  if (!is.null(detail) && nzchar(detail)) {
    children <- c(children, list(
      htmltools::tags$div(class = "summary-card-detail", detail)
    ))
  }
  htmltools::tags$div(class = "summary-card", children)
}


# ---- Section builders ----------------------------------------------------------

nav_item <- function(id, label) {
  htmltools::tags$div(
    class = "comp-nav-item",
    id = paste0("nav-", id),
    onclick = paste0("switchSection(", shQuote(id), ")"),
    htmltools::tags$span(class = "comp-nav-dot"),
    label
  )
}

comp_section <- function(id, title, ..., visible = FALSE) {
  htmltools::tags$div(
    class = paste("comp-section", if (visible) "comp-visible" else ""),
    id = paste0("section-", id),
    htmltools::tags$div(class = "comp-section-title", title),
    ...
  )
}

plot_block <- function(title, widget) {
  htmltools::tags$div(
    class = "comp-plot-block",
    htmltools::tags$div(class = "comp-plot-title", title),
    htmltools::tags$div(class = "comp-plot-body",
                        htmltools::as.tags(widget))
  )
}


# ---- HTML Assembly -------------------------------------------------------------

build_html <- function(summary, plots, comp_table,
                        output, title = "scReportComposition") {

  has_cond <- !is.na(summary$n_conditions)

  # ---- Build sections ----
  sections <- list()

  sections$overview <- comp_section(
    "overview", "Overview", visible = TRUE,
    htmltools::tags$div(class = "summary-cards",
                        build_summary_cards(summary))
  )

  sections$sample <- comp_section(
    "sample", "Sample Composition",
    if (!is.null(plots$p_sample_count))
      plot_block("Cell Counts per Sample", plots$p_sample_count),
    if (!is.null(plots$p_sample_frac))
      plot_block("Cell Fractions per Sample", plots$p_sample_frac)
  )

  if (has_cond) {
    sections$condition <- comp_section(
      "condition", "Condition Composition",
      if (!is.null(plots$p_cond_count))
        plot_block("Cell Counts per Condition", plots$p_cond_count),
      if (!is.null(plots$p_cond_frac))
        plot_block("Cell Fractions per Condition", plots$p_cond_frac)
    )
  }

  if (has_cond) {
    sections$ctdist <- comp_section(
      "ctdist", "Cell Type Distribution",
      if (!is.null(plots$p_ct_frac_by_cond))
        plot_block("Fraction by Condition", plots$p_ct_frac_by_cond),
      if (!is.null(plots$p_ct_count_by_cond))
        plot_block("Count by Condition", plots$p_ct_count_by_cond)
    )
  }

  sections$heatmaps <- comp_section(
    "heatmaps", "Heatmaps",
    if (!is.null(plots$p_heatmap_sample))
      plot_block("Sample x Cell Type", plots$p_heatmap_sample),
    if (!is.null(plots$p_heatmap_cond))
      plot_block("Condition x Cell Type", plots$p_heatmap_cond)
  )

  sections$table <- comp_section(
    "table", "Composition Table", comp_table
  )

  # ---- Build navigation ----
  nav_items <- list(
    htmltools::tags$div(class = "comp-nav-label", "Report"),
    nav_item("overview", "Overview"),
    nav_item("sample", "Sample Composition")
  )

  if (has_cond) {
    nav_items <- c(nav_items, list(
      nav_item("condition", "Condition Composition"),
      nav_item("ctdist", "Cell Type Distribution")
    ))
  }

  nav_items <- c(nav_items, list(
    nav_item("heatmaps", "Heatmaps"),
    nav_item("table", "Composition Table")
  ))

  # ---- Build full page ----
  header <- htmltools::tags$header(class = "report-header",
    htmltools::tags$div(class = "report-title", title),
    htmltools::tags$div(class = "report-meta",
      "Cell Composition Report \u2014 scReportComposition v0.1.0")
  )

  sidebar <- htmltools::tags$nav(class = "comp-nav", nav_items)
  main    <- htmltools::tags$main(class = "comp-content", sections)
  footer  <- htmltools::tags$footer(class = "report-footer",
    "Generated by scReportComposition v0.1.0  |  scReport Ecosystem")

  script_tag <- htmltools::tags$script(
    htmltools::HTML(report_js())
  )

  page <- htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "UTF-8"),
      htmltools::tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1.0"
      ),
      htmltools::tags$title(title),
      htmltools::tags$style(htmltools::HTML(report_css()))
    ),
    htmltools::tags$body(
      htmltools::tags$div(class = "container",
        header,
        htmltools::tags$div(class = "report-body",
          sidebar, main
        ),
        footer
      ),
      script_tag
    )
  )

  htmltools::save_html(page, file = output)

  message("Composition report written to: ",
          normalizePath(output, mustWork = FALSE))
  invisible(output)
}
