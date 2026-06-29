# scReportComposition: build_html — CSS + HTML Assembly -------------------------
#
# Generates the self-contained HTML report.
# Design follows scReportLite conventions: same accent colours, card-based
# layout, Plotly interactive charts, single-file output.


# ---- CSS ----------------------------------------------------------------------

report_css <- function() {
'/* === scReportComposition v0.1.0 Styles === */

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
  --sr-radius-lg: 10px;
}

* { box-sizing: border-box; margin: 0; padding: 0; }

body {
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto,
               "Helvetica Neue", Arial, sans-serif;
  background: #f5f6fa;
  color: #2d3436;
  line-height: 1.5;
}

.container {
  max-width: 1200px;
  margin: 0 auto;
  padding: 0 24px 48px;
}

/* --- Header --- */
.report-header {
  background: #fff;
  border-bottom: 1px solid var(--sr-border);
  padding: 20px 24px;
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin: 0 -24px 24px;
}
.report-title {
  font-size: 1.35em;
  font-weight: 600;
  color: var(--sr-text);
}
.report-meta {
  font-size: 0.85em;
  color: var(--sr-muted);
}

/* --- Summary Cards --- */
.summary-section {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 16px;
  margin-bottom: 28px;
}

.summary-card {
  background: #fff;
  border-radius: var(--sr-radius-md);
  padding: 20px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.06);
  display: flex;
  flex-direction: column;
  gap: 6px;
  border-top: 3px solid var(--sr-accent);
}

.summary-card-value {
  font-size: 1.8em;
  font-weight: 700;
  color: var(--sr-text);
  line-height: 1.1;
}

.summary-card-label {
  font-size: 0.78em;
  font-weight: 600;
  color: var(--sr-muted);
  text-transform: uppercase;
  letter-spacing: 0.5px;
}

.summary-card-detail {
  font-size: 0.82em;
  color: #636e72;
  line-height: 1.4;
}

/* --- Plot Sections --- */
.plot-section {
  background: #fff;
  border-radius: var(--sr-radius-md);
  padding: 20px;
  margin-bottom: 20px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.06);
}

.plot-section .section-title {
  font-size: 0.85em;
  font-weight: 600;
  color: #636e72;
  margin-bottom: 12px;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}

.plot-container {
  min-height: 400px;
  width: 100%;
}

.plot-container > *,
.plot-container .html-widget,
.plot-container .plotly,
.plot-container .js-plotly-plot {
  width: 100% !important;
}

/* --- Footer --- */
.report-footer {
  text-align: center;
  padding: 16px 0 32px;
  font-size: 0.78em;
  color: var(--sr-muted);
}

/* --- Plotly modebar --- */
.modebar-btn {
  border-radius: var(--sr-radius-sm) !important;
}
.modebar-btn:hover {
  background: var(--sr-accent-soft) !important;
}

/* --- Responsive --- */
@media (max-width: 768px) {
  .summary-section {
    grid-template-columns: repeat(2, 1fr);
  }
  .container {
    padding: 0 12px 24px;
  }
}
'
}


# ---- JavaScript ----------------------------------------------------------------

report_js <- function() {
'
// === scReportComposition v0.1.0 ===
// Minimal JS: report is primarily static Plotly widgets.
// Plotly modebar handles zoom / pan / download / hover natively.

window.addEventListener("resize", function() {
  if (!window.Plotly) return;
  var plots = document.querySelectorAll(".js-plotly-plot");
  for (var i = 0; i < plots.length; i++) {
    try { Plotly.Plots.resize(plots[i]); } catch(e) {}
  }
});
'
}


# ---- Summary Cards HTML -------------------------------------------------------

build_summary_cards <- function(summary) {
  cards <- list()

  # Card 1: Total Cells
  cards[[1]] <- htmltools::tags$div(
    class = "summary-card",
    htmltools::tags$div(class = "summary-card-value",
                        fmt_num(summary$total_cells)),
    htmltools::tags$div(class = "summary-card-label", "Total Cells")
  )

  # Card 2: Samples
  cards[[2]] <- htmltools::tags$div(
    class = "summary-card",
    htmltools::tags$div(class = "summary-card-value",
                        as.character(summary$n_samples)),
    htmltools::tags$div(class = "summary-card-label", "Samples")
  )

  # Card 3: Cell Types
  cards[[3]] <- htmltools::tags$div(
    class = "summary-card",
    htmltools::tags$div(class = "summary-card-value",
                        as.character(summary$n_celltypes)),
    htmltools::tags$div(class = "summary-card-label", "Cell Types")
  )

  # Card 4: Conditions (if present)
  if (!is.na(summary$n_conditions)) {
    cards[[4]] <- htmltools::tags$div(
      class = "summary-card",
      htmltools::tags$div(class = "summary-card-value",
                          as.character(summary$n_conditions)),
      htmltools::tags$div(class = "summary-card-label", "Conditions")
    )
  }

  # Card 5: Cells per sample (inline detail)
  cps_text <- paste(
    names(summary$cells_per_sample),
    fmt_num(summary$cells_per_sample),
    sep = ": ", collapse = "  |  "
  )
  cards[[length(cards) + 1]] <- htmltools::tags$div(
    class = "summary-card",
    htmltools::tags$div(class = "summary-card-value",
                        fmt_num(mean(summary$cells_per_sample))),
    htmltools::tags$div(class = "summary-card-label", "Avg Cells / Sample"),
    htmltools::tags$div(class = "summary-card-detail", cps_text)
  )

  # Card 6: Cell types detected (inline)
  ct_text <- paste(summary$celltypes_detected, collapse = ", ")
  cards[[length(cards) + 1]] <- htmltools::tags$div(
    class = "summary-card",
    htmltools::tags$div(class = "summary-card-value",
                        as.character(summary$n_celltypes)),
    htmltools::tags$div(class = "summary-card-label", "Cell Types Detected"),
    htmltools::tags$div(class = "summary-card-detail", ct_text)
  )

  cards
}


# ---- HTML Assembly -------------------------------------------------------------

#' Assemble and Write the Complete Composition HTML Report
#'
#' Combines the Summary Cards, composition plots, CSS, and JS into a
#' single self-contained HTML file.
#'
#' @param summary A list from \code{build_summary()}
#' @param plot_sample A plotly widget from \code{plot_sample_composition()}
#' @param plot_condition A plotly widget or NULL
#' @param plot_celltype A plotly widget from \code{plot_celltype_fraction()}
#' @param output Path to output HTML file
#' @param title Report title string
#' @return Invisibly, the path to the output file
#' @keywords internal
build_html <- function(summary, plot_sample, plot_condition, plot_celltype,
                        output, title = "scReportComposition") {

  # ---- Section: Sample Composition ----
  sample_section <- htmltools::tags$div(
    class = "plot-section",
    htmltools::tags$div(class = "section-title", "Sample Composition"),
    htmltools::tags$div(class = "plot-container",
                        htmltools::as.tags(plot_sample))
  )

  # ---- Section: Condition Composition (only if plot exists) ----
  condition_section <- NULL
  if (!is.null(plot_condition)) {
    condition_section <- htmltools::tags$div(
      class = "plot-section",
      id    = "section-condition-composition",
      htmltools::tags$div(class = "section-title", "Condition Composition"),
      htmltools::tags$div(class = "plot-container",
                          htmltools::as.tags(plot_condition))
    )
  }

  # ---- Section: Cell-Type Fraction ----
  celltype_section <- htmltools::tags$div(
    class = "plot-section",
    htmltools::tags$div(class = "section-title", "Cell-Type Fraction"),
    htmltools::tags$div(class = "plot-container",
                        htmltools::as.tags(plot_celltype))
  )

  # ---- Build full page ----
  page <- htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "UTF-8"),
      htmltools::tags$meta(
        name    = "viewport",
        content = "width=device-width, initial-scale=1.0"
      ),
      htmltools::tags$title(title),
      htmltools::tags$style(htmltools::HTML(report_css()))
    ),

    htmltools::tags$body(
      htmltools::tags$div(class = "container",

        # Header
        htmltools::tags$header(class = "report-header",
          htmltools::tags$div(class = "report-title", title),
          htmltools::tags$div(class = "report-meta",
            "Cell Composition Report — scReportComposition v0.1.0")
        ),

        # Summary Cards
        htmltools::tags$div(
          class = "summary-section",
          build_summary_cards(summary)
        ),

        # Sample Composition
        sample_section,

        # Condition Composition (nullable)
        condition_section,

        # Cell-Type Fraction
        celltype_section,

        # Footer
        htmltools::tags$footer(class = "report-footer",
          "Generated by scReportComposition v0.1.0  |  ",
          "scReport Ecosystem"
        )
      ),

      # JS
      htmltools::tags$script(htmltools::HTML(report_js()))
    )
  )

  # ---- Write ----
  htmltools::save_html(page, file = output)

  message("Composition report written to: ", normalizePath(output, mustWork = FALSE))
  invisible(output)
}
