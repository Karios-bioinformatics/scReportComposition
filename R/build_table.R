# scReportComposition: Composition Table Renderer --------------------------------
#
# render_composition_table() — builds an HTML table from the composition
#   data, suitable for embedding in the report.


#' Render Composition Table as HTML
#'
#' Produces a scrollable HTML table showing every row of the
#' composition data.
#'
#' @param comp_data A data.frame from \code{prepare_composition_data()}
#' @param condition_col Optional condition column name (for display)
#' @return An \code{htmltools} tag
#' @keywords internal
render_composition_table <- function(comp_data, condition_col = NULL) {

  has_condition <- "condition" %in% names(comp_data)

  # Build display data — filter to populated rows
  display <- comp_data[comp_data$total_cells > 0, , drop = FALSE]
  if (nrow(display) == 0) {
    return(htmltools::tags$p(
      class = "no-data", "No composition data available."
    ))
  }

  # Build header
  headers <- c("Sample", "Cell Type",
               "n Cells", "Total Cells", "Fraction")
  if (has_condition) {
    headers <- c("Sample", "Condition", "Cell Type",
                 "n Cells", "Total Cells", "Fraction")
  }

  header_row <- htmltools::tags$tr(
    lapply(headers, function(h) htmltools::tags$th(h))
  )

  # Build body rows
  body_rows <- lapply(seq_len(nrow(display)), function(i) {
    row <- display[i, , drop = FALSE]
    if (has_condition) {
      cells <- list(
        htmltools::tags$td(as.character(row$sample)),
        htmltools::tags$td(as.character(row$condition)),
        htmltools::tags$td(as.character(row$celltype)),
        htmltools::tags$td(fmt_num(row$n_cells)),
        htmltools::tags$td(fmt_num(row$total_cells)),
        htmltools::tags$td(fmt_pct(row$fraction))
      )
    } else {
      cells <- list(
        htmltools::tags$td(as.character(row$sample)),
        htmltools::tags$td(as.character(row$celltype)),
        htmltools::tags$td(fmt_num(row$n_cells)),
        htmltools::tags$td(fmt_num(row$total_cells)),
        htmltools::tags$td(fmt_pct(row$fraction))
      )
    }
    htmltools::tags$tr(cells)
  })

  htmltools::tags$div(
    class = "comp-table-wrapper",
    htmltools::tags$table(
      class = "comp-table",
      htmltools::tags$thead(header_row),
      htmltools::tags$tbody(body_rows)
    ),
    htmltools::tags$div(
      class = "comp-table-footer",
      sprintf("%d rows", nrow(display))
    )
  )
}
