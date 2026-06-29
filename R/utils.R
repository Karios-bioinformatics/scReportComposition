# scReportComposition: Utility Functions -----------------------------------------


#' Natural sort for character vectors
#'
#' Sorts strings with trailing numbers in natural order (e.g. Sample-1,
#' Sample-2, Sample-10 instead of Sample-1, Sample-10, Sample-2).
#' Strings without trailing digits fall back to standard character sort.
#'
#' @param x Character vector to sort
#' @return Sorted character vector
#' @keywords internal
natural_sort <- function(x) {
  x <- as.character(x)
  m <- regexpr("\\d+$", x)
  if (any(m < 0)) {
    return(sort(x))
  }
  prefix <- substr(x, 1, m - 1)
  suffix <- as.integer(substr(x, m, m + attr(m, "match.length") - 1))
  x[order(prefix, suffix)]
}


#' Generate a qualitative colour palette for cell types
#'
#' Creates a named vector mapping cell-type identifiers to hex colour
#' codes using the same 32-colour qualitative palette shared across
#' the scReport ecosystem.
#'
#' @param groups Character vector of unique group identifiers
#' @return Named character vector of hex colours
#' @keywords internal
celltype_color_map <- function(groups) {
  palette <- c(
    "#E6194B", "#3CB44B", "#FFE119", "#0082C8", "#F58231", "#911EB4",
    "#46F0F0", "#F032E6", "#BCF60C", "#E6BEFF", "#008080", "#A52A2A",
    "#AA6E28", "#800000", "#22B14C", "#808000", "#000080", "#808080",
    "#DC143C", "#0A751C", "#FF6600", "#6200EA", "#B8860B", "#00CED1",
    "#6A1B9A", "#9E9D24", "#E91E63", "#0288D1", "#388E3C", "#D81B60",
    "#8D6E63", "#7C4DFF"
  )
  n <- length(groups)
  if (n > length(palette)) {
    warning(
      "Number of groups (", n, ") exceeds palette size (",
      length(palette), "). Colours will be recycled.",
      call. = FALSE
    )
  }
  colors <- rep(palette, length.out = n)
  names(colors) <- as.character(groups)
  colors
}


#' Format a numeric value for summary display
#'
#' @param x Numeric value
#' @param digits Number of decimal places
#' @return Character string
#' @keywords internal
fmt_num <- function(x, digits = 0) {
  format(round(x, digits), big.mark = ",", scientific = FALSE)
}


#' Format a proportion for display
#'
#' @param x Numeric proportion (0–1)
#' @return Character string like "24.0%"
#' @keywords internal
fmt_pct <- function(x) {
  sprintf("%.1f%%", x * 100)
}
