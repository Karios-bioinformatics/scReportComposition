# scReportComposition: build_comp_audit_plots.R — 7 Core Plots -----------------
#
# All plots use direct plotly (plotly::plot_ly), matching the existing
# scReportComposition codebase style.
#
# Plot 1: Sample total cell count
# Plot 2: Identity composition by sample
# Plot 3: Identity proportion heatmap
# Plot 4: Sample contribution within each identity
# Plot 5: Maximum sample contribution per identity
# Plot 6: Descriptive identity composition by group
# Plot 7: Sample-level identity proportions by group


# ---- Sample display order helper ---------------------------------------------

#' Sort samples by group then within-group natural order
#'
#' Computes a natural-sort rank for each sample, then orders by
#' group first and rank second so samples are grouped visually.
#'
#' @param sample_total data.frame with sample and group columns
#' @return Sorted character vector of sample names
#' @keywords internal
sample_display_order <- function(sample_total) {
  samples <- as.character(sample_total$sample)
  groups  <- as.character(sample_total$group)

  # Compute natural-sort rank of each sample within the whole set
  sorted_all <- natural_sort(samples)
  rank <- match(samples, sorted_all)

  # Order by group first, then by natural-sort rank
  idx <- order(groups, rank)
  samples[idx]
}


# ---- Plot 1: Sample total cell count ------------------------------------------

#' Plot 1 — Sample Total Cell Count
#'
#' Bar chart showing total cells per sample, coloured by group.
#'
#' @param sample_total data.frame from \code{build_sample_total()}
#' @param group_colors Named colour vector for groups
#' @return A plotly htmlwidget
#' @keywords internal
plot_sample_total_cells <- function(sample_total, group_colors) {
  groups      <- names(group_colors)
  group_cols  <- unname(group_colors)
  samples     <- sample_display_order(sample_total)

  sample_total$hover <- sprintf(
    "<b>%s</b><br>Group: %s<br>Total cells: %s",
    sample_total$sample, sample_total$group,
    fmt_num(sample_total$total_cells)
  )

  p <- plotly::plot_ly(
    data        = sample_total,
    x           = ~sample,
    y           = ~total_cells,
    color       = ~group,
    colors      = group_cols,
    type        = "bar",
    text        = ~hover,
    hoverinfo   = "text",
    hoverlabel  = list(bgcolor = "#2d3436", font = list(color = "#ffffff")),
    marker      = list(line = list(width = 0))
  )

  p <- plotly::layout(p,
    title    = list(text = "Sample total cell count", font = list(size = 14)),
    xaxis    = list(
      title = "", tickangle = -45,
      categoryorder = "array",
      categoryarray = samples
    ),
    yaxis    = list(title = "Number of cells"),
    margin   = list(l = 80, r = 30, b = 100, t = 40),
    legend   = list(title = list(text = "Group")),
    hovermode = "closest"
  )

  p <- plotly::config(p,
    displayModeBar = TRUE,
    displaylogo    = FALSE,
    modeBarButtons = list(list("toImage", "zoom2d", "pan2d",
                               "resetScale2d", "hoverClosestCartesian")),
    toImageButtonOptions = list(
      format = "png", filename = "sample_total_cells",
      height = 600, width = 1000
    )
  )
  p
}


# ---- Plot 2: Identity composition by sample -----------------------------------

#' Plot 2 — Identity Composition by Sample
#'
#' Stacked barplot showing identity proportions per sample.
#'
#' @param prop_table data.frame from \code{build_prop_table()}
#' @param identity_colors Named colour vector for identities
#' @return A plotly htmlwidget
#' @keywords internal
plot_composition_by_sample <- function(prop_table, identity_colors, sample_total = NULL) {
  id_names <- names(identity_colors)
  id_cols  <- unname(identity_colors)
  if (!is.null(sample_total)) {
    samples <- sample_display_order(sample_total)
  } else {
    samples <- levels(prop_table$sample)
  }

  prop_table$hover <- sprintf(
    "<b>%s</b><br>Sample: %s<br>Cells: %s / %s<br>Proportion: %.1f%%",
    prop_table$identity, prop_table$sample,
    fmt_num(prop_table$n_cells), fmt_num(prop_table$total_cells),
    prop_table$proportion * 100
  )

  p <- plotly::plot_ly(
    data        = prop_table,
    x           = ~sample,
    y           = ~proportion,
    color       = ~identity,
    colors      = id_cols,
    type        = "bar",
    text        = ~hover,
    hoverinfo   = "text",
    hoverlabel  = list(bgcolor = "#2d3436", font = list(color = "#ffffff"))
  )

  p <- plotly::layout(p,
    title    = list(text = "Identity composition by sample", font = list(size = 14)),
    xaxis    = list(
      title = "", tickangle = -45,
      categoryorder = "array", categoryarray = samples
    ),
    yaxis    = list(title = "Proportion", tickformat = ".0%", range = c(0, 1)),
    barmode  = "stack",
    bargap   = 0.25,
    margin   = list(l = 60, r = 30, b = 100, t = 40),
    legend   = list(title = list(text = "Identity"), traceorder = "normal"),
    hovermode = "closest"
  )

  p <- plotly::config(p,
    displayModeBar = TRUE,
    displaylogo    = FALSE,
    modeBarButtons = list(list("toImage", "zoom2d", "pan2d",
                               "resetScale2d", "hoverClosestCartesian")),
    toImageButtonOptions = list(
      format = "png", filename = "composition_by_sample",
      height = 600, width = 1000
    )
  )
  p
}


# ---- Plot 3: Identity proportion heatmap --------------------------------------

#' Plot 3 — Identity Proportion Heatmap
#'
#' Tile heatmap of identity proportions across samples.
#'
#' @param prop_table data.frame from \code{build_prop_table()}
#' @param count_table data.frame for identity ordering
#' @return A plotly htmlwidget
#' @keywords internal
plot_identity_proportion_heatmap <- function(prop_table, count_table = NULL, sample_total = NULL) {
  # Sort identity axis
  id_levels <- identity_display_order(levels(prop_table$identity), count_table)
  if (!is.null(sample_total)) {
    samples <- sample_display_order(sample_total)
  } else {
    samples <- levels(prop_table$sample)
  }

  # Build matrix
  mat <- matrix(0, nrow = length(id_levels), ncol = length(samples),
                dimnames = list(id_levels, samples))
  for (i in seq_len(nrow(prop_table))) {
    s <- as.character(prop_table$sample[i])
    id <- as.character(prop_table$identity[i])
    mat[id, s] <- prop_table$proportion[i]
  }

  p <- plotly::plot_ly(
    x          = samples,
    y          = id_levels,
    z          = mat,
    type       = "heatmap",
    colorscale = list(c(0, "#f8f9fc"), c(1, "#00b894")),
    hovertemplate = paste0(
      "Sample: %{x}<br>",
      "Identity: %{y}<br>",
      "Proportion: %{z:.1%}<br>",
      "<extra></extra>"
    ),
    colorbar   = list(title = "Proportion", tickformat = ".0%")
  )

  p <- plotly::layout(p,
    title  = list(text = "Identity proportion heatmap", font = list(size = 14)),
    xaxis  = list(title = "", tickangle = -45),
    yaxis  = list(title = "Identity"),
    margin = list(l = 120, r = 60, b = 120, t = 40)
  )

  p <- plotly::config(p,
    displayModeBar = TRUE, displaylogo = FALSE,
    modeBarButtons = list(list("toImage", "zoom2d", "pan2d",
                               "resetScale2d", "hoverClosestCartesian")),
    toImageButtonOptions = list(
      format = "png", filename = "identity_proportion_heatmap",
      height = 600, width = 1000
    )
  )
  p
}


# ---- Plot 4: Sample contribution within each identity -------------------------

#' Plot 4 — Sample Contribution within Each Identity
#'
#' Tile heatmap showing what fraction of each identity's cells
#' come from each sample.
#'
#' @param identity_sample_contribution data.frame from
#'   \code{build_identity_sample_contribution()}
#' @param count_table data.frame for identity sorting
#' @return A plotly htmlwidget
#' @keywords internal
plot_sample_contribution_heatmap <- function(identity_sample_contribution,
                                              count_table = NULL,
                                              sample_total = NULL) {
  ids     <- unique(identity_sample_contribution$identity)
  id_levels <- identity_display_order(ids, count_table)
  if (!is.null(sample_total)) {
    samples <- sample_display_order(sample_total)
  } else {
    samples <- natural_sort(unique(identity_sample_contribution$sample))
  }

  # Build matrix
  mat <- matrix(0, nrow = length(id_levels), ncol = length(samples),
                dimnames = list(id_levels, samples))
  for (i in seq_len(nrow(identity_sample_contribution))) {
    s  <- as.character(identity_sample_contribution$sample[i])
    id <- as.character(identity_sample_contribution$identity[i])
    mat[id, s] <- identity_sample_contribution$sample_contribution[i]
  }

  p <- plotly::plot_ly(
    x          = samples,
    y          = id_levels,
    z          = mat,
    type       = "heatmap",
    colorscale = list(c(0, "#f8f9fc"), c(1, "#e17055")),
    hovertemplate = paste0(
      "Sample: %{x}<br>",
      "Identity: %{y}<br>",
      "Contribution: %{z:.1%}<br>",
      "<extra></extra>"
    ),
    colorbar   = list(title = "Contribution", tickformat = ".0%")
  )

  p <- plotly::layout(p,
    title  = list(text = "Sample contribution within each identity",
                  font = list(size = 14)),
    xaxis  = list(title = "", tickangle = -45),
    yaxis  = list(title = "Identity"),
    margin = list(l = 120, r = 60, b = 120, t = 40)
  )

  p <- plotly::config(p,
    displayModeBar = TRUE, displaylogo = FALSE,
    modeBarButtons = list(list("toImage", "zoom2d", "pan2d",
                               "resetScale2d", "hoverClosestCartesian")),
    toImageButtonOptions = list(
      format = "png", filename = "sample_contribution_heatmap",
      height = 600, width = 1000
    )
  )
  p
}


# ---- Plot 5: Maximum sample contribution per identity -------------------------

#' Plot 5 — Maximum Sample Contribution per Identity
#'
#' Horizontal bar chart with dominance_threshold reference line.
#'
#' @param dominance_table data.frame from \code{build_dominance_table()}
#' @param dominance_threshold Threshold for the reference line
#' @return A plotly htmlwidget
#' @keywords internal
plot_max_sample_contribution <- function(dominance_table, dominance_threshold = 0.8) {
  dom <- dominance_table[order(dominance_table$max_sample_contribution), ]
  dom$identity <- factor(dom$identity, levels = dom$identity)

  dom$hover <- sprintf(
    "<b>%s</b><br>Dominant sample: %s (%s)<br>Cells: %s / %s<br>Max contribution: %.1f%%",
    dom$identity, dom$dominant_sample, dom$dominant_group,
    fmt_num(dom$dominant_n_cells), fmt_num(dom$identity_total_cells),
    dom$max_sample_contribution * 100
  )

  # Bar colours: red if above threshold
  bar_colors <- ifelse(
    dom$max_sample_contribution >= dominance_threshold,
    "#e17055", "#00b894"
  )

  p <- plotly::plot_ly(
    data        = dom,
    y           = ~identity,
    x           = ~max_sample_contribution,
    type        = "bar",
    orientation = "h",
    text        = ~hover,
    hoverinfo   = "text",
    hoverlabel  = list(bgcolor = "#2d3436", font = list(color = "#ffffff")),
    marker      = list(
      color = bar_colors,
      line  = list(width = 0)
    )
  )

  # Add threshold reference line via layout shapes (yref=paper, categorical-axis safe)
  p <- plotly::layout(p,
    title  = list(text = "Maximum sample contribution per identity",
                  font = list(size = 14)),
    xaxis  = list(
      title = "Max sample contribution",
      tickformat = ".0%",
      range = c(0, 1)
    ),
    yaxis  = list(title = ""),
    margin = list(l = 120, r = 30, b = 60, t = 40),
    shapes = list(
      list(
        type      = "line",
        x0        = dominance_threshold,
        x1        = dominance_threshold,
        y0        = 0,
        y1        = 1,
        yref      = "paper",
        line      = list(dash = "dash", color = "#d63031", width = 1.5)
      )
    ),
    annotations = list(
      list(
        x         = dominance_threshold,
        y         = 1.02,
        yref      = "paper",
        text      = paste0("threshold = ", dominance_threshold),
        showarrow = FALSE,
        font      = list(size = 10, color = "#d63031"),
        xanchor   = "left"
      )
    ),
    hovermode = "closest"
  )

  p <- plotly::config(p,
    displayModeBar = TRUE, displaylogo = FALSE,
    modeBarButtons = list(list("toImage", "zoom2d", "pan2d",
                               "resetScale2d", "hoverClosestCartesian")),
    toImageButtonOptions = list(
      format = "png", filename = "max_sample_contribution",
      height = 600, width = 1000
    )
  )
  p
}


# ---- Plot 6: Descriptive composition by group ---------------------------------

#' Plot 6 — Descriptive Identity Composition by Group
#'
#' Stacked barplot of mean sample-level proportions per group.
#' Purely descriptive — no statistical inference.
#'
#' @param group_summary data.frame from \code{build_group_summary()}
#' @param identity_colors Named colour vector for identities
#' @param warning_table data.frame for subtitle logic
#' @return A plotly htmlwidget
#' @keywords internal
plot_group_composition <- function(group_summary, identity_colors,
                                    warning_table = NULL) {
  id_names <- names(identity_colors)
  id_cols  <- unname(identity_colors)
  groups   <- unique(group_summary$group)

  # Build subtitle
  subtitle <- NULL
  has_small <- FALSE
  if (!is.null(warning_table) && nrow(warning_table) > 0) {
    has_small <- any(warning_table$warning_type == "group_n_less_than_2")
    if (has_small) {
      subtitle <- paste(
        "Caution: one or more groups have fewer than 2 samples;",
        "group-level summaries are descriptive only."
      )
    }
  }

  group_summary$hover <- sprintf(
    "<b>%s</b><br>Group: %s<br>Mean proportion: %.1f%%<br>Samples: %s  |  Cells: %s",
    group_summary$identity, group_summary$group,
    group_summary$mean_proportion * 100,
    group_summary$n_samples,
    fmt_num(group_summary$total_cells)
  )

  title_text <- "Descriptive identity composition by group"
  if (!is.null(subtitle)) {
    title_text <- paste0(title_text, "<br><sup>", subtitle, "</sup>")
  }

  p <- plotly::plot_ly(
    data        = group_summary,
    x           = ~group,
    y           = ~mean_proportion,
    color       = ~identity,
    colors      = id_cols,
    type        = "bar",
    text        = ~hover,
    hoverinfo   = "text",
    hoverlabel  = list(bgcolor = "#2d3436", font = list(color = "#ffffff"))
  )

  p <- plotly::layout(p,
    title    = list(
      text = title_text,
      font = list(size = 14)
    ),
    xaxis    = list(
      title = "",
      categoryorder = "array", categoryarray = groups
    ),
    yaxis    = list(
      title = "Mean proportion (of sample-level proportions)",
      tickformat = ".0%", range = c(0, 1)
    ),
    barmode  = "stack",
    bargap   = 0.25,
    margin   = list(l = 80, r = 30, b = 80, t = 60),
    legend   = list(title = list(text = "Identity"), traceorder = "normal"),
    hovermode = "closest"
  )

  p <- plotly::config(p,
    displayModeBar = TRUE,
    displaylogo    = FALSE,
    modeBarButtons = list(list("toImage", "zoom2d", "pan2d",
                               "resetScale2d", "hoverClosestCartesian")),
    toImageButtonOptions = list(
      format = "png", filename = "composition_by_group",
      height = 600, width = 1000
    )
  )
  p
}


# ---- Plot 7: Sample-level identity proportions by group -----------------------

#' Plot 7 — Sample-Level Identity Proportions by Group
#'
#' Jitter + mean crossbar per identity x group.
#' Each point is a sample, not a cell.
#'
#' @param prop_table data.frame from \code{build_prop_table()}
#' @param group_colors Named colour vector for groups
#' @param warning_table data.frame for subtitle logic
#' @return A plotly htmlwidget
#' @keywords internal
plot_sample_identity_by_group <- function(prop_table, group_colors,
                                           warning_table = NULL) {
  group_names <- names(group_colors)
  group_cols  <- unname(group_colors)
  identities  <- levels(prop_table$identity)
  groups      <- unique(as.character(prop_table$group))

  # Subtitle
  subtitle <- "Each point is one sample; groups with n < 2 are descriptive only."
  if (!is.null(warning_table) && nrow(warning_table) > 0) {
    has_small <- any(warning_table$warning_type == "group_n_less_than_2")
    if (has_small) {
      subtitle <- paste(
        "Each point is one sample.",
        "One or more groups have < 2 samples; all results are descriptive only."
      )
    }
  }

  # Filter zero-total rows
  plot_data <- prop_table[prop_table$total_cells > 0, ]

  plot_data$hover <- sprintf(
    "<b>%s</b><br>Sample: %s<br>Group: %s<br>Proportion: %.1f%%",
    plot_data$identity, plot_data$sample, plot_data$group,
    plot_data$proportion * 100
  )

  # Build as faceted scatter with mean crossbars using subplots
  n_ids <- length(identities)

  # For each identity, create a subplot with jitter + mean
  subplot_list <- list()
  annotations   <- list()

  for (i in seq_along(identities)) {
    id <- identities[i]
    id_data <- plot_data[plot_data$identity == id, ]

    if (nrow(id_data) == 0) next

    # Jitter x positions
    set.seed(42)
    group_to_num <- setNames(seq_along(groups), groups)
    id_data$x_num <- group_to_num[as.character(id_data$group)] +
      stats::runif(nrow(id_data), -0.2, 0.2)

    # Compute mean per group
    agg <- stats::aggregate(proportion ~ group, data = id_data, FUN = mean)
    agg$x_num <- group_to_num[as.character(agg$group)]

    p_id <- plotly::plot_ly(
      data        = id_data,
      x           = ~x_num,
      y           = ~proportion,
      color       = ~group,
      colors      = group_cols,
      type        = "scatter",
      mode        = "markers",
      text        = ~hover,
      hoverinfo   = "text",
      hoverlabel  = list(bgcolor = "#2d3436", font = list(color = "#ffffff")),
      marker      = list(size = 8, opacity = 0.65,
                         line = list(width = 0.5, color = "#ffffff")),
      legendgroup = ~group,
      showlegend  = (i == 1)
    )

    # Add mean crossbars
    for (j in seq_len(nrow(agg))) {
      g_num <- agg$x_num[j]
      mn    <- agg$proportion[j]
      p_id <- plotly::add_segments(p_id,
        x    = g_num - 0.35,
        xend = g_num + 0.35,
        y    = mn,
        yend = mn,
        line  = list(color = "#2d3436", width = 2),
        showlegend = FALSE,
        hoverinfo  = "none"
      )
    }

    p_id <- plotly::layout(p_id,
      xaxis = list(
        title    = if (i == n_ids) "Group" else "",
        tickvals = group_to_num,
        ticktext = names(group_to_num),
        range    = c(0.3, length(groups) + 0.7)
      ),
      yaxis = list(
        title    = "Proportion",
        tickformat = ".0%"
      ),
      annotations = list(list(
        text = paste0("<b>", id, "</b>"),
        x = 0.5, y = 1.05,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = 11, color = "#2d3436")
      ))
    )

    subplot_list[[length(subplot_list) + 1]] <- p_id
  }

  # Use subplot
  n_plots <- length(subplot_list)
  if (n_plots == 0) return(NULL)

  n_cols <- min(4, n_plots)
  n_rows <- ceiling(n_plots / n_cols)

  p <- plotly::subplot(
    subplot_list,
    nrows    = n_rows,
    shareX   = TRUE,
    shareY   = FALSE,
    titleX   = TRUE,
    titleY   = TRUE
  )

  p <- plotly::layout(p,
    title = list(
      text = paste0(
        "Sample-level identity proportions by group<br>",
        "<sup>", subtitle, "</sup>"
      ),
      font = list(size = 14)
    ),
    margin = list(l = 60, r = 30, b = 60, t = 80),
    hovermode = "closest"
  )

  p <- plotly::config(p,
    displayModeBar = TRUE, displaylogo = FALSE,
    modeBarButtons = list(list("toImage", "zoom2d", "pan2d",
                               "resetScale2d", "hoverClosestCartesian")),
    toImageButtonOptions = list(
      format = "png", filename = "sample_level_by_group",
      height = 600, width = 1000
    )
  )
  p
}


# ---- Build all plots ----------------------------------------------------------

#' Build All Core Composition Audit Plots
#'
#' Generates all 7 plots from intermediate tables.
#'
#' @param tables Named list from \code{build_all_composition_tables()}
#' @param group_colors Named colour vector for groups
#' @param identity_colors Named colour vector for identities
#' @param warning_table data.frame from \code{build_warning_table()}
#' @param dominance_threshold Threshold for Plot 5 reference line
#' @return A named list of plotly htmlwidgets
#' @keywords internal
build_all_composition_plots <- function(tables,
                                         group_colors,
                                         identity_colors,
                                         warning_table,
                                         dominance_threshold = 0.8) {
  message("Generating 7 core composition audit plots...")

  plots <- list()

  message("  Plot 1: Sample total cell count")
  plots$sample_total <- plot_sample_total_cells(
    tables$sample_total, group_colors
  )

  message("  Plot 2: Identity composition by sample")
  plots$composition_by_sample <- plot_composition_by_sample(
    tables$prop_table, identity_colors, tables$sample_total
  )

  message("  Plot 3: Identity proportion heatmap")
  plots$proportion_heatmap <- plot_identity_proportion_heatmap(
    tables$prop_table, tables$count_table, tables$sample_total
  )

  message("  Plot 4: Sample contribution heatmap")
  plots$sample_contribution_heatmap <- plot_sample_contribution_heatmap(
    tables$identity_sample_contribution, tables$count_table, tables$sample_total
  )

  message("  Plot 5: Maximum sample contribution")
  plots$max_sample_contribution <- plot_max_sample_contribution(
    tables$dominance_table, dominance_threshold
  )

  message("  Plot 6: Descriptive composition by group")
  plots$composition_by_group <- plot_group_composition(
    tables$group_summary, identity_colors, warning_table
  )

  message("  Plot 7: Sample-level identity proportions by group")
  plots$sample_level_by_group <- plot_sample_identity_by_group(
    tables$prop_table, group_colors, warning_table
  )

  message("All 7 plots generated.")
  plots
}
