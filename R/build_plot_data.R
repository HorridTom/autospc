# Preparing analysed charts for drawing

#' Build the plot data of each chart
#'
#' One set per chart, in the order the charts are drawn.
#'
#' @param charts A list of analysed `autospc_chart` objects.
#' @param visualisation_params A named list of the visualisation parameters.
#'
#' @return A list of plot data, as `plot_data_for_chart()` gives it.
#' @noRd
build_plot_data <- function(charts,
                            visualisation_params) {
  return(lapply(charts,
    plot_data_for_chart,
    visualisation_params = visualisation_params
  ))
}


#' The plot data of one chart
#'
#' Four elements: the `chart`, its `table`, its `axis_extents`, and the
#' `axis_titles` the chart resolved from its class.
#'
#' `table` is `chart$result$table` - the analysis - with the columns only the
#' drawing uses added to it: the exclusion highlights, the floating median, the
#' centre line labels and their arrows, and the rows the limits are extended
#' over. It is neither `chart$data`, which is the series the algorithm ran on,
#' nor `chart$result$table`, which is what the algorithm produced. Which columns
#' it has depends on the visualisation parameters, which is why it is here
#' rather than on the chart.
#'
#' The chart comes back with it so that what is drawn and what it was drawn from
#' travel together.
#'
#' @param chart An analysed `autospc_chart`.
#' @param visualisation_params A named list of the visualisation parameters.
#'
#' @return A list of the `chart`, its `table`, its `axis_extents` and the
#'   `axis_titles`.
#' @noRd
plot_data_for_chart <- function(chart,
                                visualisation_params) {
  table <- chart$result$table

  axes <- axis_specifications(
    table = table,
    chart = chart,
    visualisation_params = visualisation_params
  )

  if (visualisation_params$show_limits && enough_data_for_limits(chart)) {
    table <- add_plot_columns(
      table = table,
      chart = chart,
      visualisation_params = visualisation_params,
      axis_extents = axes$axis_extents
    )
  }

  return(list(
    chart = chart,
    table = table,
    axis_extents = axes$axis_extents,
    axis_titles = axes$axis_titles
  ))
}


#' The plot data of a faceted plot
#'
#' Every facet in one table, and the axis values taken from all of them.
#'
#' @param plot_data The facets' plot data, as `build_plot_data()` gives it.
#' @param visualisation_params A named list of the visualisation parameters.
#'
#' @return Plot data, in the shape `plot_data_for_chart()` gives.
#' @noRd
faceted_plot_data <- function(plot_data,
                              visualisation_params) {
  table <- combine_plot_data(
    plot_data = plot_data,
    visualisation_params = visualisation_params,
    faceted = TRUE
  )

  # Every facet is the same kind of chart, so the axes are taken from the last.
  chart <- plot_data[[length(plot_data)]]$chart

  axes <- axis_specifications(
    table = table,
    chart = chart,
    visualisation_params = visualisation_params
  )

  return(list(
    chart = chart,
    table = table,
    axis_extents = axes$axis_extents,
    axis_titles = axes$axis_titles
  ))
}


#' The charts as one table
#'
#' What `plot_chart = FALSE` returns.
#'
#' @param charts A list of analysed `autospc_chart` objects.
#' @param visualisation_params A named list of the visualisation parameters.
#' @param faceted TRUE where the charts are the stages of a faceted plot.
#'
#' @return A data frame.
#' @noRd
charts_as_table <- function(charts,
                            visualisation_params,
                            faceted = FALSE) {
  plot_data <- build_plot_data(
    charts = charts,
    visualisation_params = visualisation_params
  )

  return(combine_plot_data(
    plot_data = plot_data,
    visualisation_params = visualisation_params,
    faceted = faceted
  ))
}


#' The plot data of several charts as one table
#'
#' An XmR pair goes out wide, the moving range and its limits beside the X
#' columns. The facets of a faceted chart stack long, with `stage` saying which
#' each row came from. The rows an SPC chart does not draw - the ones with no
#' `x` - are dropped from each chart that has limits.
#'
#' @param plot_data The charts' plot data, as `build_plot_data()` gives it.
#' @param visualisation_params A named list of the visualisation parameters.
#' @param faceted TRUE where the charts are the stages of a faceted plot. A
#'   faceted plot of one stage is still faceted, so this is not the number of
#'   charts.
#'
#' @return A data frame.
#' @noRd
combine_plot_data <- function(plot_data,
                              visualisation_params,
                              faceted = FALSE) {
  charts <- lapply(plot_data, function(each) each$chart)

  if (faceted) {
    stages <- lapply(plot_data, function(each) {
      if (visualisation_params$show_limits &&
        enough_data_for_limits(each$chart)) {
        return(each$table)
      }

      return(each$table)
    })

    return(dplyr::bind_rows(stages, .id = "stage"))
  }

  # The facets have returned above, so what is left is one chart, or the
  # location half of a pair with the dispersion half joined on.
  main <- plot_data[[1]]

  data <- main$table

  if (!(visualisation_params$show_limits &&
    enough_data_for_limits(main$chart))) {
    return(data)
  }

  if (is_xmr_pair(charts)) {
    data <- join_mr_columns(
      x_table = data,
      mr_table = plot_data$dispersion$table
    )
  }

  return(data)
}


#' Join the moving range analysis onto the X analysis
#'
#' An XmR pair is one analysis of one series shown as two charts, so it goes
#' out wide: the moving range and its limits sit beside the X columns as `mr`,
#' `amr`, `url` and `lrl`.
#'
#' @return A data frame.
#' @noRd
join_mr_columns <- function(x_table,
                            mr_table) {
  joined <- x_table %>%
    dplyr::left_join(
      mr_table %>%
        dplyr::select(x,
          mr = y,
          amr = cl,
          url = ucl,
          lrl = lcl
        ),
      by = c("x" = "x")
    ) %>%
    dplyr::select(
      x, y, cl, ucl, lcl,
      mr, amr, url, lrl,
      dplyr::everything()
    )

  return(joined)
}


#' The axis extents and axis titles a table is drawn with
#'
#' The table is passed in rather than read from the chart, because a faceted
#' plot draws every facet from one table and takes its axes from all of them.
#'
#' @param data The table to be drawn.
#' @param chart The `autospc_chart` the vertical axis is taken from.
#' @param visualisation_params A named list of the visualisation parameters.
#'
#' @return A list of the `axis_extents` and the `axis_titles`.
#' @noRd
axis_specifications <- function(table,
                                chart,
                                visualisation_params) {
  x_pad_end <- visualisation_params$x_pad_end

  if (!is.null(visualisation_params$extend_limits_to) && is.null(x_pad_end)) {
    x_pad_end <- visualisation_params$extend_limits_to
  }

  start_x <- min(table$x, na.rm = TRUE)
  x_max <- max(table$x, na.rm = TRUE)
  end_x <- max(x_max, x_pad_end)

  if (!enough_data_for_limits(chart)) {
    ylimlow <- min(table$y, na.rm = TRUE)
    ylimhigh <- max(table$y, na.rm = TRUE)
  } else {
    y_range <- y_axis_range(
      chart = chart,
      data = table
    )
    ylimlow <- y_range$low
    ylimhigh <- y_range$high
  }

  if (!is.null(visualisation_params$override_y_lim)) {
    ylimhigh <- visualisation_params$override_y_lim
  }

  # The y axis title comes from the chart where the caller gave none. The x
  # axis has a title only if the caller gave one.
  x_title <- visualisation_params$override_x_title
  y_title <- visualisation_params$override_y_title

  if (is.null(y_title)) {
    y_title <- y_axis_title(chart)
  }

  return(list(
    axis_extents = list(
      start_x = start_x,
      x_max = x_max,
      end_x = end_x,
      ylimlow = ylimlow,
      ylimhigh = ylimhigh
    ),
    axis_titles = list(
      x = x_title,
      y = y_title
    )
  ))
}


#' The columns a chart with limits is drawn from
#'
#' The exclusion highlights, the floating median, the centre line labels and
#' their arrows, and the limits extended out to `extend_limits_to`.
#'
#' @param data The analysed plot data.
#' @param chart The analysed `autospc_chart`.
#' @param visualisation_params A named list of the visualisation parameters.
#' @param axis_extents The axis extents, as `axis_specifications()` gives them.
#'
#' @return A data frame.
#' @noRd
add_plot_columns <- function(table,
                             chart,
                             visualisation_params,
                             axis_extents) {
  if (visualisation_params$highlight_exclusions) {
    table <- table %>% dplyr::mutate(
      highlight = ifelse(excluded & !is.na(excluded),
        "Excluded from limits calculation",
        highlight
      )
    )
  }

  table <- floating_median_column(
    table = table,
    floating_median = chart$floating_median,
    floating_median_n = chart$floating_median_n
  )

  table <- add_annotation_data(
    table = table,
    chart = chart,
    ylimhigh = axis_extents$ylimhigh,
    align_labels = visualisation_params$align_labels,
    flip_labels = visualisation_params$flip_labels,
    upper_annotation_sf = visualisation_params$upper_annotation_sf,
    lower_annotation_sf = visualisation_params$lower_annotation_sf,
    annotation_arrow_curve = visualisation_params$annotation_arrow_curve
  )

  table <- extend_limits(
    table = table,
    chart = chart,
    extend_limits_to =
      visualisation_params$extend_limits_to,
    x_max = axis_extents$x_max
  )

  return(table)
}
