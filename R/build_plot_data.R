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
#' drawing uses added to it: the exclusion highlights, and the centre line
#' labels and their arrows. Which columns it has depends on the visualisation
#' parameters, which is why it is here rather than on the chart.
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
#' What `plot_chart = FALSE` returns, and what `as.data.frame()` on a plot
#' returns. Each chart's analysed table, combined: an XmR pair goes out wide
#' and the stages of a faceted plot stack long.
#'
#' The columns `add_plot_columns()` adds are not here. They exist to place the
#' centre line labels on a plot, so a call that asks for a table rather than a
#' plot has no use for them.
#'
#' @param charts A list of analysed `autospc_chart` objects.
#' @param faceted TRUE where the charts are the stages of a faceted plot.
#'
#' @return A data frame.
#' @noRd
charts_as_table <- function(charts,
                            faceted = FALSE) {
  analysed <- lapply(charts, function(chart) {
    return(list(
      chart = chart,
      table = chart$result$table
    ))
  })

  return(combine_plot_data(
    plot_data = analysed,
    faceted = faceted
  ))
}


#' The plot data of several charts as one table
#'
#' An XmR pair goes out wide, the moving range and its limits beside the X
#' columns. The facets of a faceted chart stack long, with `stage` saying which
#' each row came from.
#'
#' @param plot_data The charts' plot data, as `build_plot_data()` gives it.
#' @param faceted TRUE where the charts are the stages of a faceted plot. A
#'   faceted plot of one stage is still faceted, so this is not the number of
#'   charts.
#'
#' @return A data frame.
#' @noRd
combine_plot_data <- function(plot_data,
                              faceted = FALSE) {
  if (faceted) {
    stages <- lapply(plot_data, function(each) each$table)

    return(dplyr::bind_rows(stages, .id = "stage"))
  }

  # The facets have returned above, so what is left is one chart, or the
  # location half of a pair with the dispersion half joined on.
  main <- plot_data[[1]]

  charts <- lapply(plot_data, function(each) each$chart)

  # the moving range half has no limits to join on where there were too few
  # points to form a period
  if (!(is_xmr_pair(charts) && enough_data_for_limits(main$chart))) {
    return(main$table)
  }

  return(join_mr_columns(
    x_table = main$table,
    mr_table = plot_data$dispersion$table
  ))
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
          mr = series,
          amr = cl,
          url = ucl,
          lrl = lcl
        ),
      by = c("x" = "x")
    ) %>%
    dplyr::select(
      x, series, y, cl, ucl, lcl,
      mr, amr, url, lrl,
      dplyr::everything()
    )

  return(joined)
}


#' The horizontal axis values of the subgroups
#'
#' `extend_limits_to` adds rows beyond the end of the data, so the largest `x`
#' in the table is not always the largest `x` of a subgroup. `limit_extension`
#' says which rows those added ones are, and is absent from a table that has no
#' limits, where no rows have been added.
#'
#' @param table The table to be drawn.
#'
#' @return The `x` column, without the rows the extension added.
#' @noRd
x_of_the_data <- function(table) {
  if (!"limit_extension" %in% names(table)) {
    return(table$x)
  }

  return(table$x[!table$limit_extension])
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

  if (!is.null(chart$extend_limits_to) && is.null(x_pad_end)) {
    x_pad_end <- chart$extend_limits_to
  }

  start_x <- min(table$x, na.rm = TRUE)
  x_max <- max(x_of_the_data(table), na.rm = TRUE)
  end_x <- max(x_max, x_pad_end)

  if (!enough_data_for_limits(chart)) {
    ylimlow <- min(table$series, na.rm = TRUE)
    ylimhigh <- max(table$series, na.rm = TRUE)
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
#' The exclusion highlights, and the centre line labels and their arrows.
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

  return(table)
}
