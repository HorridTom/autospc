# Preparing analysed charts for drawing

#' Build the plot data of each chart
#'
#' One set per chart, in the order the charts are drawn.
#'
#' @param charts A list of analysed `autospc_chart` objects.
#' @param parameters A named list of the presentation parameters.
#'
#' @return A list of plot data, as `plot_data_for_chart()` gives it.
#' @noRd
build_plot_data <- function(charts,
                            parameters) {

  return(lapply(charts, plot_data_for_chart, parameters = parameters))

}


#' The plot data of one chart
#'
#' Four elements: the `chart`, its `table`, the `derived` axis extents, and the
#' `axis_titles` the chart resolved from its class.
#'
#' `table` is `chart$result$table` - the analysis - with the columns only the
#' drawing uses added to it: the exclusion highlights, the floating median, the
#' centre line labels and their arrows, and the rows the limits are extended
#' over. It is neither `chart$data`, which is the series the algorithm ran on,
#' nor `chart$result$table`, which is what the algorithm produced. Which columns
#' it has depends on the presentation parameters, which is why it is here rather
#' than on the chart.
#'
#' The chart comes back with it so that what is drawn and what it was drawn from
#' travel together.
#'
#' @param chart An analysed `autospc_chart`.
#' @param parameters A named list of the presentation parameters.
#'
#' @return A list of the `chart`, its `table`, the `derived` axis extents and
#'   the `axis_titles`.
#' @noRd
plot_data_for_chart <- function(chart,
                                parameters) {

  table <- chart$result$table

  axes <- axis_values(data = table,
                      chart = chart,
                      parameters = parameters)

  if(parameters$show_limits && centre_line_present(table)) {

    table <- postprocess_spc(data = table,
                             chart = chart,
                             parameters = parameters,
                             derived = axes$derived)

  }

  return(list(chart = chart,
              table = table,
              derived = axes$derived,
              axis_titles = axes$axis_titles))

}


#' The plot data of a faceted plot
#'
#' Every facet in one table, and the axis values taken from all of them.
#'
#' @param plot_data The facets' plot data, as `build_plot_data()` gives it.
#' @param parameters A named list of the presentation parameters.
#'
#' @return Plot data, in the shape `plot_data_for_chart()` gives.
#' @noRd
faceted_plot_data <- function(plot_data,
                              parameters) {

  table <- combine_plot_data(plot_data = plot_data,
                             parameters = parameters)

  # Every facet is the same kind of chart, so the axes are taken from the last.
  chart <- plot_data[[length(plot_data)]]$chart

  axes <- axis_values(data = table,
                      chart = chart,
                      parameters = parameters)

  return(list(chart = chart,
              table = table,
              derived = axes$derived,
              axis_titles = axes$axis_titles))

}


#' The charts as one table
#'
#' What `plot_chart = FALSE` returns.
#'
#' @param charts A list of analysed `autospc_chart` objects.
#' @param parameters A named list of the presentation parameters.
#'
#' @return A data frame.
#' @noRd
combined_plot_data <- function(charts,
                               parameters) {

  return(combine_plot_data(plot_data = build_plot_data(charts, parameters),
                           parameters = parameters))

}


#' The plot data of several charts as one table
#'
#' An XmR pair goes out wide, the moving range and its limits beside the X
#' columns. The facets of a faceted chart stack long, with `stage` saying which
#' each row came from. The rows an SPC chart does not draw - the ones with no
#' `x` - are dropped from each chart that has limits.
#'
#' @param plot_data The charts' plot data, as `build_plot_data()` gives it.
#' @param parameters A named list of the presentation parameters.
#'
#' @return A data frame.
#' @noRd
combine_plot_data <- function(plot_data,
                              parameters) {

  charts <- lapply(plot_data, function(each) each$chart)

  if(length(plot_data) > 1L && !is_xmr_pair(charts)) {
    # Faceted plot
    stages <- lapply(plot_data, function(each) {
      
      if(parameters$show_limits && centre_line_present(each$table)) {
        return(dplyr::filter(each$table, !is.na(x)))
      }
      
      return(each$table)
      
    })
    # Return for faceted plot
    return(dplyr::bind_rows(stages, .id = "stage"))
    
  }
  
  # The facets have returned above, so what is left is one chart, or the
  # location half of a pair with the dispersion half joined on.
  main <- plot_data[[1]]

  data <- main$table

  if(!(parameters$show_limits && centre_line_present(data))) {
    return(data)
  }

  if(is_xmr_pair(charts)) {
    data <- join_mr_columns(x_table = data,
                            mr_table = plot_data$dispersion$table)
  }

  return(dplyr::filter(data, !is.na(x)))

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
    dplyr::left_join(mr_table %>%
                       dplyr::filter(!is.na(x)) %>%
                       dplyr::select(x,
                                    mr = y,
                                    amr = cl,
                                    url = ucl,
                                    lrl = lcl),
                     by = c("x" = "x")) %>%
    dplyr::select(x, y, cl, ucl, lcl,
                  mr, amr, url, lrl,
                  dplyr::everything())

  return(joined)

}


#' The axis extents and axis titles a table is drawn with
#'
#' The table is passed in rather than read from the chart, because a faceted
#' plot draws every facet from one table and takes its axes from all of them.
#'
#' @param data The table to be drawn.
#' @param chart The `autospc_chart` the vertical axis is taken from.
#' @param parameters A named list of the presentation parameters.
#'
#' @return A list of the `derived` axis extents and the `axis_titles`.
#' @noRd
axis_values <- function(data,
                        chart,
                        parameters) {

  x_pad_end <- parameters$x_pad_end

  if(!is.null(parameters$extend_limits_to) && is.null(x_pad_end)) {
    x_pad_end <- parameters$extend_limits_to
  }

  start_x <- min(data$x, na.rm = TRUE)
  x_max <- max(data$x, na.rm = TRUE)
  end_x <- max(x_max, x_pad_end)

  if(!centre_line_present(data)) {
    ylimlow <- min(data$y, na.rm = TRUE)
    ylimhigh <- max(data$y, na.rm = TRUE)
  } else {
    y_range <- y_axis_range(chart = chart,
                            data = data)
    ylimlow <- y_range$low
    ylimhigh <- y_range$high
  }

  if(!is.null(parameters$override_y_lim)) {
    ylimhigh <- parameters$override_y_lim
  }

  # The axis titles the caller did not give come from the chart
  x_title <- parameters$override_x_title
  y_title <- parameters$override_y_title

  if(is.null(x_title)) {
    x_title <- "Day"
  }

  if(is.null(y_title)) {
    y_title <- y_axis_title(chart)
  }

  return(list(derived = list(start_x = start_x,
                             x_max = x_max,
                             end_x = end_x,
                             ylimlow = ylimlow,
                             ylimhigh = ylimhigh),
              axis_titles = list(x = x_title,
                                 y = y_title)))

}


#' The columns a chart with limits is drawn from
#'
#' The exclusion highlights, the floating median, the centre line labels and
#' their arrows, and the limits extended out to `extend_limits_to`.
#'
#' @param data The analysed plot data.
#' @param chart The analysed `autospc_chart`.
#' @param parameters A named list of the presentation parameters.
#' @param derived The axis extents, as `axis_values()` gives them.
#'
#' @return A data frame.
#' @noRd
postprocess_spc <- function(data,
                            chart,
                            parameters,
                            derived) {

  if(parameters$highlight_exclusions) {
    data <- data %>% dplyr::mutate(
      highlight = ifelse(excluded & !is.na(excluded),
                         "Excluded from limits calculation",
                         highlight)
    )
  }

  data <- floating_median_column(df = data,
                                 floating_median = chart$floating_median,
                                 floating_median_n = chart$floating_median_n)

  data <- add_annotation_data(
    df = data,
    chart = chart,
    ylimhigh = derived$ylimhigh,
    align_labels = parameters$align_labels,
    flip_labels = parameters$flip_labels,
    upper_annotation_sf = parameters$upper_annotation_sf,
    lower_annotation_sf = parameters$lower_annotation_sf,
    annotation_arrow_curve = parameters$annotation_arrow_curve)

  data <- extend_limits(df = data,
                        chart = chart,
                        extend_limits_to = parameters$extend_limits_to,
                        x_max = derived$x_max)

  return(data)

}


#' Does this table carry a centre line?
#'
#' The algorithm returns a table with no `cl`, `ucl` or `lcl` when there were
#' too few points to form a period, so the presence of `cl` answers whether it
#' produced anything to draw.
#'
#' @return TRUE or FALSE
#' @noRd
centre_line_present <- function(data) {
  
  return("cl" %in% colnames(data))
  
}
