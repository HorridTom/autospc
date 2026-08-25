# Preparing analysed charts for drawing

#' The frames the charts are drawn from
#'
#' One per chart, in the order the charts are drawn.
#'
#' @param charts A list of analysed `autospc_chart` objects.
#' @param passed A named list of the presentation parameters.
#'
#' @return A list of frames, as `drawable_frame()` gives them.
#' @noRd
drawable_frames <- function(charts,
                            passed) {

  return(lapply(charts, drawable_frame, passed = passed))

}


#' The frame a chart is drawn from, and what its axes need
#'
#' The chart carries the analysis; the frame is the analysis with the columns
#' the drawing needs added, which is why it depends on the presentation
#' parameters as well as on the chart.
#'
#' @param chart An analysed `autospc_chart`.
#' @param passed A named list of the presentation parameters.
#'
#' @return A list of the `chart`, the drawable `data`, the `derived` axis
#'   extents, and the `axis_titles` the chart resolved from its class.
#' @noRd
drawable_frame <- function(chart,
                           passed) {

  data <- chart$result$table

  axes <- axis_values(data = data,
                      chart = chart,
                      passed = passed)

  if(passed$show_limits && centre_line_present(data)) {

    data <- postprocess_spc(data = data,
                            chart = chart,
                            passed = passed,
                            derived = axes$derived)

  }

  return(list(chart = chart,
              data = data,
              derived = axes$derived,
              axis_titles = axes$axis_titles))

}


#' The frame a faceted plot is drawn from
#'
#' Every facet in one frame, and the axis values taken from all of them.
#'
#' @param charts A list of analysed `autospc_chart` objects, one per facet.
#' @param passed A named list of the presentation parameters.
#' @param frames The facets' frames, where the caller already has them.
#'
#' @return A frame, in the shape `drawable_frame()` gives.
#' @noRd
faceted_frame <- function(charts,
                          passed,
                          frames = drawable_frames(charts, passed)) {

  data <- drawable_table(charts = charts,
                         passed = passed,
                         frames = frames)

  # Every facet is the same kind of chart, so the axes are taken from the last.
  chart <- charts[[length(charts)]]

  axes <- axis_values(data = data,
                      chart = chart,
                      passed = passed)

  return(list(chart = chart,
              data = data,
              derived = axes$derived,
              axis_titles = axes$axis_titles))

}


#' The charts' frames as one table
#'
#' What `plot_chart = FALSE` returns, and what a faceted plot is drawn from.
#'
#' An XmR pair goes out wide, the moving range and its limits beside the X
#' columns. The facets of a faceted chart stack long, with `stage` saying which
#' each row came from. The rows an SPC chart does not draw - the ones with no
#' `x` - are dropped from each chart that has limits.
#'
#' @param charts A list of analysed `autospc_chart` objects.
#' @param passed A named list of the presentation parameters.
#' @param frames The charts' frames, where the caller already has them.
#'
#' @return A data frame.
#' @noRd
drawable_table <- function(charts,
                           passed,
                           frames = drawable_frames(charts, passed)) {

  if(length(frames) > 1L && !is_xmr_pair(charts)) {

    stages <- lapply(frames, function(frame) {

      if(passed$show_limits && centre_line_present(frame$data)) {
        return(dplyr::filter(frame$data, !is.na(x)))
      }

      return(frame$data)

    })

    return(dplyr::bind_rows(stages, .id = "stage"))

  }

  data <- frames[[1]]$data

  if(!(passed$show_limits && centre_line_present(data))) {
    return(data)
  }

  if(is_xmr_pair(charts)) {
    data <- join_mr_columns(x_table = data,
                            mr_table = frames[[2]]$data)
  }

  return(dplyr::filter(data, !is.na(x)))

}


#' The axis extents and axis titles a frame is drawn with
#'
#' The frame is passed in rather than read from the chart, because a faceted
#' plot draws every facet from one frame and takes its axes from all of them.
#'
#' @param data The frame to be drawn.
#' @param chart The `autospc_chart` the vertical axis is taken from.
#' @param passed A named list of the presentation parameters.
#'
#' @return A list of the `derived` axis extents and the `axis_titles`.
#' @noRd
axis_values <- function(data,
                        chart,
                        passed) {

  x_pad_end <- passed$x_pad_end

  if(!is.null(passed$extend_limits_to) && is.null(x_pad_end)) {
    x_pad_end <- passed$extend_limits_to
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

  if(!is.null(passed$override_y_lim)) {
    ylimhigh <- passed$override_y_lim
  }

  # The axis titles the caller did not give come from the chart
  x_title <- passed$override_x_title
  y_title <- passed$override_y_title

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
#' @param data The analysed frame.
#' @param chart The analysed `autospc_chart`.
#' @param passed A named list of the presentation parameters.
#' @param derived The axis extents, as `axis_values()` gives them.
#'
#' @return A data frame.
#' @noRd
postprocess_spc <- function(data,
                            chart,
                            passed,
                            derived) {

  if(passed$highlight_exclusions) {
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
    align_labels = passed$align_labels,
    flip_labels = passed$flip_labels,
    upper_annotation_sf = passed$upper_annotation_sf,
    lower_annotation_sf = passed$lower_annotation_sf,
    annotation_arrow_curve = passed$annotation_arrow_curve)

  data <- extend_limits(df = data,
                        chart = chart,
                        extend_limits_to = passed$extend_limits_to,
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
