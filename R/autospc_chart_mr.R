# autospc_chart_mr class

#' Construct an autospc_chart_mr object
#'
#' @return An object of class `c("autospc_chart_mr", "autospc_chart")`.
#' @noRd
new_autospc_chart_mr <- function(x) {

  return(
    new_autospc_chart(x,
                      class = "autospc_chart_mr")
  )

}


#' Validate an autospc_chart_mr object
#'
#' See `validate_autospc_chart()` for the class contract.
#'
#' @return `x`, unchanged, if valid; otherwise an error.
#' @noRd
validate_autospc_chart_mr <- function(x) {

  if(!inherits(x, "autospc_chart_mr")) {
    stop("Not an autospc_chart_mr object.", call. = FALSE)
  }

  x <- validate_autospc_chart(x)

  require_column(data = x$data,
                 column = "y",
                 message = paste("y not specified. For X, MR and XMR charts, y",
                                 "must be specified."))

  require_column_type(data = x$data,
                      column = "y",
                      types = c("integer", "double"),
                      message = paste("For X, MR and XMR charts, y must be of",
                                      "type integer or double."))

  return(x)

}


#' Create an autospc_chart_mr object
#'
#' Helper for MR charts: assemble, construct, validate, round, return.
#'
#' @return An object of class `c("autospc_chart_mr", "autospc_chart")`.
#' @noRd
autospc_chart_mr <- function(data,
                             x,
                             y,
                             ...) {

  autospc_chart_mr_l <- autospc_chart_list(data = data,
                                           x = x,
                                           y = y,
                                           ...)

  autospc_chart_mr_l <- normalise_columns(autospc_chart_mr_l,
                                          fields = c("x", "y"))

  autospc_chart_mr_object <- new_autospc_chart_mr(autospc_chart_mr_l)

  autospc_chart_mr_object <- validate_autospc_chart_mr(autospc_chart_mr_object)

  autospc_chart_mr_object <- round_counts(autospc_chart_mr_object)

  return(autospc_chart_mr_object)

}


# Analysis methods

# No aggregate_data() method: MR charts plot one moving range per observation, so
# the superclass default - return the chart unchanged - is the correct behaviour.

#' Turn the aggregated data into the series the algorithm analyses
#'
#' An MR chart analyses the moving ranges, so `y` is replaced by them. Nothing
#' downstream needs the original values, and `chart$data_original` keeps what
#' the user supplied.
#'
#' `moving_ranges()` prepends `NA`, so the series stays aligned with `x` and is
#' one non-missing value shorter - see `n_effective_points()`.
#'
#' @return autospc_chart object of the same class as chart
#' @noRd
prepare_data.autospc_chart_mr <- function(chart) {

  mrs <- moving_ranges(y = chart$data$y)

  chart$data <- chart$data %>%
    dplyr::mutate(y = mrs)

  return(chart)

}


#' Number of points available for analysis
#'
#' One more than the non-missing moving ranges. `moving_ranges()` prepends `NA`,
#' so an MR series always has exactly one fewer non-missing value than the
#' series it was derived from, and the algorithm's data-sufficiency checks are
#' about that underlying series.
#'
#' @return integer
#' @noRd
n_effective_points.autospc_chart_mr <- function(chart,
                                                data) {

  points <- NextMethod() + 1L

  return(points)

}


#' Calculate control limits for a subset of MR-chart data
#'
#' Centre line is the mean moving range of the non-excluded points, with the
#' upper limit 3.267 times that and the lower limit zero.
#'
#' Screening is deliberately not applied. It exists to estimate the average
#' moving range used for the X chart's limits; the MR chart itself never screens
#' (Provost and Murray), so `mr_screen_max_loops` is fixed at zero rather than
#' read from the chart.
#'
#' @return list of cl, ucl and lcl, each the same length as period, plus the
#'   moving ranges as supplied
#' @noRd
calculate_limits.autospc_chart_mr <- function(chart,
                                              period,
                                              exclusion_points) {

  # period$y holds the moving ranges, put there by prepare_data()
  limits <- get_mr_limits(mr = period$y,
                          mr_screen_max_loops = 0L,
                          exclusion_points = exclusion_points)

  return(limits)

}

# Presentation methods

#' Chart name
#'
#' @return string, name of chart for labels
#' @noRd
chart_type_label.autospc_chart_mr <- function(chart) {
  return("MR")
}


#' Row that carries the first centre line label
#'
#' The first moving range is undefined, so the label goes on the second row.
#'
#' @return integer, row number
#' @noRd
first_label_row.autospc_chart_mr <- function(chart) {

  return(2L)

}


#' Rounding accuracy for centre line labels
#'
#' Four significant figures at the scale of the axis, because the values are in
#' the units of the measure rather than percentages.
#'
#' @return number, passed to scales::number(accuracy =)
#' @noRd
label_accuracy.autospc_chart_mr <- function(chart,
                                            ylimhigh) {

  accuracy <- 10^(ceiling(log10(ylimhigh)) - 4)

  return(accuracy)

}


#' Do this chart's labels always sit above the centre line?
#'
#' Yes. A label below the centre line of a range chart reads badly, so
#' `flip_labels` does not apply here.
#'
#' @return TRUE or FALSE
#' @noRd
labels_stay_above.autospc_chart_mr <- function(chart) {

  return(TRUE)

}


#' Lower and upper ends of the y axis
#'
#' @return list of two numbers, low and high
#' @noRd
y_axis_range.autospc_chart_mr <- function(chart,
                                          data) {

  high <- max(data$ucl,
              data$y,
              na.rm = TRUE) * 1.1

  return(list(low = 0,
              high = high))

}


#' Retrieve default y axis label
#'
#' @return string
#' @noRd
y_axis_title.autospc_chart_mr <- function(chart) {
  return("MR")
}
