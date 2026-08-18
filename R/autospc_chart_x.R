# autospc_chart_x class

#' Construct an autospc_chart_x object
#'
#' @return An object of class `c("autospc_chart_x", "autospc_chart")`.
#' @noRd
new_autospc_chart_x <- function(x) {

  return(
    new_autospc_chart(x,
                      class = "autospc_chart_x")
  )

}


#' Validate an autospc_chart_x object
#'
#' See `validate_autospc_chart()` for the class contract.
#'
#' @return `x`, unchanged, if valid; otherwise an error.
#' @noRd
validate_autospc_chart_x <- function(x) {

  if(!inherits(x, "autospc_chart_x")) {
    stop("Not an autospc_chart_x object.", call. = FALSE)
  }

  return(
    validate_autospc_chart(x)
  )

}


#' Create an autospc_chart_x object
#'
#' Helper for X charts: assemble, construct, validate, return.
#'
#' @return An object of class `c("autospc_chart_x", "autospc_chart")`.
#' @noRd
autospc_chart_x <- function(data,
                            x,
                            y,
                            ...) {

  autospc_chart_x_l <- autospc_chart_list(data = data,
                                          x = x,
                                          y = y,
                                          ...)

  autospc_chart_x_l <- normalise_columns(autospc_chart_x_l,
                                         fields = c("x", "y"))

  autospc_chart_x_object <- new_autospc_chart_x(autospc_chart_x_l)

  autospc_chart_x_object <- validate_autospc_chart_x(autospc_chart_x_object)

  return(autospc_chart_x_object)

}


# Analysis methods

# No aggregate_data() method: X charts plot the observations as supplied, so the
# superclass default - return the chart unchanged - is the correct behaviour.

#' Calculate control limits for a subset of X-chart data
#'
#' Centre line is the mean of the non-excluded points, with the standard
#' deviation estimated from the mean moving range, screened for outliers. The
#' number of screening passes is taken from the chart's `mr_screen_max_loops`.
#'
#' @return list of three vectors (cl, ucl, lcl), each the same length as period
#' @noRd
calculate_limits.autospc_chart_x <- function(chart,
                                             period,
                                             exclusion_points) {

  limits <- get_i_limits(y = period$y,
                         mr_screen_max_loops = chart$mr_screen_max_loops,
                         exclusion_points = exclusion_points)

  return(limits)

}


# Presentation methods

#' Chart name
#'
#' @return string, name of chart for labels
#' @noRd
chart_type_label.autospc_chart_x <- function(chart) {
  return("X")
}


#' Rounding accuracy for centre line labels
#'
#' Four significant figures at the scale of the axis, because the values are in
#' the units of the measure rather than percentages.
#'
#' @return number, passed to scales::number(accuracy =)
#' @noRd
label_accuracy.autospc_chart_x <- function(chart,
                                           ylimhigh) {

  accuracy <- 10^(ceiling(log10(ylimhigh)) - 4)

  return(accuracy)

}


#' Lower and upper ends of the y axis
#'
#' @return list of two numbers, low and high
#' @noRd
y_axis_range.autospc_chart_x <- function(chart,
                                         data) {

  low <- min(data$lcl,
             data$y,
             na.rm = TRUE)

  if(sign(low) != -1) {
    low <- low * 0.9
  } else {
    low <- low * 1.1
  }

  high <- max(data$ucl,
              data$y,
              na.rm = TRUE) * 1.1

  return(list(low = low,
              high = high))

}


#' Retrieve default y axis label
#'
#' @return string
#' @noRd
y_axis_title.autospc_chart_x <- function(chart) {
  return("X")
}
