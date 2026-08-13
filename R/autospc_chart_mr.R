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

  return(
    validate_autospc_chart(x)
  )

}


#' Create an autospc_chart_mr object
#'
#' Helper for MR charts: assemble, construct, validate, return.
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

  return(autospc_chart_mr_object)

}


# Analysis methods

# No aggregate_data() method: MR charts plot one moving range per observation, so
# the superclass default - return the chart unchanged - is the correct behaviour.

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

  # period$y holds the moving ranges by this point in the pipeline
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


#' Retrieve default y axis label
#'
#' @return string
#' @noRd
y_axis_title.autospc_chart_mr <- function(chart) {
  return("MR")
}
