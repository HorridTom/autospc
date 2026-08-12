# autospc_chart_cp class

#' Construct an autospc_chart_cp object
#'
#' @return An object of class `c("autospc_chart_cp", "autospc_chart")`.
#' @noRd
new_autospc_chart_cp <- function(x) {

  return(
    new_autospc_chart(x,
                      class = "autospc_chart_cp")
  )

}


#' Validate an autospc_chart_cp object
#'
#' See `validate_autospc_chart()` for the class contract.
#'
#' @return `x`, unchanged, if valid; otherwise an error.
#' @noRd
validate_autospc_chart_cp <- function(x) {

  if(!inherits(x, "autospc_chart_cp")) {
    stop("Not an autospc_chart_cp object.", call. = FALSE)
  }

  return(
    validate_autospc_chart(x)
  )

}


#' Create an autospc_chart_cp object
#'
#' Helper for C' charts: assemble, construct, validate, return.
#'
#' @return An object of class `c("autospc_chart_cp", "autospc_chart")`.
#' @noRd
autospc_chart_cp <- function(data,
                             x,
                             y,
                             ...) {

  autospc_chart_cp_l <- autospc_chart_list(data = data,
                                           x = x,
                                           y = y,
                                           ...)

  autospc_chart_cp_object <- new_autospc_chart_cp(autospc_chart_cp_l)

  autospc_chart_cp_object <- validate_autospc_chart_cp(autospc_chart_cp_object)

  return(autospc_chart_cp_object)

}


# Methods

#' Aggregate data for analysis
#'
#' Sums y (count) over x (subgroup)
#'
#' @return autospc_chart object of the same class as chart
#' @noRd
aggregate_data.autospc_chart_cp <- function(chart) {

  df_agg <- chart$data %>%
    dplyr::group_by(x) %>%
    dplyr::summarise(y = sum(y))

  chart$data <- df_agg

  return(chart)

}


#' Retrieve default y axis label
#'
#' @return string
#' @noRd
y_axis_title.autospc_chart_cp <- function(chart) {
  return("Number")
}


#' Chart name
#'
#' @return string, name of chart for labels
#' @noRd
chart_type_label.autospc_chart_cp <- function(chart) {
  return("C'")
}


#' Calculate control limits for a subset of C'-chart data
#'
#' As for the C chart, but with the standard deviation inflated by the mean
#' moving range of the data, screened for outliers. The number of screening
#' passes is taken from the chart's `mr_screen_max_loops`.
#'
#' @return list of three vectors (cl, ucl, lcl), each the same length as period
#' @noRd
calculate_limits.autospc_chart_cp <- function(chart,
                                              period,
                                              exclusion_points) {

  limits <- get_cp_limits(y = period$y,
                          exclusion_points = exclusion_points,
                          mr_screen_max_loops = chart$mr_screen_max_loops)

  return(limits)

}
