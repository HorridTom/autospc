

#' Aggregate data for analysis
#'
#' @return autospc_chart object of the same class as chart
#' @noRd
aggregate_data <- function(chart) {
  UseMethod("aggregate_data")
}

#' Retrieve default y axis label
#'
#' @return string
#' @noRd
y_axis_title <- function(chart) {
  UseMethod("y_axis_title")
}

