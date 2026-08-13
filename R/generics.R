

#' Aggregate data for analysis
#'
#' @return autospc_chart object of the same class as chart
#' @noRd
aggregate_data <- function(chart) {
  UseMethod("aggregate_data")
}


#' Calculate control limits for a subset of the chart data
#' 
#' @param period a dataframe providing the subset of the data to use as
#' calculation period
#' @param exclusion_points vector of row numbers, relative to period, to exclude
#' from limit calculations
#'
#' @return list of three vectors (cl, ucl, lcl), each the same length as period
#' @noRd
calculate_limits <- function(chart,
                             period,
                             exclusion_points) {
  UseMethod("calculate_limits")
}


#' Columns the limits table carries in addition to the common ones
#'
#' The names are inserted between `y` and `ucl`, so the order matters.
#'
#' @return character vector, possibly empty
#' @noRd
limits_table_columns <- function(chart) {
  UseMethod("limits_table_columns")
}


#' Chart name
#'
#' @return string, name of chart for labels
#' @noRd
chart_type_label <- function(chart) {
  UseMethod("chart_type_label")
}


#' Retrieve default y axis label
#'
#' @return string
#' @noRd
y_axis_title <- function(chart) {
  UseMethod("y_axis_title")
}

