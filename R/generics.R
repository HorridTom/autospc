

#' Aggregate data for analysis
#'
#' @return autospc_chart object of the same class as chart
#' @noRd
aggregate_data <- function(chart) {
  UseMethod("aggregate_data")
}


#' Turn the aggregated data into the series the algorithm analyses
#'
#' Runs after `aggregate_data()`, because the P and P' transform is computed
#' from the aggregated numerator and denominator.
#'
#' @return autospc_chart object of the same class as chart, with `chart$data`
#'   replaced
#' @noRd
prepare_data <- function(chart) {
  UseMethod("prepare_data")
}


#' Number of points available for analysis
#'
#' Used to decide whether there is enough data to form a period.
#'
#' @param data a dataframe with a y column
#'
#' @return integer
#' @noRd
n_effective_points <- function(chart,
                               data) {
  UseMethod("n_effective_points")
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


#' Extend the limits of the preceding calculation period over the display period
#'
#' Called with `counter` already known to be within the table.
#'
#' @param limits_table the limits table so far
#' @param counter row number of the first display point
#'
#' @return `limits_table`, with the display rows filled in
#' @noRd
extend_display_limits <- function(chart,
                                  limits_table,
                                  counter) {
  UseMethod("extend_display_limits")
}


#' Limits to use beyond the end of the data
#'
#' Used when `extend_limits_to` carries the final period's limits out past the
#' last data point. One set of values for the whole extension.
#'
#' @param period the final calculation period
#'
#' @return list of single values, named cl, lcl and ucl
#' @noRd
extrapolate_limits <- function(chart,
                               period) {
  UseMethod("extrapolate_limits")
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

