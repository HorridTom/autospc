

#' Round the count columns to whole numbers
#'
#' Called at construction, so that the counts are whole numbers before any
#' calculation uses them. This matters because `aggregate_data()` sums the
#' counts within each subgroup, and the sum of the rounded values is not always
#' equal to the rounded sum.
#'
#' @return autospc_chart object of the same class as chart
#' @noRd
round_counts <- function(chart) {
  UseMethod("round_counts")
}


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


#' The centre line label, formatted
#'
#' Everything about how a centre line value is written: the rounding, which
#' comes from `label_accuracy()`, and whether it carries a per cent sign or a
#' thousands separator.
#'
#' @param cl the centre line values
#' @param ylimhigh upper end of the y axis, passed on to `label_accuracy()`
#' @return character, one label per value
#' @noRd
centre_line_label <- function(chart, cl, ylimhigh) {
  UseMethod("centre_line_label")
}


#' Chart name
#'
#' @return string, name of chart for labels
#' @noRd
chart_type_label <- function(chart) {
  UseMethod("chart_type_label")
}


#' Row that carries the first centre line label
#'
#' @return integer, row number
#' @noRd
first_label_row <- function(chart) {
  UseMethod("first_label_row")
}


#' Rounding accuracy for centre line labels
#'
#' @param ylimhigh upper end of the y axis, used by the classes whose accuracy
#'   depends on the scale of the data
#' @return number, passed to scales::number(accuracy =)
#' @noRd
label_accuracy <- function(chart, ylimhigh) {
  UseMethod("label_accuracy")
}


#' Do this chart's labels always sit above the centre line?
#'
#' `flip_labels` asks for labels below the line when the centre line falls. Some
#' chart types read badly that way and always stay above, whatever it says.
#'
#' @return TRUE or FALSE
#' @noRd
labels_stay_above <- function(chart) {
  UseMethod("labels_stay_above")
}


#' Default vertical position of centre line labels
#'
#' Answers only the class-dependent question. The caller applies any user
#' override, and derives the lower scale factor from the upper.
#'
#' @return number, the scale factor applied to the upper control limit
#' @noRd
upper_annotation_sf_default <- function(chart) {
  UseMethod("upper_annotation_sf_default")
}


#' Lower and upper ends of the y axis
#'
#' Answers only the class-dependent question. The caller applies the
#' short-series fallback and any user override.
#'
#' @return list of two numbers, low and high
#' @noRd
y_axis_range <- function(chart, data) {
  UseMethod("y_axis_range")
}


#' Retrieve default y axis label
#'
#' @return string
#' @noRd
y_axis_title <- function(chart) {
  UseMethod("y_axis_title")
}

