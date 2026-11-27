# Analysis generics


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
#' Runs after `aggregate_data()`, because the P, P', U and U' transforms are
#' computed from the aggregated numerator and denominator.
#'
#' @return autospc_chart object of the same class as chart, with `chart$data`
#'   replaced
#' @noRd
prepare_data <- function(chart) {
  UseMethod("prepare_data")
}


#' Prepare one segment of a series that starts at a forced restart
#'
#' `prepare_data()` prepares the whole series at once, before it is split at
#' the forced restarts. Where a row's `series` value is worked out from earlier
#' rows as well as its own, the value at the first row of a segment depends on
#' rows before the restart. This recalculates any such value from the
#' segment's own rows, so that the segment's `series` is what `prepare_data()`
#' would give if the series began at the segment's first row.
#'
#' @param chart The chart being analysed.
#' @param data The rows of `chart$data` from one forced restart to the row
#'   before the next, or to the end of the series, already prepared by
#'   `prepare_data()`.
#'
#' @return `data`, with `series` as `prepare_data()` would give it for a series
#'   starting at the segment's first row.
#' @noRd
prepare_segment_data <- function(chart,
                                 data) {
  UseMethod("prepare_segment_data")
}


#' The rows of a location chart's segment where its dispersion chart has a
#' value
#'
#' Used only for the location chart of a pair whose
#' `dispersion_chart_establishment` is "shared".
#'
#' @param chart The location chart being analysed.
#' @param data One segment's rows of `chart$data`, as `segment_data()` returns
#'   them before this column is added.
#'
#' @return A logical vector, one element per row of `data`.
#' @noRd
dispersion_observed_rows <- function(chart,
                                     data) {
  UseMethod("dispersion_observed_rows")
}


#' The rows of a prepared series that hold an observation
#'
#' A row whose `series` value is missing because there was never a value to
#' have is not a gap in the series. `observed_rows.autospc_chart_mr()` is where
#' that applies.
#'
#' With `location = TRUE`, the rows that hold an observation on the location
#' chart instead. The algorithm finds the gaps after which it restarts from
#' these, so that both charts of a pair find the same gaps. For a location
#' chart the answer is the same either way; the dispersion charts' methods read
#' the location chart's value from `y`.
#'
#' @param chart The chart being analysed.
#' @param data A prepared series, with one row per point and a `series`
#'   column.
#' @param location Whether to give the location chart's observations rather
#'   than this chart's.
#'
#' @return A logical vector, one element per row of `data`.
#' @noRd
observed_rows <- function(chart,
                          data,
                          location = FALSE) {
  UseMethod("observed_rows")
}


#' Whether the point at the start of a mirrored calculation period is excluded
#' from that period's calculation
#'
#' Read only for the dispersion chart of a pair whose
#' `dispersion_chart_establishment` is not "independent", at each calculation
#' period after the first in a segment. An excluded point is marked
#' `excluded = TRUE` and does not count towards `max_exclusions`.
#'
#' @param chart The chart being analysed.
#'
#' @return TRUE or FALSE
#' @noRd
mirrored_period_start_excluded <- function(chart) {
  UseMethod("mirrored_period_start_excluded")
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


#' The number of points the dispersion chart has in a calculation period of
#' each length
#'
#' Used only for the location chart of a pair whose
#' `dispersion_chart_establishment` is "shared". Counted as the dispersion
#' chart counts towards `period_min`.
#'
#' @param chart The location chart being analysed.
#' @param available For each row from the start of the period onwards, whether
#'   the dispersion chart has a value there, as `dispersion_observed_rows()`
#'   returns it.
#'
#' @return An integer vector the length of `available`: element `i` is the
#'   number of points the dispersion chart has in a period of the first `i`
#'   rows.
#' @noRd
dispersion_point_counts <- function(chart,
                                    available) {
  UseMethod("dispersion_point_counts")
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


#' Control limits from a period's statistics
#'
#' The one place limits are formed. Three standard errors either side of the
#' centre line, except where a class's limits are defined some other way.
#'
#' @param statistics The period's centre line and standard deviation estimate,
#'   as `calculate_limits()` returns them.
#' @param rows The rows to give limits to.
#'
#' @return list of two numeric vectors named ucl and lcl, one value per row
#' @noRd
limits_from_statistics <- function(chart, statistics, rows) {
  UseMethod("limits_from_statistics")
}


#' The standard error at each of a set of rows
#'
#' Limits sit three standard errors either side of the centre line. For most
#' classes the standard deviation estimate is that standard error already,
#' because the statistic is a single observation. A P, P', U or U' chart's
#' estimate is free of the denominator, so each row's standard error is the
#' estimate over the square root of that row's denominator.
#'
#' @param sd_estimate The period's standard deviation estimate, as one value or
#'   as one per row.
#' @param rows The rows to give a standard error to.
#'
#' @return numeric, one value per row of `rows`
#' @noRd
standard_error_at <- function(chart, sd_estimate, rows) {
  UseMethod("standard_error_at")
}


#' The range the plotted statistic can take
#'
#' Limits are constrained to this range, so that a chart does not draw a limit
#' at a value the statistic could never take. A count cannot be negative, a
#' percentage lies between 0 and 100, and an individuals value has no bound.
#'
#' @return list of two numbers, low and high
#' @noRd
limit_bounds <- function(chart) {
  UseMethod("limit_bounds")
}


#' The period statistics a limits table carries beside the limits
#'
#' Every row of a period carries these, so that limits can be formed at rows
#' outside the calculation from any one of them.
#'
#' @return character vector
#' @noRd
period_statistics_columns <- function(chart) {
  UseMethod("period_statistics_columns")
}


#' Columns the limits table carries beside the series under analysis
#'
#' The names are inserted between `series` and `ucl`, so the order matters.
#'
#' @return character vector
#' @noRd
limits_table_columns <- function(chart) {
  UseMethod("limits_table_columns")
}


#' A period's standard deviation estimate at each of a set of rows
#'
#' For most classes the estimate is one value for the whole period. Where it
#' depends on each row's subgroup size, the class forms it at each row from the
#' period's statistics.
#'
#' @param statistics A period's statistics, as `period_statistics()` gives them.
#' @param rows The rows to give an estimate to.
#'
#' @return numeric, one value per row of `rows`
#' @noRd
sd_estimate_at <- function(chart, statistics, rows) {
  UseMethod("sd_estimate_at")
}


#' Whether the trend rule is applied to a chart
#'
#' The trend rule is not applied where the limits vary within a period. Applied
#' by default. Overridden by the classes whose limits vary with the
#' denominator or the subgroup size.
#'
#' @param table The analysed table, with one row per point.
#'
#' @return TRUE or FALSE
#' @noRd
trend_rule_applies <- function(chart, table) {
  UseMethod("trend_rule_applies")
}


#' Columns a dispersion chart adds when its pair is joined wide
#'
#' Only the dispersion chart of a pair has an answer, so there is no default.
#'
#' @return named character vector: the name each column takes in the joined
#'   table, holding the name of the dispersion chart's column it comes from
#' @noRd
paired_columns <- function(chart) {
  UseMethod("paired_columns")
}


# Presentation generics


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


#' The earliest row that can carry the first centre line label
#'
#' @param cl The centre line, one element per row of the analysis table.
#'
#' @return integer, row number, or `NA` where no row has a centre line
#' @noRd
first_label_row <- function(chart,
                            cl) {
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


# Rule generics


#' Describe a rule in words
#'
#' Used for a rule's default label, and when a rule set is printed.
#'
#' @param rule An `autospc_rule`.
#'
#' @return string
#' @noRd
rule_description <- function(rule) {
  UseMethod("rule_description")
}


#' Where a rule is broken
#'
#' @param rule An `autospc_rule`.
#' @param points Each point's distance from the centre line, its side and its
#'   stretch, as `variables_for_rules()` gives them.
#'
#' @return logical, one value per point: TRUE where the point is part of a
#'   pattern that breaks the rule, NA where the rule is not applied to it
#' @noRd
rule_breaks <- function(rule, points) {
  UseMethod("rule_breaks")
}
