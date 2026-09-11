# Extending limits beyond the end of the data, specified by `extend_limits_to`
#
# The package extends limits in three different senses:
# (a) extending calculation period limits into display periods
# (b) extending limits beyond the end of the data, and
# (c) extending limits over a subgroup where the analysis series is missing
#
# This file holds the functionality for (b).
#
# The other two are elsewhere and named accordingly:
# (a) is `extend_display_limits()`, and (c) is `limits_for_missing_rows()`, both
# autospc_chart generics with methods in the chart class files.


#' The columns an extension row is defined by
#'
#' Every other column of the table describes a subgroup, and an
#' extension row is not one, so `extension_row()` leaves the rest missing.
#'
#' @return A character vector of column names.
#' @noRd
extension_row_columns <- function() {
  column_names <- c(
    "x", "cl", "ucl", "lcl", "limit_width",
    "period_type", "period_start", "plot_period",
    "limit_change", "cl_change",
    "excluded", "break_point", "rule1", "rule2", "run_start",
    "above_or_below_cl", "highlight", "limit_extension"
  )

  return(column_names)
}


#' A value for extending the x column, of the same type as the column
#'
#' The rows the extension adds are copied from the table so that their columns
#' match it, and the x value they are given has to match too.
#'
#' Returns a whole number where a whole number value is requested for integer x,
#' so that the extension does not change the type of the x column.
#'
#' A non-integer value (i.e. a value that a column of integers cannot hold) is
#' left as it is, and the x column becomes a column of doubles when the rows are
#' added.
#'
#' @param value Requested position on the horizontal axis where a row of the
#' extension sits.
#' @param x The `x` column of the table.
#'
#' @return `value`, valid x-column value corresponding to passed value
#' @noRd
x_value_for_extension <- function(value,
                                  x) {
  holds_whole_numbers <- is.integer(x) &&
    is.numeric(value) &&
    !is.na(value) &&
    value == round(value) &&
    abs(value) <= .Machine$integer.max

  if (holds_whole_numbers) {
    return(as.integer(value))
  }

  return(value)
}


#' The start of the period the extension continues
#'
#' The extension continues the final period's limits past the end of the data,
#' so it takes that period's number rather than being numbered as a period of
#' its own.
#'
#' The last row of the table is not always in a period: a series that ends with
#' rows holding no observation ends with rows that have no limits and so no
#' period. The last row that has one is taken.
#'
#' @param table The analysed table.
#'
#' @return The `period_start` of the final period, or NA where no row has one.
#' @noRd
final_period_start <- function(table) {
  starts <- table$period_start[!is.na(table$period_start)]

  if (length(starts) == 0L) {
    return(NA_integer_)
  }

  return(starts[length(starts)])
}


#' One row of the extension beyond the end of the data
#'
#' Copied from the last row of the table, so column types match the table, then
#' given the values the extension defines. The columns that describe a subgroup
#' are left missing, because there is no subgroup at this point on the axis.
#'
#' The limits are those of the final calculation period rather than of the
#' final display period, which for a chart whose limits vary with the
#' denominator means a mean denominator taken over a whole period rather than
#' over however many points the final display period happens to hold.
#'
#' @param table The analysed table.
#' @param x_value Where on the horizontal axis the row sits.
#' @param limits The limits for the extension, as
#'   `limits_for_extension_rows()` gives them.
#' @param final_period The rows of the final calculation period.
#'
#' @return A one-row data frame with the columns of `table`.
#' @noRd
extension_row <- function(table,
                          x_value,
                          limits,
                          final_period) {
  row <- table %>%
    dplyr::slice_tail(n = 1L) %>%
    dplyr::mutate(
      x = x_value,
      cl = limits$cl,
      lcl = limits$lcl,
      ucl = limits$ucl,
      period_type = "display",
      excluded = NA,
      break_point = FALSE,
      rule1 = FALSE,
      rule2 = FALSE,
      run_start = FALSE,
      above_or_below_cl = 0,
      highlight = "None",
      limit_extension = TRUE
    )

  row$period_start <- final_period_start(table)
  row$plot_period <- paste0(row$period_type, row$period_start)
  row$limit_change <- FALSE
  row$cl_change <- 0

  if ("limit_width" %in% names(row)) {
    row$limit_width <- limit_width_of(final_period)
  }

  # assigning into the column rather than replacing it keeps the column's type
  for (column in setdiff(names(row), extension_row_columns())) {
    row[[column]][1L] <- NA
  }

  return(row)
}


#' Whether the horizontal axis holds whole units only
#'
#' An integer column counts, and so does a `Date`, which R stores as a number
#' of whole days. A step of less than one unit on either of those is a step to
#' a value the column cannot tell apart from the one before it.
#'
#' @param x_values Values from the horizontal axis, read for their type.
#'
#' @return TRUE or FALSE
#' @noRd
axis_holds_whole_units <- function(x_values) {
  return(is.integer(x_values) || inherits(x_values, "Date"))
}


#' The step from the last subgroup to the first row of the extension
#'
#' The extension begins one subgroup on from the end of the data, so that it
#' starts where the next subgroup would have been. The median gap between
#' consecutive subgroups is what "one subgroup" means for a series whose
#' spacing is not perfectly regular, and it carries the units of the axis: a
#' day for daily data, a month for monthly, a hundredth of a second for data
#' measured every ten milliseconds and expressed in seconds.
#'
#' The step is capped at half the extension, because the caller may ask for an
#' extension shorter than one subgroup, and the first row has to fall inside
#' it. Where the cap binds, the limits slope over the first half of the
#' extension rather than holding level; the extension is short whenever that
#' happens, and the axis is at least `period_min` subgroups long, so it is a
#' small part of the chart.
#'
#' Rounding up can take the step back past that cap, so on a whole-unit axis it
#' is held to the length of the extension. The first row of the extension then
#' sits at `extend_limits_to` itself, and there is only one row.
#'
#' The gaps are measured as plain numbers so that a `Date` or `POSIXct`
#' difference cannot arrive in different units from the extension it is
#' compared with. Adding the result back to `x` returns to the axis's own
#' units, because that is how `+` reads a number for those classes.
#'
#' @param x_values The horizontal axis values of the subgroups.
#' @param extend_limits_to The point the caller asked to extend to.
#'
#' @return The step, as a number in the units of the `x` column.
#' @noRd
extension_step <- function(x_values,
                           extend_limits_to) {
  positions <- sort(unique(as.numeric(x_values)))

  extension <- as.numeric(extend_limits_to) - positions[length(positions)]

  step <- min(stats::median(diff(positions)), extension / 2)

  if (axis_holds_whole_units(x_values)) {
    step <- min(ceiling(step), extension)
  }

  return(step)
}


#' Extend the final period's limits out beyond the end of the data
#'
#' The functionality for the `extend_limits_to` argument. Rows are added to the
#' table at the first point past the last x of the data and at
#' `extend_limits_to`, both holding the limits `limits_for_extension_rows()`
#' gives for the final calculation period, and no observation. Drawing a line
#' through them puts the limits across the extension.
#'
#' There are two such rows, except where the step from the last subgroup
#' reaches `extend_limits_to` itself, which leaves one.
#'
#' `limit_extension` says which rows those are, and is FALSE on every row of
#' the data whether or not the caller asked for an extension.
#'
#' @param table The analysed table.
#' @param chart The chart being analysed, read for `extend_limits_to`.
#'
#' @return `table`, with `limit_extension` added and, where the caller asked
#'   for an extension, the rows of the extension.
#' @noRd
extend_limits_beyond_data <- function(table,
                                      chart) {
  table$limit_extension <- FALSE

  extend_limits_to <- chart$extend_limits_to

  if (is.null(extend_limits_to)) {
    return(table)
  }

  x_max <- max(table$x, na.rm = TRUE)

  last_calc_period <- table %>%
    dplyr::filter(period_type == "calculation") %>%
    dplyr::slice_tail(n = 1L) %>%
    dplyr::pull(plot_period)

  final_period <- table %>%
    dplyr::filter(plot_period == last_calc_period)

  limits <- limits_for_extension_rows(
    chart = chart,
    period = final_period
  )

  step <- extension_step(
    x_values = subgroup_x_values(table),
    extend_limits_to = extend_limits_to
  )

  starts_at <- x_value_for_extension(x_max + step, table$x)
  ends_at <- x_value_for_extension(extend_limits_to, table$x)

  # a row at each distinct position, which is one row where the two coincide
  rows <- lapply(
    unique(c(starts_at, ends_at)),
    function(x_value) extension_row(table, x_value, limits, final_period)
  )

  return(dplyr::bind_rows(c(list(table), rows)))
}


#' The horizontal axis values at which there is a subgroup
#'
#' `extend_limits_beyond_data()` adds rows past the end of the data, and those
#' rows hold no subgroup, so the largest `x` in the table is not always the
#' largest `x` of a subgroup. `limit_extension` says which rows the extension
#' added, and is absent from a table that has no limits, where none were.
#'
#' A row inside a gap is a subgroup and is included: there was a subgroup
#' there, and nothing is known about it.
#'
#' @param table The table to be drawn.
#'
#' @return The `x` column, without the rows the extension added.
#' @noRd
subgroup_x_values <- function(table) {
  if (!"limit_extension" %in% names(table)) {
    return(table$x)
  }

  return(table$x[!table$limit_extension])
}
