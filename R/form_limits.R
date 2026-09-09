# Function to form calculation limits for a period
# data has columns x and series
form_calculation_limits <- function(data,
                                    counter,
                                    chart) {
  if (counter == 1L) {
    period_length <- baseline_period_length(chart, data = data)
  } else {
    period_length <- chart$period_min
  }

  exclusion_points <- find_extremes(
    data = data,
    chart = chart,
    counter = counter,
    period_length = period_length
  )

  calculation_period <- data[counter:(counter + period_length - 1), ]

  # Calculation of limits excluding extremes for selected section of data
  limits_list <- calculate_limits(
    chart = chart,
    period = calculation_period,
    exclusion_points = exclusion_points
  )

  calculation_period$cl <- limits_list$cl
  calculation_period$ucl <- limits_list$ucl
  calculation_period$lcl <- limits_list$lcl

  # only the classes whose limits vary with the denominator return this
  calculation_period$limit_width <- limits_list$limit_width

  extra_columns <- limits_table_columns(chart)

  calculation_period <- calculation_period %>%
    dplyr::select(x, series, ucl, lcl, cl, dplyr::any_of("limit_width")) %>%
    dplyr::mutate(period_type = "calculation") %>%
    dplyr::mutate(
      excluded = ifelse(dplyr::row_number() %in% exclusion_points, T, F)
    )


  # First period does not already have the additional columns
  if (counter == 1) {
    # Joins limits to the existing data
    limits_table <- data %>%
      dplyr::left_join(calculation_period, by = "x") %>%
      dplyr::mutate(series = dplyr::if_else(
        is.na(series.y), series.x, series.y
      ))

    limits_table <- limits_table %>%
      dplyr::select(
        x, series, dplyr::all_of(extra_columns), ucl, lcl, cl,
        dplyr::any_of("limit_width"),
        period_type, excluded,
        dplyr::any_of("run_break"),
        dplyr::any_of("log")
      )
    # Add the break_point column to keep track of break points as they are
    # added. For compatibility with (at least)
    # add_rule_breaks_respecting_periods, the first point is not classed as a
    # break point.
    limits_table <- limits_table %>%
      dplyr::mutate(break_point = dplyr::if_else(dplyr::row_number() == counter,
        NA,
        FALSE
      ))
  } else {
    # joins limits to the existing data, overwriting display limits
    limits_table <- data %>%
      dplyr::left_join(calculation_period, by = "x") %>%
      dplyr::mutate(series = dplyr::if_else(
        is.na(series.y), series.x, series.y
      )) %>%
      dplyr::mutate(ucl = dplyr::if_else(is.na(ucl.y), ucl.x, ucl.y)) %>%
      dplyr::mutate(lcl = dplyr::if_else(is.na(lcl.y), lcl.x, lcl.y)) %>%
      dplyr::mutate(cl = dplyr::if_else(is.na(cl.y), cl.x, cl.y)) %>%
      dplyr::mutate(period_type = dplyr::if_else(
        is.na(period_type.y), period_type.x, period_type.y
      )) %>%
      dplyr::mutate(excluded = dplyr::if_else(
        is.na(excluded.y), excluded.x, excluded.y
      ))

    if ("limit_width.y" %in% names(limits_table)) {
      limits_table$limit_width <- dplyr::if_else(
        is.na(limits_table$limit_width.y),
        limits_table$limit_width.x,
        limits_table$limit_width.y
      )
    }

    limits_table <- limits_table %>%
      dplyr::mutate(break_point = (break_point |
        dplyr::row_number() == counter))

    limits_table <- limits_table %>%
      dplyr::select(
        x, series, dplyr::all_of(extra_columns), ucl, lcl, cl,
        dplyr::any_of("limit_width"),
        period_type, excluded,
        dplyr::contains("break_point"),
        dplyr::contains("rule"),
        dplyr::contains("above_or_below"),
        dplyr::contains("highlight"),
        dplyr::contains("run"),
        dplyr::any_of("log")
      )
  }

  return(limits_table)
}


# Function to form display limits (period extension)
form_display_limits <- function(limits_table, counter, chart) {
  if (counter > nrow(limits_table)) {
    # No display limits needed - no data beyond calculation period
    return(limits_table)
  }

  limits_table <- extend_display_limits(
    chart = chart,
    limits_table = limits_table,
    counter = counter
  )

  return(limits_table)
}


# Function to create limits for new calculation and display period with rule
# breaks
form_calculation_and_display_limits <- function(
  data,
  counter_at_period_start,
  chart
) {
  # form calculation limits for first period
  limits_table <- form_calculation_limits(
    data = data,
    counter = counter_at_period_start,
    chart = chart
  )


  # extend display limits to end

  if (counter_at_period_start == 1L) {
    period_length <- baseline_period_length(chart, data = data)
  } else {
    period_length <- chart$period_min
  }

  limits_table <- form_display_limits(
    limits_table = limits_table,
    counter = counter_at_period_start +
      period_length,
    chart = chart
  )

  # add rule breaks considering where periods are
  limits_table <- add_rule_breaks_respecting_periods(
    limits_table = limits_table,
    counter = counter_at_period_start,
    centre_line_tolerance = chart$centre_line_tolerance,
    shift_rule_threshold = chart$shift_rule_threshold
  )

  return(limits_table)
}


#' The columns an extension row is defined by
#'
#' Every other column of the table describes a subgroup, and an extension row
#' is not one, so `extension_row()` leaves the rest missing.
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
#' its own. Where the series ends in a display period the two are one period,
#' and where it ends in a calculation period the extension becomes the
#' corresponding display period.
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
#' @param limits The extrapolated limits, as `extrapolate_limits()` gives them.
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


#' Carry the final period's limits out beyond the end of the data
#'
#' The functionality for the `extend_limits_to` argument. Two rows are added to
#' the table, one at the first point past the last x and one at
#' `extend_limits_to`, both holding the limits `extrapolate_limits()` gives for
#' the final calculation period and no observation. Drawing a line between them
#' puts the limits across the extension.
#'
#' `limit_extension` says which rows those are, and is FALSE on every row of
#' the data whether or not the caller asked for an extension.
#'
#' @param table The analysed table.
#' @param chart The chart being analysed, read for `extend_limits_to`.
#'
#' @return `table`, with `limit_extension` added and, where the caller asked
#'   for an extension, the two rows.
#' @noRd
extend_limits <- function(table,
                          chart) {
  table$limit_extension <- FALSE

  extend_limits_to <- chart$extend_limits_to

  if (is.null(extend_limits_to)) {
    return(table)
  }

  x_max <- max(table$x, na.rm = TRUE)

  if (extend_limits_to <= x_max) {
    stop("Limits can only be extended to a point beyond the end of the data.")
  }

  last_calc_period <- table %>%
    dplyr::filter(period_type == "calculation") %>%
    dplyr::slice_tail(n = 1L) %>%
    dplyr::pull(plot_period)

  final_period <- table %>%
    dplyr::filter(plot_period == last_calc_period)

  limits <- extrapolate_limits(
    chart = chart,
    period = final_period
  )

  starts_at <- x_value_for_extension(x_max + 1, table$x)
  ends_at <- x_value_for_extension(extend_limits_to, table$x)

  return(dplyr::bind_rows(
    table,
    extension_row(table, starts_at, limits, final_period),
    extension_row(table, ends_at, limits, final_period)
  ))
}
