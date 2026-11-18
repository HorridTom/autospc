# Function to form calculation limits for a period
# data has columns x and series; the period is the period_length rows of data
# starting at row counter
form_calculation_limits <- function(data,
                                    counter,
                                    period_length,
                                    chart) {
  exclusion_points <- find_extremes(
    data = data,
    chart = chart,
    counter = counter,
    period_length = period_length,
    excluded_first = mirrored_exclusions(chart,
      data = data,
      counter = counter,
      period_length = period_length
    )
  )

  calculation_period <- data[counter:(counter + period_length - 1), ]

  # Calculation of statistics excluding extremes for selected section of data
  statistics <- calculate_limits(
    chart = chart,
    period = calculation_period,
    exclusion_points = exclusion_points
  )

  limits <- constrain_limits(
    limits = limits_from_statistics(
      chart = chart,
      statistics = statistics,
      rows = calculation_period
    ),
    bounds = limit_bounds(chart)
  )

  statistic_columns <- period_statistics_columns(chart)

  # add the period's centre line, limits and statistics to its rows, mark it as
  # a calculation period, and mark the excluded points
  new_values <- c(
    list(cl = statistics$cl, ucl = limits$ucl, lcl = limits$lcl),
    statistics[statistic_columns],
    list(
      period_type = "calculation",
      excluded = seq_len(nrow(calculation_period)) %in% exclusion_points
    )
  )
  for (column in names(new_values)) {
    calculation_period[[column]] <- rep_len(
      new_values[[column]], nrow(calculation_period)
    )
  }

  # keep only these columns, in this order
  calculation_period <- calculation_period[unique(c(
    "x", "series", "ucl", "lcl", "cl", statistic_columns,
    "period_type", "excluded"
  ))]

  extra_columns <- limits_table_columns(chart)


  # First period does not already have the additional columns
  if (!"break_point" %in% names(data)) {
    # Joins limits to the existing data
    limits_table <- data %>%
      dplyr::left_join(calculation_period, by = "x") %>%
      dplyr::mutate(
        series = dplyr::if_else(is.na(series.y), series.x, series.y),
        # the first point is not classed as a break point, for compatibility
        # with (at least) add_rule_breaks_respecting_periods
        break_point = dplyr::if_else(dplyr::row_number() == counter,
          NA,
          FALSE
        )
      )

    # keep only these columns, in this order. break_point keeps track of break
    # points as they are added
    limits_table <- limits_table %>%
      dplyr::select(
        x, series, dplyr::all_of(extra_columns), ucl, lcl, cl,
        dplyr::all_of(statistic_columns),
        period_type, excluded,
        dplyr::any_of("run_break"),
        dplyr::any_of("log"),
        break_point
      )
  } else {
    # copy the new period's values into its rows of data, missing values
    # included, replacing the display limits there. calculation_period was
    # taken from data starting at row counter, so its rows are rows counter
    # onwards of data. Rows beyond the end of data are skipped
    limits_table <- data
    rows <- counter:(counter + nrow(calculation_period) - 1L)
    in_table <- rows <= nrow(limits_table)
    for (column in setdiff(names(calculation_period), "x")) {
      new <- calculation_period[[column]]
      limits_table[[column]][rows[in_table]] <- new[in_table]
    }

    # the first row of the new period is a break point
    limits_table[["break_point"]] <- limits_table$break_point |
      seq_len(nrow(limits_table)) == counter

    # keep only these columns, in this order. Names are matched ignoring case
    columns <- names(limits_table)
    containing <- function(text) {
      return(columns[grepl(text, tolower(columns), fixed = TRUE)])
    }
    limits_table <- limits_table[unique(c(
      "x", "series", extra_columns, "ucl", "lcl", "cl", statistic_columns,
      "period_type", "excluded",
      containing("break_point"), containing("rule"),
      containing("above_or_below"), containing("highlight"),
      containing("run"), intersect("log", columns)
    ))]
  }

  return(limits_table)
}


#' Give the display rows the limits of the preceding calculation period
#'
#' Every row from `counter` to the end of the table is given limits formed at
#' that row from the centre line and standard deviation estimate of the row at
#' `counter - 1`, the last row of the calculation period.
#'
#' @param limits_table The limits table being built.
#' @param counter The first display row.
#'
#' @return `limits_table`, with the display rows filled in
#' @noRd
form_display_limits <- function(limits_table, counter, chart) {
  if (counter > nrow(limits_table)) {
    # No display limits needed - no data beyond calculation period
    return(limits_table)
  }

  # the limits for each display row, from the statistics of the last row of the
  # calculation period
  display_rows <- counter:nrow(limits_table)

  limits <- limits_at_rows(
    chart = chart,
    statistics = period_statistics(limits_table[counter - 1L, ]),
    rows = limits_table[display_rows, ]
  )

  # write the limits into the display rows and mark them as display rows
  new_values <- c(limits, list(period_type = "display"))
  for (column in names(new_values)) {
    limits_table[[column]][display_rows] <- rep_len(
      new_values[[column]], length(display_rows)
    )
  }

  return(limits_table)
}


# Function to create limits for new calculation and display period with rule
# breaks
form_calculation_and_display_limits <- function(
    data,
    counter_at_period_start,
    period_length,
    chart) {
  # form calculation limits for first period
  limits_table <- form_calculation_limits(
    data = data,
    counter = counter_at_period_start,
    period_length = period_length,
    chart = chart
  )


  # extend display limits to end
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


#' Form limits over calculation periods specified in advance
#'
#' Takes calculation periods as given, and for each in turn forms
#' calculation limits from the period's points, applies display limits for
#' subsequent points prior to the next period, and marks the rule breaks, in the
#' same way as the algorithm does for a period it establishes. The last period's
#' display limits run to the end of `data`.
#'
#' @param data A data frame with one row per point of the series that has an
#'   observation, in order of `x`, as `compact_series()` returns it. It has the
#'   columns `x` and `series`, and any others the chart's limit calculation
#'   reads, such as `n` for a P chart.
#' @param periods A data frame with one row per calculation period, in order of
#'   `x`, and the columns `first_x` and `last_x`. The rows of `data` in a period
#'   are those whose `x` is at least `first_x` and at most `last_x`, so both end
#'   points are included. The first period also includes any rows of `data`
#'   before its `first_x`, so that it starts at the first row of `data`. Where
#'   no row of `data` is in a period, the rows from its `first_x` up to the next
#'   period have no centre line and no limits; where that period is the first,
#'   the rows before the first period with a row of `data` have none either.
#' @param chart The chart being analysed.
#'
#' @return `data`, one row per row, with the centre line, the limits, the period
#'   type, the excluded points and the rule breaks added.
#' @noRd
form_limits_over_periods <- function(data,
                                     periods,
                                     chart) {
  limits_table <- data
  formed <- FALSE

  for (period in seq_len(nrow(periods))) {
    in_period <- data$x >= periods$first_x[[period]] &
      data$x <= periods$last_x[[period]]

    if (!any(in_period)) {
      if (formed) {
        limits_table <- form_period_without_points(limits_table,
          counter = match(TRUE, data$x >= periods$first_x[[period]]),
          chart = chart
        )
      }

      next
    }

    if (period == 1L) {
      counter <- 1L
      period_length <- max(which(in_period))
    } else {
      counter <- which(in_period)[[1L]]
      period_length <- sum(in_period)
    }

    limits_table <- form_calculation_and_display_limits(
      data = limits_table,
      counter_at_period_start = counter,
      period_length = period_length,
      chart = chart
    )
    formed <- TRUE
  }

  return(limits_table)
}


#' Positions in a calculation period excluded because the period mirrors the
#' location chart's
#'
#' For a chart with `location_periods` where `mirrored_period_start_excluded()`
#' is TRUE, the point at the first `x` of each of the location chart's periods,
#' except the point in the first row of `data`, which starts the first period
#' of the segment.
#'
#' @param chart The chart being analysed.
#' @param data The series being analysed, as in `form_calculation_limits()`.
#' @param counter The row of `data` at which the period starts.
#' @param period_length The number of rows of `data` in the period.
#'
#' @return Positions in the period, counted from 1; empty for every other chart.
#' @noRd
mirrored_exclusions <- function(chart,
                                data,
                                counter,
                                period_length) {
  if (is.null(chart$location_periods) ||
    !mirrored_period_start_excluded(chart)) {
    return(integer(0))
  }

  period_x <- data$x[counter:(counter + period_length - 1L)]
  period_starts <- setdiff(chart$location_periods$first_x, data$x[[1L]])

  return(which(period_x %in% period_starts))
}


#' Remove the centre line and limits from the rows after the start of a period
#' with no points
#'
#' The rows from `counter` to the end of the table become display rows with no
#' centre line, no limits and no period statistics, starting a new period at
#' `counter`, and the rule breaks are marked again, as where a period is
#' established.
#'
#' @param limits_table The limits table being built, holding an earlier
#'   period's limits.
#' @param counter The first row at or after the start of the period, or NA where
#'   there is none.
#' @param chart The chart being analysed.
#'
#' @return `limits_table`, with those rows changed.
#' @noRd
form_period_without_points <- function(limits_table,
                                       counter,
                                       chart) {
  if (is.na(counter)) {
    return(limits_table)
  }

  rows <- counter:nrow(limits_table)

  for (column in c("cl", "ucl", "lcl", period_statistics_columns(chart))) {
    limits_table[[column]][rows] <- NA_real_
  }
  limits_table$period_type[rows] <- "display"
  limits_table$break_point[counter] <- TRUE

  return(add_rule_breaks_respecting_periods(
    limits_table = limits_table,
    counter = counter,
    centre_line_tolerance = chart$centre_line_tolerance,
    shift_rule_threshold = chart$shift_rule_threshold
  ))
}


#' The calculation periods in an analysed table
#'
#' @param table An analysed table, `chart$result$table`.
#'
#' @return A data frame with one row per calculation period, in order of `x`:
#'   `first_x` and `last_x`, the `x` of the period's first and last rows, both
#'   rows being part of the period.
#' @noRd
calculation_periods <- function(table) {
  return(
    table %>%
      dplyr::filter(period_type == "calculation") %>%
      dplyr::group_by(plot_period) %>%
      dplyr::summarise(first_x = min(x), last_x = max(x)) %>%
      dplyr::arrange(first_x) %>%
      dplyr::select(first_x, last_x) %>%
      as.data.frame()
  )
}
