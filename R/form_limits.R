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

  calculation_period$cl <- statistics$cl
  calculation_period$ucl <- limits$ucl
  calculation_period$lcl <- limits$lcl
  calculation_period$sd_estimate <- statistics$sd_estimate

  extra_columns <- limits_table_columns(chart)

  calculation_period <- calculation_period %>%
    dplyr::select(x, series, ucl, lcl, cl, dplyr::any_of("sd_estimate")) %>%
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
        dplyr::any_of("sd_estimate"),
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

    if ("sd_estimate.y" %in% names(limits_table)) {
      limits_table$sd_estimate <- dplyr::if_else(
        is.na(limits_table$sd_estimate.y),
        limits_table$sd_estimate.x,
        limits_table$sd_estimate.y
      )
    }

    limits_table <- limits_table %>%
      dplyr::mutate(break_point = (break_point |
        dplyr::row_number() == counter))

    limits_table <- limits_table %>%
      dplyr::select(
        x, series, dplyr::all_of(extra_columns), ucl, lcl, cl,
        dplyr::any_of("sd_estimate"),
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

  calculated <- dplyr::slice_head(limits_table, n = counter - 1L)
  display <- dplyr::slice(limits_table, counter:nrow(limits_table))

  limits <- limits_at_rows(
    chart = chart,
    statistics = period_statistics(dplyr::slice_tail(calculated, n = 1L)),
    rows = display
  )

  display <- display %>%
    dplyr::mutate(
      cl = limits$cl,
      ucl = limits$ucl,
      lcl = limits$lcl,
      sd_estimate = limits$sd_estimate,
      period_type = "display"
    )

  return(dplyr::bind_rows(calculated, display))
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
