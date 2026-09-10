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
