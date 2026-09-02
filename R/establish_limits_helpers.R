# Function to determine whether there are enough data points left to form a new
# period
enough_data_for_new_period <- function(data,
                                       counter,
                                       chart) {
  remaining_data <- data %>%
    dplyr::filter(dplyr::row_number() >= counter)

  num_remaining_non_missing_data_points <- n_effective_points(
    chart = chart,
    data = remaining_data
  )

  enough_data <- num_remaining_non_missing_data_points >= chart$period_min

  return(enough_data)
}


#' Can this chart have limits?
#'
#' Whether the prepared series is long enough for the algorithm to form one
#' calculation period. Read from `chart$data` and the chart's parameters.
#'
#' @return TRUE or FALSE
#' @noRd
enough_data_for_limits <- function(chart) {
  return(enough_data_for_new_period(
    data = chart$data,
    counter = 1L,
    chart = chart
  ))
}


# Function to find most extreme points outside of control limits and return
# their positions
# period_length is the length of *this* period, which is baseline_length for
# the first one, so it is not chart$period_min and cannot be read off the chart.
find_extremes <- function(data,
                          chart,
                          counter,
                          period_length) {
  # initialise variables
  i <- 1
  exclusion_points <- NULL
  furthest_extremes <- NULL

  while (i <= chart$max_exclusions) {
    calculation_period <- data[counter:(counter + period_length - 1), ]

    limits_list <- calculate_limits(
      chart = chart,
      period = calculation_period,
      exclusion_points = exclusion_points
    )

    calculation_period$cl <- limits_list$cl
    calculation_period$ucl <- limits_list$ucl
    calculation_period$lcl <- limits_list$lcl

    calculation_period <- calculation_period %>%
      dplyr::select(x, y, ucl, lcl, cl)

    calculation_period <- add_rule_breaks(
      calculation_period,
      centre_line_tolerance = chart$centre_line_tolerance,
      shift_rule_threshold = chart$shift_rule_threshold
    )
    calculation_period <- calculation_period %>%
      dplyr::mutate(above_cl = ifelse(y > cl,
        TRUE,
        ifelse(y < cl,
          FALSE,
          NA
        )
      )) %>%
      dplyr::mutate(rule1_distance = ifelse(rule1 & above_cl,
        y - ucl,
        ifelse(rule1 & !above_cl,
          lcl - y,
          NA
        )
      )) %>%
      # Set already established extremes as NA
      dplyr::mutate(rule1_distance = ifelse(dplyr::row_number() %in%
        exclusion_points,
      NA,
      rule1_distance
      ))

    if (sum(!is.na(calculation_period$rule1_distance)) == 0) {
      # If no extremes, set furthest_extreme to -Inf
      furthest_extreme <- -Inf
    } else {
      # Otherwise, set furthest extreme to the greatest distance from limit
      furthest_extreme <- max(calculation_period$rule1_distance, na.rm = T)
    }
    exclusion_point <- which(
      calculation_period$rule1_distance == furthest_extreme
    )

    # Add next exclusion point and furthest extreme to the vectors
    furthest_extremes <- c(furthest_extremes, furthest_extreme)
    exclusion_points <- c(exclusion_points, exclusion_point)
    i <- i + 1
  }

  # Check whether there are more than 3 exclusion points (due to points with the
  # same values)
  if (length(exclusion_points) > chart$max_exclusions) {
    exclusion_points <- exclusion_points[1:chart$max_exclusions]
  }

  if (length(exclusion_points) == 0) {
    NULL
  } else {
    exclusion_points
  }
}


# Function to scan to see where start of each rule 2 break is -
# returns list of these points
rule2_break_start_positions <- function(limits_table, counter) {
  # Add a column for start of rule 2 breaks - i.e. if there is a rule 2
  # highlight and that is not preceded by a rule 2 highlight
  limits_table <- limits_table %>%
    dplyr::mutate(start_of_rule2_break = rule2 &
      (rule2 != dplyr::lag(rule2) |
        different_cl_side(above_or_below_cl,
          y = dplyr::lag(above_or_below_cl)
        )))

  next_rule_break_positions <-
    (which(limits_table$start_of_rule2_break[
      counter:nrow(limits_table)
    ] == T)) + counter - 1

  next_rule_break_positions
}

different_cl_side <- function(x, y) {
  return(x * y == -1)
}


# Function to identify whether there has been a rule break in the opposite
# direction in calc period returns TRUE for rule break in opposite direction
# within candidate calc period including hang over into display set counter to
# beginning of candidate limits
identify_opposite_break <- function(limits_table,
                                    counter,
                                    period_min,
                                    triggering_rule_break_direction,
                                    centre_line_tolerance,
                                    shift_rule_threshold,
                                    overhanging_reversions = TRUE) {
  # start rule breaks from candidate period so as not to include "overhanging"
  # rule breaks from prev period.
  # overhanging_reversions controls "overhanging" reversions into following
  # display period prevent re-establishment
  candidate_start <- counter
  if (overhanging_reversions) {
    candidate_end <- nrow(limits_table)
  } else {
    candidate_end <- counter + period_min - 1L
  }

  limits_table_candidate <- limits_table[candidate_start:candidate_end, ]
  limits_table_candidate <- add_rule_breaks(
    limits_table_candidate,
    centre_line_tolerance = centre_line_tolerance,
    shift_rule_threshold = shift_rule_threshold
  )

  limits_table_candidate <- limits_table_candidate %>%
    dplyr::mutate(
      lagged_above_or_below_cl = dplyr::lag(above_or_below_cl),
      new_run = dplyr::if_else(
        (is.na(lagged_above_or_below_cl) |
          (above_or_below_cl != 0 &
            above_or_below_cl != lagged_above_or_below_cl)),
        TRUE,
        FALSE
      ),
      run_count = cumsum(new_run)
    )

  # looks for a rule break in the opposite direction within the candidate period
  # Don't consider the first run as a potential opposite rule break. If it is
  # in the same direction as the triggering run, it can't be an opposite break,
  # and if it is in the opposite direction, it just represents a transition on
  # the way to the new level
  limits_table_candidate <- limits_table_candidate %>%
    dplyr::mutate(opposite_break = dplyr::if_else(
      rule2 & (above_or_below_cl != triggering_rule_break_direction) &
        run_count > 1,
      TRUE,
      FALSE
    ))

  if (!overhanging_reversions & nrow(limits_table) > candidate_end) {
    limits_table_tail <- limits_table[(candidate_end + 1L):nrow(limits_table), ]
    limits_table_tail <- limits_table_tail %>%
      dplyr::mutate(opposite_break = FALSE)

    limits_table_candidate <- limits_table_candidate %>%
      dplyr::bind_rows(limits_table_tail)
  }

  # return list containing: boolean of whether there is an opposite break,
  # the next rule break position if applicable,
  # the candidate table
  if (all(limits_table_candidate$opposite_break == FALSE)) {
    # if there are no further rule breaks
    output <- list(FALSE, NA, limits_table_candidate)
  } else {
    next_rule_break_position <- min(
      which(limits_table_candidate$opposite_break == TRUE)
    ) + counter - 1

    last_point_in_calc_period <- utils::tail(
      which(limits_table_candidate$period_type == "calculation"),
      n = 1L
    ) + counter - 1

    if (next_rule_break_position > last_point_in_calc_period) {
      # No rule break in opposite direction
      output <- list(FALSE, NA, limits_table_candidate)
    } else {
      output <- list(TRUE, next_rule_break_position, limits_table_candidate)
    }
  }

  output
}


# Function to establish whether the final run in the candidate calculation
# period prevents limits being re-established (for no regrets)
final_run_prevents_re_establishment <- function(
  candidate_limits_table,
  triggering_rule_break_direction
) {
  # Filter data to exclude everything prior to the last calculation period
  data <- candidate_limits_table
  data <- data %>%
    dplyr::mutate(
      lagged_period_type = dplyr::lag(period_type),
      new_period = dplyr::if_else(
        (is.na(lagged_period_type) |
          lagged_period_type != period_type), TRUE, FALSE
      ),
      period_count = cumsum(new_period)
    )
  period_table <- data %>%
    dplyr::distinct(period_type, period_count)

  last_calc_period <- period_table %>%
    dplyr::filter(period_type == "calculation") %>%
    dplyr::pull(period_count) %>%
    max()

  data <- data %>%
    dplyr::filter(period_count >= last_calc_period)

  # handles NA value that appears sometimes at the end of the data
  if (is.na(data$y[nrow(data)])) {
    data <- data[1:(nrow(data) - 1), ]
  }

  # identify the row number of the last point, in the last calculation period,
  # that is not on the centre line
  last_point_in_last_calc_period <- utils::tail(
    which(data$period_type == "calculation" &
      data$above_or_below_cl != 0),
    n = 1L
  )

  if (length(last_point_in_last_calc_period) != 1L) {
    # all the points in the last calculation period are on the centre line
    return(FALSE)
  }

  final_direction <- data[
    last_point_in_last_calc_period,
    "above_or_below_cl"
  ]

  if (final_direction == triggering_rule_break_direction) {
    # the last point in the final calculation period is in the same direction
    # as the triggering run, and therefore there is no potential for a rule-
    # breaking run in the opposite direction spanning the end of the last
    # calculation period
    return(FALSE)
  } else {
    # the last point in the final calculation period is in the opposite
    # direction to the triggering rule break

    # is the final run of the final calculation period the final run overall?
    final_calc_run_is_final_run <- data %>%
      dplyr::filter(
        dplyr::row_number() >= last_point_in_last_calc_period,
        above_or_below_cl != 0
      ) %>%
      dplyr::pull(above_or_below_cl) %>%
      is_numeric_vector_constant()

    if (final_calc_run_is_final_run) {
      # The final run in the final calculation period is also the final run
      # in the data. There are two cases: either a) it is a rule breaking
      # run, or b) it is not. In either case, there is *at least* potential for
      # a rbr in the opposite direction.
      return(TRUE)
    } else {
      # The final run in the final calculation period is not the final run in
      # the data. There are two cases: either a) it is a rbr, b) it is not.
      # (a) in this case, identify_opposite_break will identify it and prevent
      # limits being re-established at the triggering rule break.
      # (b) in this case, there is no reason to prevent re-establishment
      return(FALSE)
    }
  }
}


# Function to add rule breaks to data with many periods. Avoids issues with
# highlighting across periods. NB this function counts actual break points, not
# period starts, hence it relies on the break_point column not being TRUE on the
# first row.
add_rule_breaks_respecting_periods <- function(limits_table,
                                               counter,
                                               centre_line_tolerance,
                                               shift_rule_threshold) {
  # get breakpoint positions
  breakpoints <- which(limits_table$break_point)


  if (counter == 1 | length(breakpoints) == 0L) {
    # for first period, or cases where there is only one period

    # add rule breaks to all of data
    limits_table <- add_rule_breaks(
      x = limits_table,
      centre_line_tolerance = centre_line_tolerance,
      shift_rule_threshold = shift_rule_threshold
    )
  } else if (length(breakpoints) == 1) {
    # for data with 2 periods

    # split data into sections
    limits_table_top <- limits_table[1:(counter - 1), ]
    limits_table_bottom <- limits_table[counter:nrow(limits_table), ]

    # add rule breaks to the old and new periods separately
    limits_table_top <- add_rule_breaks(
      x = limits_table_top,
      centre_line_tolerance = centre_line_tolerance,
      shift_rule_threshold = shift_rule_threshold
    )
    limits_table_bottom <- add_rule_breaks(
      x = limits_table_bottom,
      centre_line_tolerance = centre_line_tolerance,
      shift_rule_threshold = shift_rule_threshold
    )

    # put data back together
    limits_table <- dplyr::bind_rows(
      limits_table_top,
      limits_table_bottom
    )
  } else if (length(breakpoints) >= 2) {
    # for data with 3 or more periods,
    # only re-run rule breaks on most recent 2 periods

    # find start of previous period
    no_of_breakpoints <- length(breakpoints)
    penultimate_breakpoint <- breakpoints[no_of_breakpoints - 1L]

    # split data into sections
    limits_table_top <- limits_table[1:(penultimate_breakpoint - 1L), ]
    limits_table_mid <- limits_table[penultimate_breakpoint:(counter - 1L), ]
    limits_table_bottom <- limits_table[counter:nrow(limits_table), ]

    # add rule breaks to the penultimate and new periods only
    limits_table_mid <- add_rule_breaks(
      x = limits_table_mid,
      centre_line_tolerance = centre_line_tolerance,
      shift_rule_threshold = shift_rule_threshold
    )
    limits_table_bottom <- add_rule_breaks(
      x = limits_table_bottom,
      centre_line_tolerance = centre_line_tolerance,
      shift_rule_threshold = shift_rule_threshold
    )

    # put data back together
    limits_table <- dplyr::bind_rows(
      limits_table_top,
      limits_table_mid,
      limits_table_bottom
    )
  }

  limits_table
}


# Helper function to establish whether all elements of a numeric vector are
# equal
is_numeric_vector_constant <- function(x) {
  diff(range(x)) < .Machine$double.eps^0.5
}


# Helper function to fill in NA values with previous non-NA value
fill_na <- function(x) {
  which.na <- c(which(!is.na(x)), length(x) + 1)
  values <- stats::na.omit(x)

  if (which.na[1] != 1) {
    which.na <- c(1, which.na)
    values <- c(values[1], values)
  }

  diffs <- diff(which.na)
  return(rep(values, times = diffs))
}


# Check whether a floating median is required, and if so add a column to the
# table holding its values
floating_median_column <- function(table,
                                   floating_median,
                                   floating_median_n) {
  median_from_x <- table %>%
    dplyr::mutate(non_missing_y = !is.na(y)) %>%
    dplyr::arrange(dplyr::desc(x)) %>%
    dplyr::mutate(cumulative_num_non_missing = cumsum(non_missing_y)) %>%
    dplyr::filter(cumulative_num_non_missing == floating_median_n) %>%
    dplyr::pull(x) %>%
    max()

  addfloating_median <- switch(
    EXPR = floating_median,
    yes = TRUE,
    auto = any(table %>%
      dplyr::filter(x >= median_from_x) %>%
      dplyr::pull(rule2)),
    FALSE
  )

  if (addfloating_median) {
    table <- table %>%
      dplyr::mutate(
        median =
          dplyr::if_else(x >= median_from_x,
            stats::median(
              table %>%
                dplyr::filter(x >= median_from_x) %>%
                dplyr::pull(y),
              na.rm = TRUE
            ),
            NA
          )
      )
  }

  return(table)
}


sign_chr <- function(x) {
  y <- dplyr::case_when(
    x < 0 ~ "01",
    x == 0 ~ "00",
    x > 0 ~ "10"
  )

  return(y)
}


counter_at_rule_break <- function(table,
                                  counter,
                                  shift_rule_threshold) {
  if (!(table %>%
    dplyr::filter(dplyr::row_number() == counter) %>%
    dplyr::pull(rule2))) {
    return(FALSE)
  }

  start_of_next_run <- table %>%
    dplyr::mutate(row_index = dplyr::row_number()) %>%
    dplyr::filter(
      row_index >= counter,
      run_start
    ) %>%
    dplyr::slice_head(n = 1L) %>%
    dplyr::pull(row_index)

  if (length(start_of_next_run) == 0L) {
    start_of_next_run <- nrow(table) + 1L
  }

  result <- start_of_next_run - counter >= shift_rule_threshold

  return(result)
}


#' Length of the first calculation period
#'
#' `baseline_length`, or everything available when the series is shorter.
#'
#' @return number of rows
#' @noRd
baseline_period_length <- function(chart,
                                   data) {
  return(min(chart$baseline_length, nrow(data)))
}


#' Describe the periods the algorithm formed
#'
#' Four columns, each a function of the limits table alone: where the limits
#' changed, where each period starts, an identifier for the period, and the
#' direction the centre line moved at each change.
#'
#' `plot_period` is re-derived by `extend_limits()` for any rows it adds beyond
#' the end of the data.
#'
#' @return `data`, with the four columns added
#' @noRd
add_period_columns <- function(data) {
  data <- data %>%
    dplyr::mutate(limit_change = ifelse(period_type == dplyr::lag(period_type),
      FALSE,
      TRUE
    ))

  data <- data %>%
    dplyr::mutate(period_start = dplyr::if_else(limit_change == TRUE |
      is.na(limit_change) |
      break_point == TRUE,
    dplyr::row_number(),
    NA_integer_
    ))

  data$period_start <- fill_na(data$period_start)

  data <- data %>%
    dplyr::mutate(
      plot_period = paste0(period_type, period_start),
      cl_change = sign(cl - dplyr::lag(cl))
    )

  return(data)
}
