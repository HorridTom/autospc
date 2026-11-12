# Function to determine whether there are enough data points left to form a new
# period
enough_data_for_new_period <- function(data,
                                       counter,
                                       chart) {
  # the rows from counter to the end of the data
  remaining_data <- data[seq_len(nrow(data)) >= counter, ]

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
#' calculation period, at the first point of the series or at any forced
#' restart. Read from `chart$data` and the chart's parameters.
#'
#' @return TRUE or FALSE
#' @noRd
enough_data_for_limits <- function(chart) {
  segments <- segment_rows(chart)

  enough <- vapply(
    seq_along(segments),
    function(segment) {
      enough_data_for_new_period(
        data = segment_data(chart,
          rows = segments[[segment]],
          first_segment = segment == 1L
        ),
        counter = 1L,
        chart = chart
      )
    },
    logical(1L)
  )

  return(any(enough))
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

    statistics <- calculate_limits(
      chart = chart,
      period = calculation_period,
      exclusion_points = exclusion_points
    )

    limits <- limits_from_statistics(
      chart = chart,
      statistics = statistics,
      rows = calculation_period
    )

    series <- calculation_period$series
    cl <- statistics$cl
    ucl <- limits$ucl
    lcl <- limits$lcl

    # whether each point is outside the limits, and whether it is above or
    # below the centre line (NA for a point on it)
    rule1 <- (series > ucl) | (series < lcl)
    above_cl <- ifelse(series > cl, TRUE, ifelse(series < cl, FALSE, NA))

    # how far each point outside the limits lies beyond them. NA for points
    # within the limits and for points already excluded
    rule1_distance <- ifelse(rule1 & above_cl,
      series - ucl,
      ifelse(rule1 & !above_cl, lcl - series, NA)
    )
    rule1_distance[seq_along(series) %in% exclusion_points] <- NA

    if (sum(!is.na(rule1_distance)) == 0) {
      # If no extremes, set furthest_extreme to -Inf
      furthest_extreme <- -Inf
    } else {
      # Otherwise, set furthest extreme to the greatest distance from limit
      furthest_extreme <- max(rule1_distance, na.rm = T)
    }
    exclusion_point <- which(rule1_distance == furthest_extreme)

    # Add next exclusion point and furthest extreme to the vectors
    furthest_extremes <- c(furthest_extremes, furthest_extreme)
    exclusion_points <- c(exclusion_points, exclusion_point)
    i <- i + 1
  }

  # Check whether there are more than max_exclusions exclusion points (due to
  # ties)
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
  # A rule 2 break starts where a run that is long enough to break rule 2
  # commences. Using run_start, rather than comparing each point with the one
  # before, means a run that a gap ended (na_ends_run) is not taken to continue
  # into the run after the gap, even when both are on the same side of the
  # centre line.
  limits_table <- limits_table %>%
    dplyr::mutate(start_of_rule2_break = rule2 & run_start)

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
  # overhanging_reversions controls whether "overhanging" reversions into
  # following display period prevent re-establishment
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

  # number the runs
  side <- limits_table_candidate$above_or_below_cl
  previous_side <- c(NA, side)[seq_along(side)]
  new_run <- is.na(previous_side) | (side != 0 & side != previous_side)
  run_count <- cumsum(new_run)
  limits_table_candidate[["lagged_above_or_below_cl"]] <- previous_side
  limits_table_candidate[["new_run"]] <- new_run
  limits_table_candidate[["run_count"]] <- run_count

  # a rule 2 break in the opposite direction to the triggering run. The first
  # run is not counted: if it is in the same direction as the triggering run
  # it cannot be an opposite break, and if it is in the opposite direction it
  # is only the move to the new level
  limits_table_candidate[["opposite_break"]] <- limits_table_candidate$rule2 &
    (side != triggering_rule_break_direction) & run_count > 1

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
    triggering_rule_break_direction) {
  # number the periods: a new period starts at the first row, and wherever
  # period_type differs from the row before
  period_type <- candidate_limits_table$period_type
  previous_period_type <- c(NA, period_type)[seq_along(period_type)]
  new_period <- is.na(previous_period_type) |
    previous_period_type != period_type
  period_count <- cumsum(new_period)

  # keep the last calculation period and the rows after it
  last_calc_period <- max(period_count[which(period_type == "calculation")])

  data <- candidate_limits_table[which(period_count >= last_calc_period), ]

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
    # it is, if every point from there to the end that is off the centre line
    # is on the same side
    sides <- data$above_or_below_cl[
      seq_len(nrow(data)) >= last_point_in_last_calc_period
    ]
    final_calc_run_is_final_run <- is_numeric_vector_constant(
      sides[which(sides != 0)]
    )

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


#' The number of points in the series that have a value
#'
#' What the floating median is taken over, and what decides whether there are
#' enough of them for one.
#'
#' @param table The analysed table.
#'
#' @return A count.
#' @noRd
points_with_a_value <- function(table) {
  return(sum(!is.na(table$series)))
}


#' Check whether a floating median is required, and if so add a column to the
#' table holding its values
#'
#' A median is taken at the end of each segment of the series: the whole series
#' where there is no restart, or the rows from one restart to the next.
#' Each is taken over the segment's last `floating_median_n` points that have a
#' value, and drawn from the first of those points to the last, so not over any
#' points with no value that follow them. A segment with fewer such points has
#' no median. Where the last segment is too short, `floating_median = "yes"`
#' gives a warning, because the caller asked for a median; `"auto"` does not,
#' because the caller left the decision to the package.
#'
#' @param table The analysed table.
#' @param floating_median One of "no", "yes" and "auto".
#' @param floating_median_n The number of points the median is taken over.
#' @param segments The rows of each segment, as `segment_rows()` gives them.
#'   The rows the extension added beyond the end of the data are in none, so
#'   no median is taken over them.
#'
#' @return `table`, with a `median` column where a floating median is drawn.
#' @noRd
floating_median_column <- function(table,
                                   floating_median,
                                   floating_median_n,
                                   segments) {
  # the column is always returned, and holds a value only on the rows a
  # median is drawn over
  medians <- rep(NA_real_, nrow(table))

  if (!identical(floating_median, "no")) {
    for (segment in seq_along(segments)) {
      rows <- segments[[segment]]

      medians[rows] <- segment_floating_median(
        segment_table = dplyr::slice(table, rows),
        floating_median = floating_median,
        floating_median_n = floating_median_n,
        warn = identical(floating_median, "yes") &&
          segment == length(segments),
        restarted = length(segments) > 1L
      )
    }
  }

  table <- table %>%
    dplyr::mutate(median = medians)

  return(table)
}


#' The floating median over one segment of the series
#'
#' @param segment_table The rows of the analysed table in the segment.
#' @param floating_median One of "yes" and "auto".
#' @param floating_median_n The number of points the median is taken over.
#' @param warn Whether to warn when the segment is too short for a median.
#' @param restarted Whether the series has a restart, which decides how
#'   the warning describes the segment.
#'
#' @return One value per row of `segment_table`: the median on the rows it is
#'   taken over, and NA elsewhere.
#' @noRd
segment_floating_median <- function(segment_table,
                                    floating_median,
                                    floating_median_n,
                                    warn,
                                    restarted) {
  no_median <- rep(NA_real_, nrow(segment_table))
  points <- points_with_a_value(segment_table)

  if (points < floating_median_n) {
    if (warn) {
      warning(paste0(
        "A floating median is taken over the last ", floating_median_n,
        " points that have a value, and ",
        if (restarted) {
          "the segment of this series after its last restart has "
        } else {
          "this series has "
        },
        points, ". No floating median is drawn."
      ))
    }

    return(no_median)
  }

  median_from_x <- segment_table %>%
    dplyr::mutate(non_missing = !is.na(series)) %>%
    dplyr::arrange(dplyr::desc(x)) %>%
    dplyr::mutate(cumulative_num_non_missing = cumsum(non_missing)) %>%
    dplyr::filter(cumulative_num_non_missing == floating_median_n) %>%
    dplyr::pull(x) %>%
    max()

  window <- segment_table %>%
    dplyr::filter(x >= median_from_x)

  addfloating_median <- switch(
    EXPR = floating_median,
    yes = TRUE,
    # a row with no value is not part of a shift rule break, so its NA counts
    # as no break rather than making the whole test missing
    auto = any(window$rule2, na.rm = TRUE),
    FALSE
  )

  if (!addfloating_median) {
    return(no_median)
  }

  # drawn from the first of those points to the last
  median_to_x <- max(segment_table$x[!is.na(segment_table$series)])

  return(dplyr::if_else(
    segment_table$x >= median_from_x & segment_table$x <= median_to_x,
    stats::median(window$series, na.rm = TRUE),
    NA_real_
  ))
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
  # no rule 2 break at counter
  if (!(table$rule2[seq_len(nrow(table)) == counter])) {
    return(FALSE)
  }

  # the first row at or after counter where a run starts, or one past the end
  # of the table if there is none
  run_starts <- which(table$run_start)
  start_of_next_run <- utils::head(run_starts[run_starts >= counter], 1L)

  if (length(start_of_next_run) == 0L) {
    start_of_next_run <- nrow(table) + 1L
  }

  # counted the same way add_rule_two() counts a run, so that the two agree on
  # whether the threshold is reached: points on the centre line do not count
  side <- unlist(table$above_or_below_cl)[counter:(start_of_next_run - 1L)]

  result <- sum(!is.na(side) & side != 0L) >= shift_rule_threshold

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
#' `extension_row()` sets these columns itself on the rows
#' `extend_limits_beyond_data()` adds past the end of the data, from the period
#' those rows continue.
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
