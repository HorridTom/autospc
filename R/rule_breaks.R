#' Add a column for each rule of a set, other than rule 1 and the shift rule
#'
#' One logical column per rule, named `rule<id>`: TRUE where the point is part
#' of a pattern that breaks the rule, FALSE where it is not, and NA where the
#' rule is not applied. The table is one period: a pattern is looked for within
#' it and does not continue beyond it.
#'
#' @param table One period of an analysed table, with the columns `series`,
#'   `cl`, `sd_estimate` and `above_or_below_cl`, those `standard_error_at()`
#'   needs for the chart, and optionally `run_break`.
#' @param rules The set's rules other than rule 1 and the shift rule, as
#'   `rule_set()` holds them.
#' @param chart The chart being analysed.
#' @param trend_applies Whether the trend rule is applied to this chart, as
#'   `trend_rule_applies()` gives it.
#'
#' @return `table`, with the columns added.
#' @noRd
add_rule_set_columns <- function(table,
                                 rules,
                                 chart,
                                 trend_applies) {
  points <- variables_for_rules(table, chart = chart)

  for (rule in rules) {
    broken <- if (identical(rule$kind, "trend") && !trend_applies) {
      rep(NA, nrow(table))
    } else {
      rule_breaks(rule, points = points)
    }

    table[[paste0("rule", rule$id)]] <- broken
  }

  return(table)
}


#' Measure each point's distance from the centre line, its side and its stretch
#'
#' @param table One period of an analysed table, as for
#'   `add_rule_set_columns()`.
#' @param chart The chart being analysed.
#'
#' @return A data frame with one row per row of `table` and the columns
#'   `series`; `z`, the point's distance from the centre line in standard
#'   errors, NA where the row has no limits; `side`, as `above_or_below_cl`;
#'   and `stretch`, which numbers the stretches of consecutive points within
#'   which patterns are looked for. A pattern cannot continue from one stretch
#'   into the next. A new stretch starts after a gap that ends a pattern
#'   (`run_break`) and at a point with no value.
#' @noRd
variables_for_rules <- function(table,
                                chart) {
  standard_error <- standard_error_at(chart,
    sd_estimate = table$sd_estimate,
    rows = table
  )

  return(data.frame(
    series = table$series,
    z = distance_in_standard_errors(
      series = table$series,
      cl = table$cl,
      standard_error = standard_error
    ),
    side = table$above_or_below_cl,
    stretch = cumsum(
      dplyr::coalesce(table$run_break, FALSE) | is.na(table$series)
    )
  ))
}


#' The distance of each point from the centre line in standard errors
#'
#' Where the standard error is 0, a point on the centre line is 0 away from it,
#' and a point off it infinitely far.
#'
#' @return numeric, one value per point, NA where there is no centre line or no
#'   standard error
#' @noRd
distance_in_standard_errors <- function(series,
                                        cl,
                                        standard_error) {
  difference <- series - cl

  z <- difference / standard_error
  z[which(standard_error == 0 & difference == 0)] <- 0

  return(z)
}


#' Where a zone rule is broken
#'
#' A window of `m` consecutive points breaks the rule where at least `k` of
#' them are in the zone, and those `k` or more are marked. For a rule whose
#' points must be on the same side of the centre line, each side is looked at
#' separately, and a point on the other side takes a place in a window without
#' counting towards the `k`.
#'
#' A point on the zone's inner boundary neither counts towards a pattern nor
#' ends one, and takes no place in a window. Where the zone starts at the
#' centre line (`beyond = 0`), that is a point with no side, within
#' `centre_line_tolerance` of it, as for the shift rule; otherwise it is a
#' point exactly `beyond` standard errors from the centre line. A zone with no
#' `beyond` includes the centre line and has no inner boundary.
#'
#' A point with no limits ends a pattern, takes no place in a window, and the
#' rule is not applied to it.
#'
#' @param rule An `autospc_zone_rule`.
#' @param points Each point's distance from the centre line, its side and its
#'   stretch, as `variables_for_rules()` gives them.
#'
#' @return logical, one value per point
#' @noRd
rule_breaks.autospc_zone_rule <- function(rule,
                                          points) {
  distance <- abs(points$z)
  side <- dplyr::coalesce(points$side, 0L)
  no_limits <- is.na(distance)

  in_zone <- !no_limits
  on_inner_boundary <- rep(FALSE, length(distance))
  if (!is.null(rule$beyond)) {
    in_zone <- in_zone & distance %>>% rule$beyond
    on_inner_boundary <- if (rule$beyond == 0) {
      !no_limits & side == 0L
    } else {
      !no_limits & distance %==% rule$beyond
    }
  }
  if (!is.null(rule$within)) {
    in_zone <- in_zone & distance %<=% rule$within
  }
  in_zone <- in_zone & !on_inner_boundary

  # a point with no limits ends a pattern
  stretch <- paste(points$stretch, cumsum(no_limits))
  takes_place <- !no_limits & !on_inner_boundary

  if (identical(rule$side, "either")) {
    broken <- k_of_m_breaks(in_zone,
      takes_place = takes_place, stretch = stretch, k = rule$k, m = rule$m
    )
  } else {
    broken <- k_of_m_breaks(in_zone & side == 1L,
      takes_place = takes_place, stretch = stretch, k = rule$k, m = rule$m
    ) | k_of_m_breaks(in_zone & side == -1L,
      takes_place = takes_place, stretch = stretch, k = rule$k, m = rule$m
    )
  }

  broken[no_limits] <- NA

  return(broken)
}


#' Where a trend rule is broken
#'
#' `k` consecutive points each higher than the one before, or each lower. A
#' point equal to the one before it neither continues nor ends a trend, and is
#' marked where it lies inside one.
#'
#' @param rule An `autospc_trend_rule`.
#' @param points Each point's distance from the centre line, its side and its
#'   stretch, as `variables_for_rules()` gives them.
#'
#' @return logical, one value per point, NA where the point has no value
#' @noRd
rule_breaks.autospc_trend_rule <- function(rule,
                                           points) {
  direction <- step_directions(points)
  broken <- rep(FALSE, length(direction))

  # the trend being followed: its direction, its first point, its last point
  # and the number of points in it
  trend <- list(direction = NA_integer_, first = NA, last = NA, points = 0L)

  for (i in seq_along(direction)) {
    if (is.na(direction[i])) {
      broken <- mark_pattern(broken, trend, k = rule$k)
      trend$direction <- NA_integer_
    } else if (direction[i] == 0L) {
      next
    } else if (identical(direction[i], trend$direction)) {
      trend$last <- i
      trend$points <- trend$points + 1L
    } else {
      broken <- mark_pattern(broken, trend, k = rule$k)
      trend <- list(
        direction = direction[i], first = i - 1L, last = i,
        points = 2L
      )
    }
  }

  broken <- mark_pattern(broken, trend, k = rule$k)
  broken[is.na(points$series)] <- NA

  return(broken)
}


#' Where an alternating rule is broken
#'
#' `k` consecutive points going up and down in turn. A point equal to the one
#' before it ends the pattern.
#'
#' @param rule An `autospc_alternating_rule`.
#' @param points Each point's distance from the centre line, its side and its
#'   stretch, as `variables_for_rules()` gives them.
#'
#' @return logical, one value per point, NA where the point has no value
#' @noRd
rule_breaks.autospc_alternating_rule <- function(rule,
                                                 points) {
  direction <- step_directions(points)
  broken <- rep(FALSE, length(direction))

  # the pattern being followed: the direction of its last step, its first
  # point, its last point and the number of points in it
  pattern <- list(direction = NA_integer_, first = NA, last = NA, points = 0L)

  for (i in seq_along(direction)) {
    if (is.na(direction[i]) || direction[i] == 0L) {
      broken <- mark_pattern(broken, pattern, k = rule$k)
      pattern$direction <- NA_integer_
    } else if (identical(direction[i], -pattern$direction)) {
      pattern$direction <- direction[i]
      pattern$last <- i
      pattern$points <- pattern$points + 1L
    } else {
      broken <- mark_pattern(broken, pattern, k = rule$k)
      pattern <- list(
        direction = direction[i], first = i - 1L, last = i,
        points = 2L
      )
    }
  }

  broken <- mark_pattern(broken, pattern, k = rule$k)
  broken[is.na(points$series)] <- NA

  return(broken)
}


# Helpers


#' The windows of `m` consecutive points with at least `k` that qualify
#'
#' Windows are taken within each stretch, over the points that take a place in
#' a window.
#'
#' @param qualifies Whether each point qualifies.
#' @param takes_place Whether each point takes a place in a window.
#' @param stretch The stretch each point is in.
#' @param k,m As for `zone_rule()`.
#'
#' @return logical, one value per point: TRUE for each qualifying point of a
#'   window with at least `k` that qualify
#' @noRd
k_of_m_breaks <- function(qualifies,
                          takes_place,
                          stretch,
                          k,
                          m) {
  broken <- rep(FALSE, length(qualifies))

  for (rows in split(which(takes_place), stretch[takes_place])) {
    if (length(rows) < m) {
      next
    }

    in_window <- c(0L, cumsum(qualifies[rows]))
    starts <- seq_len(length(rows) - m + 1L)
    hits <- which(in_window[starts + m] - in_window[starts] >= k)

    for (start in hits) {
      window <- rows[start:(start + m - 1L)]
      broken[window[qualifies[window]]] <- TRUE
    }
  }

  return(broken)
}


#' The direction of each step from the point before
#'
#' @param points Each point's distance from the centre line, its side and its
#'   stretch, as `variables_for_rules()` gives them.
#'
#' @return integer, one value per point: 1 up, -1 down, 0 level, NA for the
#'   first point of a stretch and for a point with no value
#' @noRd
step_directions <- function(points) {
  direction <- as.integer(sign(c(NA, diff(points$series))))
  first_of_stretch <- c(TRUE, diff(points$stretch) != 0L)
  direction[first_of_stretch] <- NA_integer_

  return(direction)
}


#' Mark the points of a pattern with enough points
#'
#' @param broken The marks so far, one per point.
#' @param pattern A list holding the pattern's `direction`, its `first` and
#'   `last` points, and the number of `points` in it. A pattern whose
#'   `direction` is NA has ended and is not marked again.
#' @param k The number of points that breaks the rule.
#'
#' @return `broken`, with the pattern's points marked where it has at least `k`
#' @noRd
mark_pattern <- function(broken,
                         pattern,
                         k) {
  if (!is.na(pattern$direction) && pattern$points >= k) {
    broken[pattern$first:pattern$last] <- TRUE
  }

  return(broken)
}
