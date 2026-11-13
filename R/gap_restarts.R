# Restarting the algorithm after a long gap in the series


#' The restarts after long gaps, and the stranded points
#'
#' `chart$data` has one row per point of the series. A gap is a run of
#' consecutive points with no value on the location chart, and a long gap is
#' one of at least `restart_after_gap` points between two points with a value.
#' The algorithm restarts at the first point with a value after each long gap.
#' Where that point is in a range given by `prevent_establishment_in`, the
#' restart moves to the first point with a value after the range. The points
#' with a value between the end of the gap and the restart are stranded: drawn,
#' but not analysed. Where no point with a value follows the range, there is no
#' restart, and every point after the gap is stranded. Where a forced restart
#' falls among the stranded points, they end there, and the algorithm restarts
#' at the forced restart instead.
#'
#' @param chart The chart being analysed, with its series prepared.
#'
#' @return A list of rows of `chart$data`: `restarts`, the points at which the
#'   algorithm restarts after a gap; and `stranded_points`, the stranded
#'   points.
#' @noRd
gap_restarts <- function(chart) {
  restarts <- integer(0)
  stranded_points <- integer(0)

  with_value <- which(observed_rows(chart,
    data = chart$data,
    location = TRUE
  ))

  if (is.infinite(chart$restart_after_gap) || length(with_value) < 2L) {
    return(list(restarts = restarts, stranded_points = stranded_points))
  }

  gap_lengths <- diff(with_value) - 1L
  gap_ends <- with_value[-1L][gap_lengths >= chart$restart_after_gap]
  forced <- forced_restart_rows(chart)

  for (gap_end in gap_ends) {
    restart <- first_point_outside_ranges(chart,
      row = gap_end,
      with_value = with_value
    )

    stranded_until <- if (is.na(restart)) nrow(chart$data) + 1L else restart
    forced_among_stranded <- forced[forced >= gap_end & forced < stranded_until]

    if (length(forced_among_stranded) > 0L) {
      stranded_until <- min(forced_among_stranded)
      restart <- NA_integer_
    }

    stranded_points <- c(
      stranded_points,
      with_value[with_value >= gap_end & with_value < stranded_until]
    )

    if (!is.na(restart)) {
      restarts <- c(restarts, restart)
    }
  }

  return(list(restarts = restarts, stranded_points = stranded_points))
}


#' The first point with a value, from a given point on, outside every
#' prevented range
#'
#' @param chart The chart being analysed. `chart$data` has one row per point.
#' @param row The row in `chart$data` of a point with a value.
#' @param with_value The rows in `chart$data` of the points that have a value.
#'
#' @return A row of `chart$data`, or NA where every point with a value from
#'   `row` on is in a range given by `prevent_establishment_in`.
#' @noRd
first_point_outside_ranges <- function(chart,
                                       row,
                                       with_value) {
  repeat {
    after <- row_after_prevented_range(chart,
      table = chart$data,
      counter = row
    )

    if (is.null(after)) {
      return(row)
    }

    row <- with_value[with_value >= after][1L]

    if (is.na(row)) {
      return(NA_integer_)
    }
  }
}
