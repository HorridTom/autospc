# Splitting a series at restarts, and joining what the segments give


#' The rows at which the series restarts
#'
#' At each forced restart, and after each long gap. `chart$data` has one row
#' per point of the series.
#'
#' @param chart The chart being analysed.
#'
#' @return The rows of `chart$data`, in order, with no row repeated.
#' @noRd
restart_rows <- function(chart) {
  return(sort(unique(c(
    forced_restart_rows(chart),
    gap_restarts(chart)$restarts
  ))))
}


#' The rows at which the series is forced to restart
#'
#' Each value of `force_restart_at` restarts the series at the first row whose
#' `x` is at or after it. A value after the last row has no such row, and a
#' value at or before the first row lands on it, where the series starts
#' anyway, so both are ignored.
#'
#' @param chart The chart being analysed.
#'
#' @return The rows of `chart$data`, in order, with no row repeated.
#' @noRd
forced_restart_rows <- function(chart) {
  restarts <- chart$force_restart_at
  x <- chart$data$x

  if (is.null(restarts) || length(x) == 0L) {
    return(integer(0))
  }

  rows <- vapply(
    seq_along(restarts),
    function(i) {
      return(match(TRUE, x >= restarts[i]))
    },
    integer(1L)
  )

  return(sort(unique(rows[!is.na(rows) & rows > 1L])))
}


#' The rows of each segment of the series
#'
#' The series is one segment where there is no restart. Each restart
#' starts a new segment, which runs to the row before the next restart or to the
#' end of the series.
#'
#' @param chart The chart being analysed.
#'
#' @return A list of integer vectors, one per segment, of rows of `chart$data`.
#' @noRd
segment_rows <- function(chart) {
  n_rows <- nrow(chart$data)

  if (n_rows == 0L) {
    return(list(integer(0)))
  }

  starts <- c(1L, restart_rows(chart))
  ends <- c(starts[-1L] - 1L, n_rows)

  return(lapply(seq_along(starts), function(segment) {
    seq(starts[[segment]], ends[[segment]])
  }))
}


#' The prepared series of one segment
#'
#' The segment's rows of `chart$data`. A segment after a restart is analysed as
#' if the series began at its first row, so its rows are passed through
#' `prepare_segment_data()`, which recalculates any `series` value that
#' `prepare_data()` worked out from rows before the restart. The stranded
#' points after a gap have their `series` value removed, so that the analysis
#' skips them, as it skips the points in the gap, which have no value.
#'
#' @param chart The chart being analysed.
#' @param rows The rows of `chart$data` that make up the segment.
#' @param first_segment Whether the segment starts at the first row of the
#'   series.
#'
#' @return The segment's rows of `chart$data`.
#' @noRd
segment_data <- function(chart,
                         rows,
                         first_segment) {
  if (first_segment && length(rows) == nrow(chart$data)) {
    data <- chart$data
  } else {
    data <- dplyr::slice(chart$data, rows)

    if (!first_segment) {
      data <- prepare_segment_data(chart, data = data)
    }
  }

  data$series[rows %in% gap_restarts(chart)$stranded_points] <- NA

  return(data)
}


#' Join the segments' histories into one
#'
#' The segments' `counter_path`, `breaks` and `candidates` are each joined in
#' order, with their row numbers already counted in the whole series.
#' `baseline` is the first segment's and `stopped` the last segment's. Each
#' restart is recorded in `restarts`: the row the new segment starts at, its
#' `type` ("forced", or "after gap"), where the previous segment's counter was
#' when it ended, why the algorithm stopped looking in the previous segment,
#' and the length of the new segment's first calculation period. NA where the
#' previous segment, or the new one, had too few points for a period. The
#' counter's move to each restart row is recorded in `counter_path`. A forced
#' restart at the end of a gap is recorded as forced.
#'
#' @param analysed The segments, as `analyse_segment()` returns them.
#' @param segments The rows of each segment, as `segment_rows()` returns them.
#' @param forced The rows of the forced restarts, as `forced_restart_rows()`
#'   returns them.
#'
#' @return The history of the whole series. With one segment, that segment's
#'   history.
#' @noRd
join_segment_histories <- function(analysed,
                                   segments,
                                   forced) {
  histories <- lapply(analysed, function(segment) segment$chart$history)

  if (length(histories) == 1L) {
    return(histories[[1L]])
  }

  counter_path <- NULL
  breaks <- NULL
  candidates <- list()
  restarts <- NULL

  for (segment in seq_along(histories)) {
    this_segment <- histories[[segment]]

    if (segment > 1L) {
      previous <- histories[[segment - 1L]]$stopped
      restart_row <- as.integer(segments[[segment]][[1L]])
      restart_type <- if (restart_row %in% forced) "forced" else "after gap"
      from <- if (is.null(previous)) NA_integer_ else previous$counter

      # a move to where the counter already is is not recorded, as in
      # record_counter_move()
      if (!isTRUE(from == restart_row)) {
        counter_path <- rbind(
          counter_path,
          data.frame(
            from = as.integer(from),
            to = restart_row,
            reason = if (restart_type == "forced") {
              "forced restart"
            } else {
              "restart after gap"
            }
          )
        )
      }

      restarts <- rbind(
        restarts,
        data.frame(
          row = restart_row,
          type = restart_type,
          counter = as.integer(from),
          previous_segment_ended = if (is.null(previous)) {
            NA_character_
          } else {
            previous$reason
          },
          baseline_length = if (is.null(this_segment$baseline)) {
            NA_integer_
          } else {
            as.integer(this_segment$baseline$length)
          }
        )
      )
    }

    counter_path <- rbind(counter_path, this_segment$counter_path)
    breaks <- rbind(breaks, this_segment$breaks)
    candidates <- c(candidates, this_segment$candidates)
  }

  history <- list()
  history$baseline <- histories[[1L]]$baseline
  history$counter_path <- counter_path
  history$breaks <- breaks

  if (length(candidates) > 0L) {
    history$candidates <- candidates
  }

  history$stopped <- histories[[length(histories)]]$stopped
  history$restarts <- restarts

  return(history)
}


#' Join the segments' analysed tables into one
#'
#' Each segment after a restart has its periods numbered after the
#' observations of the segments before it, so that no two segments share a
#' `plot_period`. Its first observation is a break point where the segment has
#' limits, and its `cl_change` is the direction from the last centre line
#' before it, or 0 where there is none.
#'
#' @param analysed The segments, as `analyse_segment()` returns them.
#' @param segments The rows of each segment, as `segment_rows()` returns them.
#'
#' @return One table, with a row for each row of the series. With one segment,
#'   that segment's table.
#' @noRd
join_segment_tables <- function(analysed,
                                segments) {
  tables <- lapply(analysed, function(segment) segment$chart$result$table)

  if (length(tables) == 1L) {
    return(tables[[1L]])
  }

  # with no limits anywhere the segments hold only the prepared series
  has_limits <- vapply(
    analysed,
    function(segment) segment$has_limits,
    logical(1L)
  )

  if (!any(has_limits)) {
    return(dplyr::bind_rows(tables))
  }

  observed_before <- cumsum(c(
    0L,
    vapply(analysed, function(segment) segment$observed, integer(1L))
  ))

  restart_breaks <- integer(0)

  for (segment in seq_along(tables)[-1L]) {
    if (!analysed[[segment]]$has_limits) {
      next
    }

    segment_chart <- analysed[[segment]]$chart
    first_observed <- match(
      TRUE,
      observed_rows(segment_chart, data = tables[[segment]])
    )

    tables[[segment]] <- tables[[segment]] %>%
      dplyr::mutate(
        period_start = period_start + observed_before[[segment]],
        plot_period = dplyr::if_else(
          is.na(plot_period),
          NA_character_,
          paste0(period_type, period_start)
        ),
        break_point = dplyr::if_else(
          dplyr::row_number() == first_observed,
          TRUE,
          break_point
        ),
        limit_change = dplyr::if_else(
          dplyr::row_number() == first_observed,
          TRUE,
          limit_change
        )
      )

    restart_breaks <- c(
      restart_breaks,
      segments[[segment]][[1L]] - 1L + first_observed
    )
  }

  table <- dplyr::bind_rows(tables) %>%
    dplyr::mutate(earlier_cl = dplyr::lag(cl)) %>%
    tidyr::fill(earlier_cl, .direction = "down") %>%
    dplyr::mutate(
      cl_change = dplyr::if_else(
        dplyr::row_number() %in% restart_breaks,
        dplyr::coalesce(sign(cl - earlier_cl), 0),
        cl_change
      )
    ) %>%
    dplyr::select(-earlier_cl)

  return(table)
}
