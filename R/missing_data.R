# Everything the package decides about missing values.


#' Whether to warn about rows the package had to exclude due to missing x
#'
#' Read from the option rather than from an argument of `autospc()` so that a
#' caller drawing many charts can set it once for the session. Any value other
#' than FALSE warns, so that a mis-typed option does not silence the warning.
#'
#' @return TRUE or FALSE
#' @noRd
warn_missing_x_enabled <- function() {
  return(!isFALSE(getOption("autospc.warn_missing_x")))
}


#' Drop the rows whose x is missing
#'
#' A row with no x cannot be placed on the horizontal axis, so it cannot be
#' plotted and cannot be put in order for the algorithm to walk. There is
#' nothing the package can do with it but leave it out.
#'
#' This runs once per call to `autospc()` or `facet_stages()`, before any chart
#' is built, so that everything downstream is working with a series that has an
#' x for every row.
#'
#' The column may be absent, where the caller named one the data does not hold.
#' The class validator reports that; nothing is dropped here.
#'
#' @param data The data frame the caller supplied.
#' @param x_column Character scalar naming the column used as x.
#'
#' @return `data`, without the rows whose x is missing.
#' @noRd
drop_missing_x <- function(data,
                           x_column) {
  if (!x_column %in% colnames(data)) {
    return(data)
  }

  missing_x <- is.na(data[[x_column]])
  dropped <- sum(missing_x)

  if (dropped == 0L) {
    return(data)
  }

  if (warn_missing_x_enabled()) {
    rlang::warn(
      sprintf(
        paste(
          "%d %s excluded because %s no %s value.",
          "Set options(autospc.warn_missing_x = FALSE) to stop this warning."
        ),
        dropped,
        if (dropped == 1L) "row was" else "rows were",
        if (dropped == 1L) "it has" else "they have",
        x_column
      ),
      class = "autospc_missing_x_warning"
    )
  }

  return(data[!missing_x, , drop = FALSE])
}


#' Reduce a prepared series to the points that were observed
#'
#' The algorithm walks the series row by row, so the rows it walks are the
#' observations and nothing else. `run_break` marks a point that a gap
#' immediately precedes, and is what stops a run continuing across that gap.
#'
#' @param data A prepared series, with a `y` column.
#' @param na_ends_run Whether a gap starts a new run.
#'
#' @return The rows of `data` that hold an observation, with `run_break` added.
#' @noRd
compact_series <- function(chart,
                           data,
                           na_ends_run) {
  observed <- observed_rows(chart, data = data)
  compacted <- data[observed, , drop = FALSE]

  if (nrow(compacted) == 0L) {
    compacted$run_break <- logical(0)

    return(compacted)
  }

  positions <- which(observed)
  after_gap <- c(FALSE, diff(positions) > 1L)

  compacted$run_break <- after_gap & na_ends_run

  return(compacted)
}


#' Put the rows that hold no observation back into the analysed table
#'
#' The analysed table has one row per observation. This returns it to one row
#' per row of the prepared series, with the limits carried across a gap between
#' two observations and left missing beyond the first and last of them.
#'
#' `extrapolate_limits()` gives the limits to carry across. For a chart whose
#' limits are constant within a period it returns those limits; for one whose
#' limits vary with the denominator it recalculates them from the period's mean
#' denominator, as it does for limits extended beyond the end of the data.
#'
#' @param limits_table The analysed table, one row per observation.
#' @param data The prepared series, one row per row.
#' @param chart The chart being analysed.
#'
#' @return A data frame with one row per row of `data`.
#' @noRd
restore_missing_rows <- function(limits_table,
                                 data,
                                 chart) {
  observed <- observed_rows(chart, data = data)

  if (all(observed)) {
    return(limits_table)
  }

  restored <- limits_table[rep(NA_integer_, nrow(data)), , drop = FALSE]
  restored[observed, ] <- limits_table
  rownames(restored) <- NULL

  restored$x <- data$x
  restored$y <- data$y

  restored <- carry_limits_across_gaps(
    restored = restored,
    observed = observed,
    chart = chart
  )

  return(restored)
}


#' Give the rows between two observations the limits of their period
#'
#' Rows before the first observation and after the last are left as they are,
#' with no limits.
#'
#' @return `restored`, with `cl`, `ucl` and `lcl` set on the rows in a gap.
#' @noRd
carry_limits_across_gaps <- function(restored,
                                     observed,
                                     chart) {
  positions <- which(observed)
  inside <- seq_len(nrow(restored)) > min(positions) &
    seq_len(nrow(restored)) < max(positions)

  gaps <- which(!observed & inside)

  if (length(gaps) == 0L) {
    return(restored)
  }

  # each gap takes the period of the observation before it
  period_of <- restored$plot_period
  period_of[gaps] <- period_of[positions[findInterval(gaps, positions)]]

  for (period in unique(period_of[gaps])) {
    period_limits <- extrapolate_limits(
      chart = chart,
      period = restored[observed & period_of == period, , drop = FALSE]
    )

    rows <- gaps[period_of[gaps] == period]

    restored$cl[rows] <- period_limits$cl
    restored$ucl[rows] <- period_limits$ucl
    restored$lcl[rows] <- period_limits$lcl
  }

  return(restored)
}


#' Convert the row numbers in a chart's history to rows of the prepared series
#'
#' The algorithm is given only the rows that hold an observation, so every row
#' number it records is a position within those rows. The history is read
#' against the prepared series - the one `prepare_data()` produced, with a row
#' for every subgroup whether or not it holds an observation - so those numbers
#' have to be converted before anything reads them. `render_log()` is the main
#' reader: it places each log entry at the row the history names.
#'
#' `candidates$table` is not converted. It is a limits table rather than a set
#' of row numbers, it is only kept when `keep_candidate_tables` is TRUE, and
#' nothing reads row numbers from it.
#'
#' @param history `chart$history`.
#' @param positions The rows of the prepared series that hold an observation,
#'   so that `positions[i]` is where the algorithm's row `i` sits.
#' @param n_rows The number of rows in the prepared series.
#'
#' @return `history`, with its row numbers converted.
#' @noRd
restate_history_rows <- function(history,
                                 positions,
                                 n_rows) {
  convert <- function(rows) {
    converted <- positions[rows]

    # the counter comes to rest one place past the last observation when it
    # reaches the end of the series. There is no such observation to look up,
    # so it is put the same distance past the end of the prepared series, and
    # render_log() holds it against the last row as it did before
    beyond <- !is.na(rows) & rows > length(positions)
    converted[beyond] <- n_rows + rows[beyond] - length(positions)

    return(converted)
  }

  # where the counter moved from and to, at each move
  if (!is.null(history$counter_path)) {
    history$counter_path$from <- convert(history$counter_path$from)
    history$counter_path$to <- convert(history$counter_path$to)
  }

  # where the counter was when the algorithm stopped
  if (!is.null(history$stopped)) {
    history$stopped$counter <- convert(history$stopped$counter)
  }

  # where the counter was when each rule break was considered, and where the
  # break itself starts
  if (!is.null(history$breaks)) {
    history$breaks$counter <- convert(history$breaks$counter)
    history$breaks$position <- convert(history$breaks$position)
  }

  # for each candidate period: where the counter was, which rows the period
  # covers, and the last row the prevailing limits applied to
  history$candidates <- lapply(history$candidates, function(candidate) {
    candidate$counter <- convert(candidate$counter)
    candidate$period_rows <- convert(candidate$period_rows)
    candidate$prevailing$last_row <- convert(candidate$prevailing$last_row)

    return(candidate)
  })

  return(history)
}


#' Sum observations over subgroups, discarding those with no value
#'
#' What `aggregation_na_rm = TRUE` does. A row is discarded when any of the
#' columns being summed has no value, so that a subgroup's numerator and
#' denominator always count the same observations.
#'
#' A subgroup all of whose observations are discarded stays in the series with
#' no value, rather than leaving it. There was a subgroup there; nothing is
#' known about it.
#'
#' @param data The data to aggregate, with an `x` column.
#' @param columns The columns to sum, as a character vector.
#'
#' @return A data frame of one row per value of `x`.
#' @noRd
sum_over_subgroups_dropping_missing <- function(data,
                                                columns) {
  complete <- data %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(columns), ~ !is.na(.x)))

  totals <- complete %>%
    dplyr::group_by(x) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(columns), sum))

  every_subgroup <- data %>%
    dplyr::distinct(x)

  return(dplyr::left_join(every_subgroup, totals, by = "x"))
}


#' Sum observations over subgroups
#'
#' `aggregation_na_rm` decides what an observation with no value does to the
#' subgroup it belongs to.
#'
#' @param data The data to aggregate, with an `x` column.
#' @param columns The columns to sum, as a character vector.
#' @param aggregation_na_rm Whether to discard an observation with no value.
#'
#' @return A data frame of one row per value of `x`.
#' @noRd
sum_over_subgroups <- function(data,
                               columns,
                               aggregation_na_rm) {
  if (aggregation_na_rm) {
    return(sum_over_subgroups_dropping_missing(data, columns = columns))
  }

  totals <- data %>%
    dplyr::group_by(x) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(columns), sum))

  return(totals)
}
