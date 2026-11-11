# Functions that record what the algorithm did, for chart$history

#' Record a move of the counter in the chart's history
#'
#' A move to where the counter already is is not recorded.
#'
#' @return autospc_chart object
#' @noRd
record_counter_move <- function(chart,
                                from,
                                to,
                                reason) {
  # the counter can already be at the destination
  if (isTRUE(from == to)) {
    return(chart)
  }

  chart$history$counter_path <- rbind(
    chart$history$counter_path,
    data.frame(
      from = as.integer(from),
      to = as.integer(to),
      reason = reason
    )
  )

  return(chart)
}


#' Record why the algorithm stopped looking for further periods
#'
#' @return autospc_chart object
#' @noRd
record_stop <- function(chart,
                        counter,
                        reason) {
  chart$history$stopped <- list(
    counter = as.integer(counter),
    reason = reason
  )

  return(chart)
}


#' Record a triggering shift rule break the algorithm identified
#'
#' The break is against the prevailing limits, which are the ones in
#' `limits_table` at the time. The `cl`, `ucl` and `lcl` recorded are those in
#' force at `position`. A candidate's opposing break is a different thing, is
#' against that candidate's own limits, and is recorded on the candidate.
#'
#' @param already_at_break was the counter already inside this break
#' @return autospc_chart object
#' @noRd
record_break <- function(chart,
                         counter,
                         position,
                         already_at_break,
                         limits_table) {
  position <- as.integer(position)

  chart$history$breaks <- rbind(
    chart$history$breaks,
    data.frame(
      counter = as.integer(counter),
      position = position,
      direction = if (is.na(position)) {
        NA_integer_
      } else {
        limits_table$above_or_below_cl[position]
      },
      already_at_break = already_at_break,
      cl = if (is.na(position)) NA_real_ else limits_table$cl[position],
      ucl = if (is.na(position)) NA_real_ else limits_table$ucl[position],
      lcl = if (is.na(position)) NA_real_ else limits_table$lcl[position]
    )
  )

  return(chart)
}


#' The limits in force before a candidate period
#'
#' The last row before the candidate's counter, and the centre line and limits
#' it holds. NA where the candidate starts at the first row.
#'
#' @return A list of `last_row`, `cl`, `ucl` and `lcl`.
#' @noRd
prevailing_limits <- function(limits_table,
                              counter) {
  last_row <- if (counter > 1L) {
    as.integer(counter) - 1L
  } else {
    NA_integer_
  }

  return(list(
    last_row = last_row,
    cl = limits_table$cl[last_row],
    ucl = limits_table$ucl[last_row],
    lcl = limits_table$lcl[last_row]
  ))
}


#' Record a candidate that was not formed because it starts in a prevented
#' range
#'
#' Recorded among the candidates, so that each shift rule break the algorithm
#' moved to is followed by a candidate. Nothing about the candidate was
#' evaluated, so `period_rows` is empty, `table` is NULL, and
#' `opposite_break` and `final_run_prevents` are NA. `prevented` is TRUE, and
#' is present only on such a candidate.
#'
#' @param counter Where the candidate would have started.
#' @param trigger_direction The direction of the shift rule break there.
#' @param limits_table The limits table at the time.
#'
#' @return autospc_chart object
#' @noRd
record_prevented_candidate <- function(chart,
                                       counter,
                                       trigger_direction,
                                       limits_table) {
  chart$history$candidates <- c(
    chart$history$candidates,
    list(list(
      counter = as.integer(counter),
      period_rows = integer(0),
      trigger_direction = trigger_direction,
      table = NULL,
      prevailing = prevailing_limits(limits_table, counter = counter),
      opposite_break = NA,
      final_run_prevents = NA,
      accepted = FALSE,
      prevented = TRUE
    ))
  )

  return(chart)
}
