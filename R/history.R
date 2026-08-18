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
  if(isTRUE(from == to)) {
    return(chart)
  }

  chart$history$counter_path <- rbind(
    chart$history$counter_path,
    data.frame(from = as.integer(from),
               to = as.integer(to),
               reason = reason))

  return(chart)

}


#' Record why the algorithm stopped looking for further periods
#'
#' @return autospc_chart object
#' @noRd
record_stop <- function(chart,
                        counter,
                        reason) {

  chart$history$stopped <- list(counter = as.integer(counter),
                                reason = reason)

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
    data.frame(counter = as.integer(counter),
               position = position,
               direction = if(is.na(position)) NA_integer_ else
                 limits_table$aboveOrBelowCl[position],
               already_at_break = already_at_break,
               cl = if(is.na(position)) NA_real_ else limits_table$cl[position],
               ucl = if(is.na(position)) NA_real_ else limits_table$ucl[position],
               lcl = if(is.na(position)) NA_real_ else limits_table$lcl[position]))

  return(chart)

}
