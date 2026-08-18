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
