# Taking constructed charts through to analysed charts

#' Analyse each chart
#'
#' Aggregate the series, order it, prepare it, and run the limit algorithm over
#' it, for each chart in turn.
#'
#' @param charts A list of `autospc_chart` objects, as `build_charts()` gives
#'   them.
#'
#' @return A list of `autospc_chart` objects, each with `chart$result` set.
#' @noRd
analyse_charts <- function(charts) {
  analysed <- lapply(charts, function(chart) {
    chart <- aggregate_data(chart)
    chart <- order_series(chart)
    chart <- prepare_data(chart)

    chart <- establish_limits(chart)

    return(chart)
  })

  return(analysed)
}


#' Order a chart's series by x, and make it a plain data frame
#'
#' The algorithm walks the data in row order, so the rows have to be in x order
#' before it runs, and before `prepare_data()` derives anything from their
#' order - an MR chart's moving ranges are differences between neighbouring
#' rows.
#'
#' `dplyr::arrange()` is stable, so rows sharing an x keep the order they
#' arrived in. Missing x values sort to the end.
#'
#' This is also where `data` becomes a plain data frame, and it is the only
#' place that does it. Every chart type passes through here, and it is after
#' `aggregate_data()`, which is what produces a tibble: `dplyr::summarise()`
#' returns one whatever it was given. Everything the algorithm derives from
#' `data` is therefore a plain data frame as well - the limits table, the
#' analysis in `chart$result$table`, and the tables recorded in
#' `chart$history`. `data_original` is left as the caller passed it.
#'
#' @return autospc_chart object of the same class as chart
#' @noRd
order_series <- function(chart) {
  chart$data <- chart$data %>%
    dplyr::arrange(x) %>%
    as.data.frame()

  return(chart)
}
