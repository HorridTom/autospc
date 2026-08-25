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

    chart <- run_limit_algorithm(chart)

    return(chart)

  })

  return(analysed)

}


#' Join the moving range analysis onto the X analysis
#'
#' An XmR pair is one analysis of one series shown as two charts, so it goes
#' out wide: the moving range and its limits sit beside the X columns as `mr`,
#' `amr`, `url` and `lrl`.
#'
#' @return A data frame.
#' @noRd
join_mr_columns <- function(x_table,
                           mr_table) {

  joined <- x_table %>%
    dplyr::left_join(mr_table %>%
                       dplyr::filter(!is.na(x)) %>%
                       dplyr::select(x,
                                    mr = y,
                                    amr = cl,
                                    url = ucl,
                                    lrl = lcl),
                     by = c("x" = "x")) %>%
    dplyr::select(x, y, cl, ucl, lcl,
                  mr, amr, url, lrl,
                  dplyr::everything())

  return(joined)

}
