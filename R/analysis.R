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


#' The columns a chart's analysis table holds, in order
#'
#' The contract `autospc(plot_chart = FALSE)` returns: for a given chart type,
#' the same columns whatever the data and whatever the other arguments. Both
#' paths through `establish_limits()` finish against this list.
#'
#' `median` is among them, and holds no value where no floating median was
#' drawn.
#'
#' @param chart The chart being analysed.
#'
#' @return A character vector of column names.
#' @noRd
analysis_table_columns <- function(chart) {
  return(c(
    "x",
    "series",
    limits_table_columns(chart),
    "ucl",
    "lcl",
    "cl",
    period_statistics_columns(chart),
    "period_type",
    "excluded",
    "break_point",
    "rule1",
    "run_start",
    "rule2",
    "above_or_below_cl",
    "highlight",
    "limit_change",
    "period_start",
    "plot_period",
    "cl_change",
    "log",
    "limit_extension",
    "median"
  ))
}


#' The type each analysed column holds
#'
#' A series too short to form a period is given the columns a full analysis
#' would have filled, holding no value. The type is declared because a column
#' of missing values is a different column if it is of a different type.
#'
#' The columns not listed here - `x`, `series`, the data columns and `log` -
#' are on the table by the time either path finishes, so their type comes from
#' the data or from the code that made them.
#'
#' @return A named list, one zero-length vector per column.
#' @noRd
analysis_column_types <- function() {
  return(list(
    ucl = numeric(0),
    lcl = numeric(0),
    cl = numeric(0),
    sd_estimate = numeric(0),
    sbar = numeric(0),
    period_type = character(0),
    excluded = logical(0),
    break_point = logical(0),
    rule1 = logical(0),
    run_start = logical(0),
    rule2 = logical(0),
    above_or_below_cl = integer(0),
    highlight = character(0),
    limit_change = logical(0),
    period_start = integer(0),
    plot_period = character(0),
    cl_change = numeric(0),
    limit_extension = logical(0),
    median = numeric(0)
  ))
}


#' Give a table the analysed columns it has not got, holding no value
#'
#' What a series too short to form a period returns.
#'
#' `limit_extension` is set rather than left missing. It says whether the
#' extension put the row there, and on this path no rows were added, so FALSE
#' is what it is, not what is unknown.
#'
#' @param table The prepared data, with the log on it.
#' @param chart The chart being analysed.
#'
#' @return `table`, holding every column of `analysis_table_columns(chart)`.
#'   `finish_analysis_table()` puts them in order.
#' @noRd
fill_analysis_columns <- function(table,
                                  chart) {
  types <- analysis_column_types()

  for (column in setdiff(analysis_table_columns(chart), names(table))) {
    table[[column]] <- rep(types[[column]][NA_integer_], nrow(table))
  }

  table$limit_extension <- rep(FALSE, nrow(table))

  return(table)
}


#' Finish a chart's analysis table
#'
#' What both paths through `establish_limits()` end with: the floating median,
#' and the columns in the order `analysis_table_columns()` declares.
#'
#' @param chart The chart being analysed, with `chart$result$table` set.
#'
#' @return `chart`, with the median drawn where there is one and the columns in
#'   the declared order.
#' @noRd
finish_analysis_table <- function(chart) {
  chart$result$table <- floating_median_column(
    table = chart$result$table,
    floating_median = chart$floating_median,
    floating_median_n = chart$floating_median_n
  )

  chart$result$table <- chart$result$table %>%
    dplyr::select(
      dplyr::all_of(analysis_table_columns(chart)),
      dplyr::everything()
    )

  return(chart)
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
