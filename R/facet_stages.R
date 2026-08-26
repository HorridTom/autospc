
#' Plot SPC charts at successive stages of a series
#'
#' `facet_stages()` analyses the same series in stages, each time using more of
#' it, and plots the results side by side - one facet per stage. Each facet is
#' what `autospc()` would have drawn from the data available at that point, so
#' the set of them shows how the chart, and the control limits, developed as the
#' data arrived.
#'
#' @inheritParams autospc
#' @param split_rows A vector of row numbers specifying the stages to display
#' results at. Names specify facet strip labels.
#' @param ... Arguments passed to [autospc::autospc()]
#'
#' @returns With `plot_chart = TRUE` (the default), an `autospc_plot`: one
#' ggplot, faceted by stage, which also carries the analysed chart behind each
#' facet and the parameters it was drawn with.
#'
#' With `plot_chart = FALSE`, a data frame holding every stage, with `stage`
#' saying which each row belongs to.
#'
#' @examples
#' # Show progression of C' chart for count of monthly attendances over time
#' facet_stages(
#'   ed_attendances_monthly,
#'   split_rows = c(30L, 60L, 90L),
#'   chart_type = "C'",
#'   x = month_start,
#'   y = att_all, 
#'   x_break = 365
#' )
#' 
#' @export  
facet_stages <- function(data,
                         split_rows,
                         plot_chart = TRUE,
                         ...) {
  
  caller <- parent.frame()

  dots_exprs <- rlang::exprs(...)

  if("show_mr" %in% names(dots_exprs)) {

    if(isTRUE(dots_exprs$show_mr)) {
      warning(paste("`facet_stages()` does not support `show_mr = TRUE`.",
                    "The X chart is faceted on its own. To facet an MR chart",
                    "by stages use `facet_stages()` with `chart_type = MR`."))
    }

    lifecycle::deprecate_warn(
      when = "0.0.0.9051",
      what = "facet_stages(show_mr)",
      with = "facet_stages(chart_type)",
      details = paste('chart_type = "X" facets the X chart on its own, which',
                      'is what facet_stages() has always drawn for',
                      'chart_type = "XMR".')
    )

    dots_exprs$show_mr <- NULL

  }

  # facet_stages() has never drawn the moving range chart, so chart_type =
  # "XMR" is faceted as an X chart.
  if(identical(dots_exprs$chart_type, "XMR")) {
    dots_exprs$chart_type <- "X"
  }

  xyn_exprs <- dots_exprs[which(names(dots_exprs) %in% c("x", "y", "n"))]

  # x, y and n name columns and must not be evaluated. Everything else is a
  # value, and takes its default from autospc() where the caller gave none.
  given <- lapply(dots_exprs[which(!names(dots_exprs) %in% c("x", "y", "n"))],
                  eval,
                  envir = caller)

  arguments <- autospc_argument_values(given)

  arguments <- validate_algorithm_parameters(arguments)

  chart_args <- arguments[autospc_chart_parameters()]
  visualisation_params <- arguments[visualisation_param_names()]

  chart_type <- arguments$chart_type

  validate_chart_type(chart_type)

  # Construct one chart from the whole series. It is not analysed: it is
  # constructed for chart$data, which has the columns renamed to x, y and n, has
  # been checked against the column requirements for the chart type, and has any
  # counts rounded. Doing this here means each of those happens once per call
  # rather than once per facet. The chart parameters are not passed because none
  # of them affects chart$data.
  whole_series <- autospc_chart(chart_type = chart_type,
                                data = data,
                                x = column_name_of(xyn_exprs, "x"),
                                y = column_name_of(xyn_exprs, "y"),
                                n = column_name_of(xyn_exprs, "n"))

  df_rn <- whole_series$data

  check_x_type(df_rn$x)

  # Resolved once for the call, from the chart of the whole series.
  visualisation_params <- resolve_default_visualisation_params(
    visualisation_params = visualisation_params,
    chart = whole_series
  )

  split_rows <- sort(split_rows)

  # Ensure the last split row is the end of the data
  if(split_rows[length(split_rows)] != nrow(data)) {
    split_rows <- c(split_rows,
                    nrow(data))
  }

  data_splits_list <- create_splits_list(df = df_rn,
                                         split_rows = split_rows)

  charts <- lapply(
    data_splits_list,
    function(split) {

      # The split came from the chart of the whole series, so its columns are
      # already named x, y and n.
      facet <- rlang::exec(build_charts,
                           chart_type = chart_type,
                           data = split,
                           x = "x",
                           y = "y",
                           n = "n",
                           !!!chart_args)

      return(analyse_charts(facet)[[1]])

    }
  )

  # The facets take their names from split_rows where it has them, and their
  # positions where it does not.
  stage_names <- names(charts)

  if(is.null(stage_names)) {
    stage_names <- as.character(seq_along(charts))
  }

  # A facet is named for its stage rather than for its chart type
  report_analysis(charts = charts,
                  show_limits = visualisation_params$show_limits,
                  verbosity = arguments$verbosity,
                  log_file_path = arguments$log_file_path,
                  labels = stage_names,
                  short_message = stages_short_message)

  if(!plot_chart) {
    return(charts_as_table(charts = charts,
                           visualisation_params = visualisation_params))
  }

  return(autospc_plot(charts = charts,
                      visualisation_params = visualisation_params,
                      split_rows = split_rows))

}


create_splits_list <- function(df,
                               split_rows) {
  
  if(is.null(split_rows)) {
    
    data_splits <- list(df)
    
  } else {
    
    data_splits <- lapply(split_rows,
                          function(x) {
                            df[1:x,]
                          })
  }
  
  return(data_splits)
  
}


#' The value each autospc() argument takes for one call
#'
#' One element per `autospc()` argument: the value the caller gave it, or the
#' default from `autospc()`'s signature where the caller gave none.
#'
#' `data`, `x`, `y` and `n` are not among them. `data` is the data itself, and
#' the other three hold column names rather than values. The deprecated
#' arguments are not among them either: their default is a sentinel rather than
#' a value, and `facet_stages()` deals with the one it supports before this is
#' called.
#'
#' @param given A named list of the argument values the caller supplied.
#'
#' @return A named list of values, one per argument.
#' @noRd
autospc_argument_values <- function(given) {

  names_wanted <- setdiff(names(formals(autospc)),
                          c("data", "x", "y", "n",
                            autospc_deprecated_arguments()))

  values <- lapply(names_wanted, autospc_default)
  names(values) <- names_wanted

  values[names(given)] <- given

  return(values)

}
