
#' facet_stages
#' @inheritParams autospc
#' @param split_rows A vector of row numbers specifying the stages to display
#' results at. Names specify facet strip labels.
#' @param ... Arguments passed to [autospc::autospc()]
#'
#' @returns An `autospc_plot` showing the results of [autospc::autospc()] at
#' different stages as specified by split_rows, one facet per stage. It is a
#' ggplot, and additionally carries the analysed chart behind each facet.
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

  arguments  <- autospc_argument_values(given)
  chart_args <- arguments[autospc_chart_parameters()]
  passed     <- arguments[autospc_plot_passed_elements()]

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

  check_x_type(df_rn)

  titles   <- titles_from_data(data = data,
                               title = passed$title,
                               subtitle = passed$subtitle)
  title    <- titles$title
  subtitle <- titles$subtitle

  split_rows <- sort(split_rows)

  # Ensure the last split row is the end of the data
  if(split_rows[length(split_rows)] != nrow(data)) {
    split_rows <- c(split_rows,
                    nrow(data))
  }

  data_splits_list <- create_splits_list(df = df_rn,
                                         split_rows = split_rows)

  analyses <- lapply(
    data_splits_list,
    function(split) {

      analysis <- analyse_series(
        data = split,
        chart_type = chart_type,
        x = "x",
        y = "y",
        n = "n",
        chart_args = chart_args,
        passed = passed,
        extend_limits_to = arguments$extend_limits_to
      )

      if(passed$show_limits && !centre_line_present(analysis$data)) {
        warning(paste("The input data has fewer than the minimum number of",
                      "points needed to calculate one period. Timeseries data",
                      "without limits has been displayed."))
      }

      log_output(analysis$chart$result$table,
                 verbosity = arguments$verbosity,
                 chart_type = chart_type,
                 log_file_path = arguments$log_file_path)

      return(analysis)

    }
  )

  results_splits_list <- lapply(
    analyses,
    function(analysis) {

      if(passed$show_limits && centre_line_present(analysis$data)) {
        return(dplyr::filter(analysis$data, !is.na(x)))
      }

      return(analysis$data)

    }
  )

  results_data <- dplyr::bind_rows(
    results_splits_list,
    .id = "stage"
  )

  if(!plot_chart) {
    return(results_data)
  }
  
  # The facets are stages of one series, so the last one holds all of it. Its
  # chart is what postprocess() dispatches y_axis_range() and y_axis_title() on,
  # and the frame it measures is every facet joined.
  chart <- analyses[[length(analyses)]]$chart

  postprocessing_vars <- postprocess(
    df = results_data,
    chart = chart,
    override_x_title = passed$override_x_title,
    override_y_title = passed$override_y_title,
    override_y_lim = passed$override_y_lim,
    x_pad_end = passed$x_pad_end,
    extend_limits_to = arguments$extend_limits_to
  )

  derived <- list(start_x = postprocessing_vars$start_x,
                  x_max = postprocessing_vars$x_max,
                  end_x = postprocessing_vars$end_x,
                  ylimlow = postprocessing_vars$ylimlow,
                  ylimhigh = postprocessing_vars$ylimhigh)

  # The titles the caller did not give have been resolved - the axis ones by
  # postprocess() from the chart's class, and the chart ones from the data.
  # They are what is drawn, so they are what the plot object records.
  passed["title"]            <- list(title)
  passed["subtitle"]         <- list(subtitle)
  passed["override_x_title"] <- list(postprocessing_vars$override_x_title)
  passed["override_y_title"] <- list(postprocessing_vars$override_y_title)

  charts <- lapply(analyses, function(analysis) analysis$chart)

  sp <- create_spc_plot(charts = charts,
                        data = results_data,
                        passed = passed,
                        derived = derived,
                        split_rows = split_rows)

  return(autospc_plot(
    plot = sp,
    charts = charts,
    passed = passed,
    derived = derived
  ))
  
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
#' the other three hold column names rather than values.
#'
#' @param given A named list of the argument values the caller supplied.
#'
#' @return A named list of values, one per argument.
#' @noRd
autospc_argument_values <- function(given) {

  defaults <- formals(autospc)
  defaults <- defaults[setdiff(names(defaults), c("data", "x", "y", "n"))]

  # formals() gives each default as the expression it was written as, not as a
  # value, so a default like getRversion() < "4.3.0" arrives as an unevaluated
  # call. Evaluating in the package namespace is what R itself would do, and is
  # what makes deprecated() - imported from lifecycle - resolvable.
  values <- lapply(defaults,
                   function(default) eval(default,
                                          envir = asNamespace("autospc")))

  values[names(given)] <- given

  return(values)

}
