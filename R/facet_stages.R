#' Plot SPC charts at successive stages of a series
#'
#' `facet_stages()` analyses the same series in stages, each time using more of
#' it, and plots the results side by side - one facet per stage. Each facet is
#' what `autospc()` would have drawn from the data available at that point, so
#' the set of them shows how the chart, and the control limits, developed as the
#' data arrived.
#'
#' @inheritParams autospc
#' @param split_at A vector of positions in the analysed series, which holds
#' one point per subgroup in x order, specifying the stages to display results
#' at. Names specify facet strip labels.
#' @param ... Arguments passed to [autospc::autospc()]
#' @param split_rows `r lifecycle::badge("deprecated")` Use `split_at` instead.
#' The positions it takes are now counted in the analysed series rather than in
#' the data as supplied.
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
#'   split_at = c(30L, 60L, 90L),
#'   chart_type = "C'",
#'   x = month_start,
#'   y = att_all,
#'   x_break = 365
#' )
#'
#' @export
facet_stages <- function(data,
                         split_at,
                         plot_chart = TRUE,
                         ...,
                         split_rows = deprecated()) {
  caller <- parent.frame()

  if (lifecycle::is_present(split_rows)) {
    lifecycle::deprecate_warn(
      when = "0.1.0.9015",
      what = "facet_stages(split_rows)",
      with = "facet_stages(split_at)",
      details = paste(
        "The positions are now counted in the analysed series, which",
        "holds one point per subgroup in x order, rather than in the",
        "data as supplied. Where the data already holds one row per",
        "subgroup, in x order, nothing changes."
      )
    )

    if (missing(split_at)) {
      split_at <- split_rows
    }
  }

  plot_chart <- match_flag(plot_chart, "plot_chart")

  dots_exprs <- rlang::exprs(...)

  if ("show_mr" %in% names(dots_exprs)) {
    if (isTRUE(dots_exprs$show_mr)) {
      warning(paste(
        "`facet_stages()` does not support `show_mr = TRUE`.",
        "The X chart is faceted on its own. To facet an MR chart",
        "by stages use `facet_stages()` with `chart_type = MR`."
      ))
    }

    lifecycle::deprecate_warn(
      when = "0.1.0",
      what = "facet_stages(show_mr)",
      with = "facet_stages(chart_type)",
      details = paste(
        'chart_type = "X" facets the X chart on its own, which',
        "is what facet_stages() has always drawn for",
        'chart_type = "XMR".'
      )
    )

    dots_exprs$show_mr <- NULL
  }

  # facet_stages() has never drawn the moving range chart, so chart_type =
  # "XMR" is faceted as an X chart.
  if (identical(dots_exprs$chart_type, "XMR")) {
    dots_exprs$chart_type <- "X"
  }

  xyn_exprs <- dots_exprs[which(names(dots_exprs) %in% c("x", "y", "n"))]

  # x, y and n name columns and must not be evaluated. Everything else is a
  # value, and takes its default from autospc() where the caller gave none.
  given <- lapply(dots_exprs[which(!names(dots_exprs) %in% c("x", "y", "n"))],
    eval,
    envir = caller
  )

  arguments <- autospc_argument_values(given)

  arguments <- validate_argument_values(arguments)

  arguments <- validate_algorithm_parameters(
    arguments,
    user_env = rlang::caller_env()
  )

  chart_args <- arguments[autospc_chart_parameters()]
  visualisation_params <- arguments[visualisation_param_names()]

  chart_type <- arguments$chart_type

  validate_chart_type(chart_type)

  # Construct one chart from the whole series. It is not analysed: it is
  # constructed for chart$data, which has the columns renamed to x, y and n, has
  # been checked against the column requirements for the chart type, and has any
  # counts rounded. Doing this here means each of those happens once per call
  # rather than once per facet. aggregation_na_rm is the only chart parameter
  # passed, because it is the only one that affects chart$data.
  whole_series <- autospc_chart(
    chart_type = chart_type,
    data = data,
    x = column_name_of(xyn_exprs, field = "x"),
    y = column_name_of(xyn_exprs, field = "y"),
    n = column_name_of(xyn_exprs, field = "n"),
    aggregation_na_rm = arguments$aggregation_na_rm
  )

  check_x_type(whole_series$data$x)

  whole_series$data <- drop_missing_x(whole_series$data, x_column = "x")

  # The split points count points in the analysed series, so the series is
  # aggregated and put in x order before it is split.
  whole_series <- order_series(aggregate_data(whole_series))

  df_rn <- whole_series$data

  # Resolved once for the call, from the chart of the whole series.
  visualisation_params <- resolve_default_visualisation_params(
    visualisation_params = visualisation_params,
    chart = whole_series
  )

  split_at <- normalise_split_at(
    split_at = split_at,
    n_points = nrow(df_rn)
  )

  data_splits_list <- create_splits_list(
    data = df_rn,
    split_at = split_at
  )

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
        !!!chart_args
      )

      return(analyse_charts(facet)[[1]])
    }
  )

  # The facets take their names from split_at where it has them, and their
  # positions where it does not.
  stage_names <- names(charts)

  if (is.null(stage_names)) {
    stage_names <- as.character(seq_along(charts))
  }

  # A facet is named for its stage rather than for its chart type
  report_analysis(
    charts = charts,
    show_limits = visualisation_params$show_limits,
    verbosity = arguments$verbosity,
    log_file_path = arguments$log_file_path,
    labels = stage_names,
    short_message = stages_short_message
  )

  if (!plot_chart) {
    return(charts_as_table(
      charts = charts,
      faceted = TRUE
    ))
  }

  return(autospc_plot(
    charts = charts,
    visualisation_params = visualisation_params,
    faceted = TRUE
  ))
}


#' The split points a call is run with
#'
#' Sorted, with any value beyond the end of the analysed series taken as its
#' last point, duplicates removed, and the last point added if it is not
#' already there. Facet names are kept.
#'
#' @param split_at The `split_at` the caller gave.
#' @param n_points The number of points in the analysed series.
#'
#' @return An integer vector of positions in the analysed series.
#' @noRd
normalise_split_at <- function(split_at,
                               n_points) {
  beyond <- split_at > n_points

  if (any(beyond)) {
    warning(paste0(
      "split_at values beyond the end of the analysed series (",
      paste(unique(split_at[beyond]), collapse = ", "),
      ") have been taken as its last point, ", n_points, "."
    ))
  }

  split_at <- sort(pmin(split_at, n_points))
  split_at <- split_at[!duplicated(split_at)]

  if (split_at[length(split_at)] != n_points) {
    split_at <- c(split_at, n_points)
  }

  return(split_at)
}


#' The series as it stood at each split point
#'
#' The stages are cumulative rather than a partition: each one is the series
#' from its start up to a split point. A split at 12 of 25 points therefore
#' gives a stage of 12 points and a stage of 25, 37 in all, and not stages of
#' 12 and 13.
#'
#' @param data The analysed series, aggregated and in x order.
#' @param split_at The normalised split points.
#'
#' @return A list of data frames, one per stage.
#' @noRd
create_splits_list <- function(data,
                               split_at) {
  if (is.null(split_at)) {
    data_splits <- list(data)
  } else {
    data_splits <- lapply(
      split_at,
      function(x) {
        data[1:x, ]
      }
    )
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
  names_wanted <- setdiff(
    names(formals(autospc)),
    c(
      "data", "x", "y", "n",
      autospc_deprecated_arguments()
    )
  )

  values <- lapply(names_wanted, autospc_default)
  names(values) <- names_wanted

  values[names(given)] <- given

  return(values)
}
