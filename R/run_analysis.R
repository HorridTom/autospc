# Taking a series through to an analysed chart
#
# analyse_series() is the whole path for one series. run_analysis() is the path
# for one chart, called once per chart the chart type asks for.
# join_mr_columns() puts the two halves of a pair back together for output.

#' Analyse a series as the chart type asks for
#'
#' Everything between the arguments a caller supplied and an analysed chart:
#' validate the chart type, build the chart or charts, preprocess the inputs,
#' resolve the presentation defaults that depend on the chart, and run the
#' analysis on the first chart.
#'
#' **Only the first chart is analysed.** The moving range half of an XmR pair is
#' analysed by the caller, once it knows the first produced limits - which is
#' also what orders the warning and the log output for each half.
#'
#' `passed` goes in as the caller gave it and comes back with `title`,
#' `subtitle` and the two annotation scale factors resolved. The axis titles are
#' not among them: they are per chart, so they come back under `axis_titles`.
#'
#' @param chart_args A named list of the chart parameters, as `autospc_chart()`
#'   takes them.
#' @param passed A named list of the presentation parameters.
#'
#' @return A list of the built `charts`; the analysed first `chart` with its
#'   drawable `data`, `derived` axis extents and `axis_titles`; the resolved
#'   `passed`; and `chart_type` and `xType`.
#' @noRd
analyse_series <- function(data,
                           chart_type,
                           x,
                           y,
                           n,
                           chart_args,
                           passed,
                           extend_limits_to,
                           floating_median,
                           floating_median_n) {

  # autospc_chart() has no branch for a chart type outside
  # autospc_chart_types(), so chart_type has to be valid before the object is
  # built. preprocess_inputs() checks it again.
  validate_chart_type(chart_type)

  # The charts are built from the data exactly as passed. build_charts() renames
  # the analysed columns to x, y and n.
  charts_list <- rlang::exec(build_charts,
                            chart_type = chart_type,
                            data = data,
                            x = x,
                            y = y,
                            n = n,
                            !!!chart_args)

  chart <- charts_list[[1]]

  preprocessed_vars <- preprocess_inputs(
    df = chart$data,
    chart_type = chart_type,
    title = passed$title,
    subtitle = passed$subtitle
  )

  chart$data      <- preprocessed_vars$df
  chart_type      <- preprocessed_vars$chart_type
  passed$title    <- preprocessed_vars$title
  passed$subtitle <- preprocessed_vars$subtitle
  xType           <- preprocessed_vars$xType

  # Centre line labels sit a scale factor above the upper control limit, and
  # the lower factor is its mirror image about 1. Only the upper default asks
  # what kind of chart this is, so only that is a method. A value the caller
  # passed wins over both.
  if(is.null(passed$upper_annotation_sf)) {
    passed$upper_annotation_sf <- upper_annotation_sf_default(chart)
  }

  if(is.null(passed$lower_annotation_sf)) {
    passed$lower_annotation_sf <- 2 - passed$upper_annotation_sf
  }

  analysis <- run_analysis(chart = chart,
                           chart_type = chart_type,
                           xType = xType,
                           passed = passed,
                           extend_limits_to = extend_limits_to,
                           floating_median = floating_median,
                           floating_median_n = floating_median_n)

  return(list(charts = charts_list,
              chart = analysis$chart,
              data = analysis$data,
              derived = analysis$derived,
              axis_titles = analysis$axis_titles,
              passed = passed,
              chart_type = chart_type,
              xType = xType))

}


#' Analyse a chart and prepare its data for drawing
#'
#' Everything between a built chart and a drawable frame: aggregate, order,
#' prepare, run the algorithm, then postprocess. Called once per chart, so an
#' XmR pair calls it twice.
#'
#' The axis titles come back separately rather than written into `passed`,
#' because each chart resolves its own from its class - the moving range half
#' of a pair is labelled MR where the X half is labelled X.
#'
#' @return A list of the analysed `chart`, the drawable `data`, the `derived`
#'   axis extents, and the resolved `axis_titles`.
#' @noRd
run_analysis <- function(chart,
                           chart_type,
                           xType,
                           passed,
                           extend_limits_to,
                           floating_median,
                           floating_median_n) {

  chart <- aggregate_data(chart)
  chart <- order_series(chart)
  chart <- prepare_data(chart)

  chart <- run_limit_algorithm(chart)

  data <- chart$result$table

  postprocessing_vars <- postprocess(
    df = data,
    chart = chart,
    override_x_title = passed$override_x_title,
    override_y_title = passed$override_y_title,
    override_y_lim = passed$override_y_lim,
    x_pad_end = passed$x_pad_end,
    extend_limits_to = extend_limits_to,
    xType = xType
  )

  data <- postprocessing_vars$df

  axis_titles <- list(x = postprocessing_vars$override_x_title,
                      y = postprocessing_vars$override_y_title)

  derived <- list(
    start_x = postprocessing_vars$start_x,
    x_max = postprocessing_vars$x_max,
    end_x = postprocessing_vars$end_x,
    ylimlow = postprocessing_vars$ylimlow,
    ylimhigh = postprocessing_vars$ylimhigh
  )

  if(passed$show_limits && centre_line_present(data)) {

    data <- postprocess_spc(
      df = data,
      chart_type = chart_type,
      chart = chart,
      highlight_exclusions = passed$highlight_exclusions,
      floating_median = floating_median,
      floating_median_n = floating_median_n,
      extend_limits_to = extend_limits_to,
      align_labels = passed$align_labels,
      flip_labels = passed$flip_labels,
      upper_annotation_sf = passed$upper_annotation_sf,
      lower_annotation_sf = passed$lower_annotation_sf,
      annotation_arrow_curve = passed$annotation_arrow_curve,
      ylimhigh = derived$ylimhigh,
      x_max = derived$x_max
    )

  }

  return(list(chart = chart,
              data = data,
              derived = derived,
              axis_titles = axis_titles))

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
