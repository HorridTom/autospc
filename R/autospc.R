#' Plot SPC charts with automated limit recalculation
#' 
#' `autospc()` creates a statistical process control chart from a
#' data frame. Control limits are re-established according to specified rules,
#' by default the Stable Shift Algorithm.
#' 
#' @param data A data frame. For column requirements by chart type, see
#' \code{vignette("data-requirements", package = "autospc")}.
#' @param x Name of column (passed using tidyselect semantics) to use as
#' subgroups on the horizontal axis of the chart.
#' @param y Name of column (passed using tidyselect semantics) to use as:
#' \itemize{
#'  \item the variable to be plotted for XMR charts,
#'  \item count (plotted on the vertical axis) for C and C' charts,
#'  \item numerator of the proportion (plotted on the vertical axis) for P and
#'  P' charts.
#'  }
#'  See \code{vignette("data-requirements", package = "autospc")} for more
#'  details.
#' @param n Name of column (passed using tidyselect semantics) to use as
#' denominator for P and P' charts.
#' \cr
#' See \code{vignette("data-requirements", package = "autospc")} for more
#' details.
#' @param chart_type The type of chart you wish to plot. Must must have length
#' one. Available options are: "XMR", "X", "MR", "C", "C'", "P", "P'".
#' 
#' ## Algorithm Parameters
#' Parameters that control behaviour of the algorithm used to re-establish
#' control limits.
#' @param period_min The minimum number of points (subgroups) per period,
#' i.e. the minimum number of points required to form control limits. 
#' @param baseline_length Integer, the number of points used to form the first
#' calculation period. Defaults to period_min. Where the series has fewer points
#' than this, all of them are used. period_min remains the minimum number of
#' points needed to form limits.
#' @param shift_rule_threshold The minimum number of consecutive points above or
#' below the centre line constituting a shift (or "rule 2") break.
#' @param baseline_only Boolean - if TRUE, do not re-establish control limits,
#' instead extend the limits calculated from the first calculation period.
#' @param establish_every_shift Boolean - whether to bypass the Stable Shift
#' Algorithm and simply re-establish limits at every shift rule break
#' (respecting period_min)
#' @param no_regrets Boolean signifying which version of the algorithm should be
#' used. Defines whether limits can change as more data is added or not.
#' @param overhanging_reversions Boolean determining whether rule breaks in the
#' opposite direction to a rule break triggering a candidate recalculation
#' prevent recalculation even if they overhang the end of the candidate
#' calculation period. Set to FALSE only with no_regrets = FALSE.
#' 
#' ## SPC Parameters
#' Parameters that control how centre line and control limits are established
#' for each period, and details of how SPC rules are applied
#' @param max_exclusions The maximum number of extreme points to exclude from 
#' limit calculations.
#' @param highlight_exclusions Boolean signifying whether excluded points are
#' greyed out.
#' @param mr_screen_max_loops Integer or Inf specifying maximum number of times
#' to recursively ignore mr values above the upper range limit when calculating
#' xmr limits. Note this does not affect the calculation of the upper range
#' limit on the mr chart.
#' @param centre_line_tolerance Minimum difference between a point's vertical
#' position and the centre line to count as "on the centre line" for the
#' purposes of shift rule breaks
#' @param floating_median Whether to add a floating median line to the chart,
#' calculated based on the final floating_median_n data points on the chart:
#' "no" - do not display a floating median,
#' "yes" - display a floating median,
#' "auto" - display a floating median if and only if there is at least one point
#' that is part of a shift rule break in the final floating_median_n data points
#' on the chart.
#' @param floating_median_n The number of points to use for calculation of the
#' floating median, if present.

#' ## Output Type
#' Arguments that control how the result is outputted
#' @param plot_chart Boolean specifying whether to plot the chart. If not, the
#' subgroup-aggregated data is returned with centre line, control limits and
#' other analytic output appended as columns.
#' @param show_limits Boolean controlling whether or not to display centre line
#' and control limits
#' @param show_mr `r lifecycle::badge("deprecated")` Use `chart_type` instead.
#' `chart_type = "XMR"` draws the pair and `chart_type = "X"` draws the X
#' chart on its own, which is what `show_mr = FALSE` did.
#' @param write_table `r lifecycle::badge("deprecated")` Save the results
#' yourself instead. `autospc(plot_chart = FALSE)` returns them as a data frame,
#' and `as.data.frame()` on the `autospc_plot` object `autospc()` returns does
#' the same, either of which can be written to a path of your choosing with
#' e.g. `write.csv()`.
#' @param verbosity Integer 0-2 specifying how talkative the algorithm is in the
#' standard output log; the higher the number the more information is provided,
#' none if 0.
#' @param log_file_path if not NULL (the default), path to save log file to.
#' The file extension provided (.rds or .csv) determines the type of file the
#' log data is saved to. Full log data is saved, regardless of verbosity.
#' @param keep_candidate_tables Boolean specifying whether to retain the table
#' of limits for each candidate calculation period the algorithm considers. The
#' candidates themselves are recorded either way, with the reasons they were
#' accepted or rejected; this is the full table of limits for each, and
#' therefore makes an analysed chart several times larger.
#' 
#' ## Chart Appearance
#' Arguments that control aspects of chart visualisation 
#' @param title Optional string specifying chart title. Overrides data$title.
#' @param subtitle Optional string specifying subtitle. Overrides data$subtitle.
#' @param use_caption Boolean controlling whether the caption is displayed.
#' @param override_x_title String specifying horizontal axis label.
#' @param override_y_title String specifying vertical axis label.
#' @param override_y_lim Optional numeric specifying upper limit of the
#' vertical axis.
#' @param x_break Optional numeric specifying spacing of horizontal axis breaks.
#' @param x_date_format Optional string format for date labels on horizontal
#' axis. Passed to scales::date_format.
#' @param x_pad_end Optional, specifies a minimum end point for the horizontal
#' axis.
#' @param extend_limits_to Optional, specifies a point on the horizontal axis
#' to extend the final limits out to
#' @param r1_col Highlight colour for breaks of rule 1 (points outside the
#' control limits)
#' @param r2_col Highlight colour for breaks of rule 2 (shifts)
#' @param point_size Size of plot points, defaults to 2. See
#' \link[ggplot2]{aes_linetype_size_shape} for more details.
#' @param line_width_sf Numeric scale factor for plot line widths. 
#' @param include_annotations Boolean specifying whether to show centre line
#' labels
#' @param basic_annotations Boolean specifying whether to force use of basic
#' annotation positioning. When TRUE, suggested packages ggrepel
#' and ggpp are not required, but annotation arrows are not supported. Defaults
#' to TRUE for R versions prior to 4.3, FALSE otherwise.
#' @param annotation_size Text size for centre line labels
#' @param align_labels Boolean specifying whether or not to align centre line
#' labels at a fixed vertical position
#' @param flip_labels Boolean specifying whether or not to place centre line
#' labels on different sides of the centre line depending on the direction of
#' change from the previous period
#' @param upper_annotation_sf Numeric scale factor specifying upper vertical
#' position of centre line labels as a multiple of the upper control limit
#' @param lower_annotation_sf Numeric scale factor specifying lower vertical
#' position of centre line labels as a multiple of the lower control limit
#' @param annotation_arrows Boolean specifying whether or not to display arrows
#' connecting centre line labels to the centre line they refer to
#' @param annotation_arrow_curve Numeric curvature of the annotation arrows
#' @param override_annotation_dist `r lifecycle::badge("deprecated")` Use
#' `upper_annotation_sf` and `lower_annotation_sf` instead. The equivalent
#' scale factor is `1 + 1/override_annotation_dist`, so
#' `override_annotation_dist = 10` becomes `upper_annotation_sf = 1.1`.
#' @param override_annotation_dist_P `r lifecycle::badge("deprecated")` Use
#' `upper_annotation_sf` and `lower_annotation_sf` instead. These apply to
#' every chart type, so a P or P' chart no longer needs an argument of its own.
#'
#' @return With `plot_chart = TRUE` (the default), an `autospc_plot`: a ggplot
#' of the chart, or of the pair for `chart_type = "XMR"`, which also carries the
#' analysed chart objects it was drawn from and the parameters it was drawn
#' with. Anything that works on a ggplot works on it, including `+`, `print()`
#' and `ggplot2::ggsave()`, and `as.data.frame()` gives the analysis behind it.
#'
#' With `plot_chart = FALSE`, a data frame: the subgroup-aggregated data with
#' the centre line, the control limits and the rest of the analytic output
#' appended as columns.
#'
#' @examples 
#' # Using a C' chart to track changes in the count of monthly attendance 
#' autospc(
#'   ed_attendances_monthly, 
#'   chart_type = "C'", 
#'   x = month_start, 
#'   y = att_all
#' )
#'    
#' #Using a P' chart to track changes in the percentage admitted within 4 hours
#' autospc(
#'   ed_attendances_monthly, 
#'   chart_type = "P'", 
#'   x = month_start, 
#'   y = within_4h, 
#'   n = att_all
#' )
#'
#' #using a shift_rule_threshold of 7 when tracking monthly attendance
#' autospc(
#'   ed_attendances_monthly, 
#'   chart_type = "C'", 
#'   x = month_start, 
#'   y = att_all,
#'   shift_rule_threshold = 7
#' )
#' 
#' @export
autospc <- function(data,
                    x,
                    y,
                    n,
                    chart_type = NULL,
                    ## Algorithm Parameters
                    period_min = 21L,
                    baseline_length = NULL,
                    shift_rule_threshold = 8L,
                    baseline_only = FALSE,
                    establish_every_shift = FALSE,
                    no_regrets = TRUE,
                    overhanging_reversions = TRUE,
                    ## SPC Parameters
                    max_exclusions = 3L,
                    highlight_exclusions = TRUE,
                    mr_screen_max_loops = 1L,
                    centre_line_tolerance = 0,
                    floating_median = "no",
                    floating_median_n = 12L,
                    ## Output Type
                    plot_chart = TRUE,
                    show_limits = TRUE,
                    show_mr = deprecated(),
                    write_table = deprecated(),
                    verbosity = 0L,
                    log_file_path = NULL,
                    keep_candidate_tables = FALSE,
                    ## Chart Appearance
                    title = NULL,
                    subtitle = NULL,
                    use_caption = TRUE,
                    override_x_title = NULL,
                    override_y_title = NULL,
                    override_y_lim = NULL,
                    x_break = NULL,
                    x_date_format = "%Y-%m-%d",
                    x_pad_end = NULL,
                    extend_limits_to = NULL,
                    r1_col = "orange",
                    r2_col = "steelblue3",
                    point_size = 2,
                    line_width_sf = 1,
                    include_annotations = TRUE,
                    basic_annotations = getRversion() < '4.3.0',
                    annotation_size = 3,
                    align_labels = FALSE,
                    flip_labels = FALSE,
                    upper_annotation_sf = NULL,
                    lower_annotation_sf = NULL,
                    annotation_arrows = FALSE,
                    annotation_arrow_curve = 0.3,
                    override_annotation_dist = deprecated(),
                    override_annotation_dist_P = deprecated()
) {

  if(lifecycle::is_present(override_annotation_dist)) {
    lifecycle::deprecate_stop(
      when = "0.0.0.9010",
      what = "autospc(override_annotation_dist)",
      with = "autospc(upper_annotation_sf)",
      details = paste("The equivalent scale factor is 1 + 1/x, so",
                      "override_annotation_dist = 10 becomes",
                      "upper_annotation_sf = 1.1. lower_annotation_sf",
                      "defaults to its mirror image, 2 - upper_annotation_sf.")
    )
  }

  if(lifecycle::is_present(override_annotation_dist_P)) {
    lifecycle::deprecate_stop(
      when = "0.0.0.9010",
      what = "autospc(override_annotation_dist_P)",
      with = "autospc(upper_annotation_sf)",
      details = paste("upper_annotation_sf and lower_annotation_sf apply to",
                      "every chart type, so a P or P' chart no longer needs an",
                      "argument of its own. The equivalent scale factor is",
                      "1 + 1/x.")
    )
  }

  if(lifecycle::is_present(write_table)) {
    lifecycle::deprecate_warn(
      when = "0.0.0.9051",
      what = "autospc(write_table)",
      details = paste("Save the results yourself instead:",
                      "autospc(plot_chart = FALSE) returns them as a data",
                      "frame, and as.data.frame() on the autospc_plot object",
                      "autospc() returns does the same. Either can be written",
                      "to a path of your choosing with e.g. write.csv().",
                      "No file has been written.")
    )
  }

  if(lifecycle::is_present(show_mr)) {
    lifecycle::deprecate_warn(
      when = "0.0.0.9051",
      what = "autospc(show_mr)",
      with = "autospc(chart_type)",
      details = paste('chart_type = "X" draws the X chart on its own, which',
                      'is what show_mr = FALSE did, and chart_type = "XMR"',
                      'draws the pair.')
    )

    # show_mr = FALSE drew the X chart of a pair on its own
    if(isFALSE(show_mr) && identical(chart_type, "XMR")) {
      chart_type <- "X"
    }

  }

  x_name <- resolve_column_name(rlang::enquo(x), "x")
  y_name <- resolve_column_name(rlang::enquo(y), "y")
  n_name <- resolve_column_name(rlang::enquo(n), "n")

  check_x_type(data[[x_name]])

  # Named list of every argument of the call by name, apart from the data,
  # the columns, and the deprecated arguments dealt with above.
  arguments <- mget(setdiff(names(formals()),
                            c("data", "x", "y", "n",
                              autospc_deprecated_arguments())))

  arguments <- validate_algorithm_parameters(arguments)
  
  # The validated analysis parameters
  chart_args <- arguments[autospc_chart_parameters()]

  # The visualisation parameters, as the caller gave them.
  visualisation_params <- arguments[visualisation_param_names()]

  validate_chart_type(chart_type)

  # Build the chart objects: one chart, or an XmR pair.
  charts <- rlang::exec(build_charts,
                        chart_type = chart_type,
                        data = data,
                        x = x_name,
                        y = y_name,
                        n = n_name,
                        !!!chart_args)
  
  # Run the analysis
  charts <- analyse_charts(charts)
  
  # Resolve visualisation parameters, based on chart type. The chart asked is
  # the one drawn in the main panel: the location chart of a pair, or the only
  # chart. Both halves of a pair answer the same.
  visualisation_params <- resolve_default_visualisation_params(
    visualisation_params = visualisation_params,
    chart = charts[[1]]
  )

  report_analysis(charts = charts,
                  show_limits = show_limits,
                  verbosity = verbosity,
                  log_file_path = log_file_path)

  if(!plot_chart) {
    return(charts_as_table(charts = charts,
                           visualisation_params = visualisation_params))
  }

  return(autospc_plot(charts = charts,
                      visualisation_params = visualisation_params))

}
