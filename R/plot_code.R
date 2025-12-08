#' Plot SPC charts with automated limit recalculation
#' 
#' `autospc()` creates a statistical process control chart from a
#' dataframe, applying the Stable Shift Algorithm to automate recalculation of
#' control limits.
#' 
#' @param data A data frame. For an XMR, C or C' chart, must have columns for:
#' \itemize{
#'  \item the subgrouping variable, to be plotted on the horizontal axis, (x);
#'  \item the variable of interest to be plotted on the vertical axis (y);
#'  \item and optionally, a title and subtitle for the plot.
#' } \cr
#' For a P or P' chart, must have columns for:
#' \itemize{
#'  \item the subgrouping variable, to be plotted on the horizontal axis, (x);
#'  \item the total count or denominator (n);
#'  \item the count meeting criteria, or numerator (y);
#'  \item and optionally, a title and subtitle for the plot.
#' }
#' @param x Name of column (passed using tidyselect semantics) to use as
#' subgroups on the horizontal axis of the chart.
#' @param y Name of column (passed using tidyselect semantics) to use as:
#' \itemize{
#'  \item the variable to be plotted for XMR charts,
#'  \item count (plotted on the vertical axis) for C and C' charts,
#'  \item numerator of the proportion (plotted on the vertical axis) for P and
#'  P' charts.
#'  }
#' @param n Name of column (passed using tidyselect semantics) to use as
#' denominator for P and P' charts.
#' @param chart_type The type of chart you wish to plot. Available options are:
#' "XMR", "MR", "C", "C'", "P", "P'".
#' 
#' ## Algorithm Parameters
#' Parameters that control behaviour of the algorithm used to re-establish
#' control limits.
#' @param period_min The minimum number of points (subgroups) per period,
#' i.e. the minimum number of points required to form control limits. 
#' @param baseline_length Integer, overrides period_min for the first calculation period
#' only, if specified
#' @param shift_rule_threshold The minimum number of consecutive points above or below
#' the centre line constituting a shift (or "rule 2") break.
#' @param baseline_only Boolean - if TRUE, do not recalculate control limits, instead
#' extend limits calculated from the first period_min points.
#' @param establish_every_shift Boolean - whether to bypass the Stable Shift Algorithm
#' and simply re-establish limits at every shift rule break (respecting
#' period_min)
#' @param no_regrets Boolean signifying which version of the algorithm should be
#' used. Defines whether limits can change as more data is added or not.
#' @param overhanging_reversions Boolean determining whether rule breaks in the
#' opposite direction to a rule break triggering a candidate recalculation
#' prevent recalculation even if they overhang the end of the candidate
#' calculation period. Set to FALSE only with no_regrets = FALSE.
#' 
#' ## SPC Parameters
#' Parameters that control how cetnre line and control limits are established
#' for each period, and details of how SPC rules are applied
#' @param max_exclusions The maximum number of extreme points to exclude from 
#' limit calculations.
#' @param highlight_exclusions Boolean signifying whether excluded points are
#' greyed out.
#' @param mr_screen_max_loops Integer or Inf specifying maximum number of times
#' to recursively ignore mr values above the upper range limit when calculating
#' xmr limits. Note this does not affect the calculation of the upper range
#' limit on the mr chart.
#' @param centre_line_tolerance Minimum difference between a point's vertical position
#' and the centre line to count as "on the centre line" for the purposes of 
#' shift rule breaks
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
#' data is returned with centre line, control limits and other analytic output
#' appended as columns.
#' @param show_limits Boolean controlling whether or not to display centre line
#' and control limits
#' @param show_mr Logical controlling whether the moving range chart is included
#' in XMR chart
#' @param write_table Boolean specifying whether to save the data as a CSV 
#' (useful for doing lots of charts at a time).
#' @param verbosity Integer 0-2 specifying how talkative the algorithm is in the
#' standard output log; the higher the number the more information is provided,
#' none if 0.
#' @param log_file_path if not NULL (the default), path to save log file to.
#' The file extension provided (.rds or .csv) determines the type of file the
#' log data is saved to. Full log data is saved, regardless of verbosity.
#' 
#' ## Chart Appearance
#' Arguments that control aspects of chart visualisation 
#' @param title Optional string specifying chart title. Overrides df$title.
#' @param subtitle Optional string specifying subtitle. Overrides df$subtitle.
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
#' @param override_annotation_dist Deprecated
#' @param override_annotation_dist_P Deprecated
#'
#' @return An SPC ggplot or corresponding data 
#'
#' @examples 
#' # Using a C' chart to track changes in the count of monthly attendance 
#' autospc(
#'   ed_attendances_monthly, 
#'   chart_type = "C'", 
#'   x = Month_Start, 
#'   y = Att_All
#' )
#'    
#' #Using a P' chart to track changes in the percentage admitted within 4 hours
#' autospc(
#'   ed_attendances_monthly, 
#'   chart_type = "P'", 
#'   x = Month_Start, 
#'   y = Within_4h, 
#'   n = Att_All
#' )
#'
#' #using a shift_rule_threshold of 7 when tracking monthly attendance
#' autospc(
#'   ed_attendances_monthly, 
#'   chart_type = "C'", 
#'   x = Month_Start, 
#'   y = Att_All,
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
                          period_min = 21,
                          baseline_length = NULL,
                          shift_rule_threshold = 8,
                          baseline_only = FALSE,
                          establish_every_shift = FALSE,
                          no_regrets = TRUE,
                          overhanging_reversions = TRUE,
                          ## SPC Parameters
                          max_exclusions = 3,
                          highlight_exclusions = TRUE,
                          mr_screen_max_loops = 1L,
                          centre_line_tolerance = 0,
                          floating_median = "no",
                          floating_median_n = 12L,
                          ## Output Type
                          plot_chart = TRUE,
                          show_limits = TRUE,
                          show_mr = TRUE,
                          write_table = FALSE,
                          verbosity = 0L,
                          log_file_path = NULL,
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
                          override_annotation_dist = NULL,
                          override_annotation_dist_P = NULL
) { 
  
  df_original <- data
  
  # Rename columns if passed
  data <- rename_columns(df = data,
                       x = {{ x }}, y = {{ y }}, n = {{ n }})
  
  # Preprocess inputs
  preprocessed_vars <- preprocess_inputs(
    df = data,
    chart_type = chart_type,
    title = title,
    subtitle = subtitle,
    upper_annotation_sf = upper_annotation_sf,
    lower_annotation_sf = lower_annotation_sf,
    override_annotation_dist = override_annotation_dist,
    override_annotation_dist_P = override_annotation_dist_P
  )
  
  data                <- preprocessed_vars$df
  chart_type           <- preprocessed_vars$chart_type
  title               <- preprocessed_vars$title
  subtitle            <- preprocessed_vars$subtitle
  xType               <- preprocessed_vars$xType
  upper_annotation_sf <- preprocessed_vars$upper_annotation_sf
  lower_annotation_sf <- preprocessed_vars$lower_annotation_sf
  
  
  # Get control limits
  data <- create_SPC_auto_limits_table(
    data,
    chart_type = chart_type, 
    period_min = period_min,
    baseline_length = baseline_length,
    shift_rule_threshold = shift_rule_threshold,
    max_exclusions  = max_exclusions,
    no_regrets = no_regrets,
    verbosity = verbosity,
    baseline_only = baseline_only,
    establish_every_shift = establish_every_shift,
    centre_line_tolerance = centre_line_tolerance,
    show_limits = show_limits,
    overhanging_reversions = overhanging_reversions,
    mr_screen_max_loops = mr_screen_max_loops
  )
  
  # Output log data
  log_output(data,
             verbosity = verbosity,
             chart_type = chart_type,
             log_file_path = log_file_path)
  
  # Postprocess data
  
  postprocessing_vars <- postprocess(
    df = data,
    chart_type = chart_type,
    period_min = period_min,
    show_limits = show_limits,
    override_x_title = override_x_title,
    override_y_title = override_y_title,
    override_y_lim = override_y_lim,
    x_pad_end = x_pad_end,
    extend_limits_to = extend_limits_to,
    xType = xType
  )
  
  data               <- postprocessing_vars$df
  override_x_title   <- postprocessing_vars$override_x_title
  override_y_title   <- postprocessing_vars$override_y_title
  num_non_missing_y  <- postprocessing_vars$num_non_missing_y
  start_x            <- postprocessing_vars$start_x
  x_max              <- postprocessing_vars$x_max
  end_x              <- postprocessing_vars$end_x
  ylimhigh           <- postprocessing_vars$ylimhigh
  ylimlow            <- postprocessing_vars$ylimlow
  
  
  # Check whether limits are to be displayed on chart
  if(show_limits & num_non_missing_y >= period_min){
    
    data <- postprocess_spc(
      df = data,
      chart_type = chart_type,
      highlight_exclusions = highlight_exclusions,
      floating_median = floating_median,
      floating_median_n = floating_median_n,
      extend_limits_to = extend_limits_to,
      align_labels = align_labels,
      flip_labels = flip_labels,
      upper_annotation_sf = upper_annotation_sf,
      lower_annotation_sf = lower_annotation_sf,
      annotation_arrow_curve = annotation_arrow_curve,
      ylimhigh = ylimhigh,
      x_max = x_max
    )
    
    if((chart_type == "XMR") & show_mr) {
      mc <- match.call()
      mc[["chart_type"]] <- "MR"
      if("title" %in% names(mc)) {mc[["title"]] <- NULL}
      if("subtitle" %in% names(mc)) {mc[["subtitle"]] <- NULL}
      mc[["data"]] <- rlang::expr(df_original)
      
      p_mr <- eval(mc)
    } else {
      p_mr <- NA
    }
    
    if(plot_chart){
      
      p <- create_spc_plot(
        df = data,
        p_mr = p_mr,
        chart_type = chart_type,
        xType = xType,
        start_x = start_x,
        end_x = end_x,
        x_max = x_max,
        ylimlow = ylimlow,
        ylimhigh = ylimhigh,
        num_non_missing_y = num_non_missing_y,
        period_min = period_min,
        title = title,
        subtitle = subtitle,
        use_caption = use_caption,
        override_x_title = override_x_title,
        override_y_title = override_y_title,
        r1_col = r1_col,
        r2_col = r2_col,
        point_size = point_size,
        line_width_sf = line_width_sf,
        include_annotations = include_annotations,
        basic_annotations = basic_annotations,
        annotation_size = annotation_size,
        annotation_arrows = annotation_arrows,
        annotation_curvature = annotation_arrow_curve,
        floating_median_n = floating_median_n,
        show_mr = show_mr,
        x_break = x_break,
        x_date_format = x_date_format
      )
      
      suppressWarnings(
        return(p) # Chart output
      )
      
    } else if(write_table) {
      # (!plot_chart)
      
      title <- gsub(":", "_",title)
      subtitle <- gsub(":","_", subtitle)
      write.csv(df,
                paste0("tables/",
                       gsub(" ", "_", title),
                       "_",
                       gsub(" ", "_", subtitle,),
                       ".csv"),
                row.names = FALSE)
      
    } else {
      # (!plot_chart)
      
      if(chart_type == "XMR" & show_mr) {
        
        data <- data %>%
          dplyr::left_join(p_mr %>%
                             dplyr::select(x,
                                           mr = y,
                                           amr = cl,
                                           url = ucl,
                                           lrl = lcl),
                           by = c("x" = "x")) %>% 
          dplyr::select(x, y, cl, ucl, lcl,
                        mr, amr, url, lrl,
                        dplyr::everything())
      }
      
      data <- data %>%
        dplyr::filter(!is.na(x))
      
      return(data)
    }
    
  } else { # Plot only the time series, without limits
    if(plot_chart == TRUE) {
      p <- create_timeseries_plot(
        df = data,
        title = title,
        subtitle = subtitle,
        override_x_title = override_x_title,
        override_y_title = override_y_title,
        ylimlow = ylimlow,
        ylimhigh = ylimhigh,
        point_size = point_size,
        line_width_sf = line_width_sf)
      
      return(p)
    } else {
      return(data) # Table output
    }
  }
}

