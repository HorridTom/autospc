#' Plot SPC charts with automated limit recalculation
#' 
#' `plot_auto_SPC()` creates a statistical process control chart from a
#' dataframe, applying the Stable Shift Algorithm to automate recalculation of
#' control limits.
#' 
#' @param df A data frame. For an XMR, C or C' chart, must have columns for:
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
#' @param chartType The type of chart you wish to plot. Available options are:
#' "XMR", "MR", "C", "C'", "P", "P'".
#' 
#' ## Algorithm Parameters
#' @param periodMin The minimum number of points (subgroups) per period,
#' i.e. the minimum number of points required to form control limits. 
#' @param runRuleLength The minimum number of consecutive points above or below
#' the centre line constituting a shift (or "rule 2") break.
#' @param noRecals Boolean - if TRUE, do not recalculate control limits, instead
#' extend limits calculated from the first periodMin points.
#' @param noRegrets Boolean signifying which version of the algorithm should be
#' used. Defines whether limits can change as more data is added or not.
#' @param overhangingReversions Boolean determining whether rule breaks in the
#' opposite direction to a rule break triggering a candidate recalculation
#' prevent recalculation even if they overhang the end of the candidate
#' calculation period. Set to FALSE only with noRegrets = FALSE.
#' 
#' ## SPC Parameters
#' @param maxNoOfExclusions The maximum number of extreme points to exclude from 
#' limit calculations.
#' @param highlightExclusions Boolean signifying whether excluded points are
#' greyed out.
#' @param mr_screen_max_loops Integer or Inf specifying maximum number of times
#' to recursively ignore mr values above the upper range limit when calculating
#' xmr limits. Note this does not affect the calculation of the upper range
#' limit on the mr chart.
#' @param rule2Tolerance Minimum difference between a point's vertical position
#' and the centre line to count as "on the centre line" for the purposes of 
#' shift rule breaks
#' @param floatingMedian Whether to add a floating median line to the chart,
#' calculated based on the final floatingMedian_n data points on the chart:
#' "no" - do not display a floating median,
#' "yes" - display a floating median,
#' "auto" - display a floating median if and only if there is at least one point
#' that is part of a shift rule break in the final floatingMedian_n data points
#' on the chart.
#' @param floatingMedian_n The number of points to use for calculation of the
#' floating median, if present.

#' ## Output Type
#' @param plotChart Boolean specifying whether to plot the chart. If not, the
#' data is returned with centre line, control limits and other analytic output
#' appended as columns.
#' @param showLimits Boolean controlling whether or not to display centre line
#' and control limits
#' @param showMR Logical controlling whether the moving range chart is included
#' in XMR chart
#' @param writeTable Boolean specifying whether to save the data as a CSV 
#' (useful for doing lots of charts at a time).
#' @param verbosity Integer specifying how talkative the algorithm is; the
#' higher the number the more information is provided, none if 0.
#' 
#' ## Chart Appearance
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
#' @param r1_col Highlight colour for breaks of rule 1 (points outside the
#' control limits)
#' @param r2_col Highlight colour for breaks of rule 2 (shifts)
#' @param includeAnnotations Boolean specifying whether to show centre line
#' labels
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
#' plot_auto_SPC(
#'   ed_attendances_monthly, 
#'   chartType = "C'", 
#'   x = Month_Start, 
#'   y = Att_All
#' )
#'    
#' #Using a P' chart to track changes in the percentage admitted within 4 hours
#' plot_auto_SPC(
#'   ed_attendances_monthly, 
#'   chartType = "P'", 
#'   x = Month_Start, 
#'   y = Within_4h, 
#'   n = Att_All
#' )
#'
#' #using a runRuleLength of 7 when tracking monthly attendance
#' plot_auto_SPC(
#'   ed_attendances_monthly, 
#'   chartType = "C'", 
#'   x = Month_Start, 
#'   y = Att_All,
#'   runRuleLength = 7
#' )
#' 
#' @export
plot_auto_SPC <- function(df,
                          x,
                          y,
                          n,
                          chartType = NULL,
                          ## Algorithm Parameters
                          periodMin = 21,
                          runRuleLength = 8,
                          noRecals = FALSE,
                          noRegrets = TRUE,
                          overhangingReversions = TRUE,
                          ## SPC Parameters
                          maxNoOfExclusions = 3,
                          highlightExclusions = TRUE,
                          mr_screen_max_loops = 1L,
                          rule2Tolerance = 0,
                          floatingMedian = "no",
                          floatingMedian_n = 12L,
                          ## Output Type
                          plotChart = TRUE,
                          showLimits = TRUE,
                          showMR = TRUE,
                          writeTable = FALSE,
                          verbosity = 1L,
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
                          r1_col = "orange",
                          r2_col = "steelblue3",
                          includeAnnotations = TRUE,
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
  
  df_original <- df
  
  #rename columns if passed
  df <- rename_columns(df = df,
                       x = {{ x }}, y = {{ y }}, n = {{ n }})
  
  #get title from data
  if(is.null(title) & "title" %in% colnames(df)){
    title <- df$title[1]
  }
  
  if(is.null(subtitle) & "subtitle" %in% colnames(df)){
    subtitle <- df$subtitle[1]
  }
  
  #get type from x variable so that ggplot axes are correct
  #currently only accepting Date, numeric and integer as acceptable types
  xType <- class(df$x)
  if(xType != "Date" & all(xType!= c("POSIXct", "POSIXt")) & xType != "numeric" & xType != "integer"){
    print("Please make sure that your x column is a 'Date', 'numeric' or 'integer' type.")
  }
  
  #decide whether the chart is C or P depending on data format if not specified 
  if(is.null(chartType)){
    
    lifecycle::deprecate_warn(
      when = "0.0.0.9008",
      what = I("chartType  = NULL"),
      details = I("Please explicitly pass the desired chart type")
    )
    
    if(all(c("x", "y") %in% colnames(df))){
      chartType <- "C'"
    }else if(all(c("x", "n", "y") %in% colnames(df))){
      chartType <- "P'"
    }else{
      print("The data you have input is not in the correct format. For C charts, data must contain at least columns 'x' and 'y'. For P charts data must contain at least 'x', 'n' and 'y' columns.")
    }
  }
  
  if(chartType == "MR") {
    mrs <- get_mrs(y = df$y)
    df <- df %>% dplyr::mutate(y = mrs)
  }
  
  # Check annotation arguments
  if(!is.null(override_annotation_dist) |
     !is.null(override_annotation_dist_P)) {
    
    lifecycle::deprecate_warn(
      when = "0.0.0.9010",
      what = I(paste0("plot_auto_SPC(override_annotation_dist,",
                      "override_annotation_dist_P)")),
      details = I(paste0("Please use `plot_auto_SPC(upper_annotation_sf, ",
                         "lower_annotation_sf)` instead. ",
                         "Note that equivalent new arguments can be obtained ",
                         "from the old by transforming as follows: 1+1/x. ",
                         "For example, override_annotation_dist = 10 is ",
                         "equivalent to upper_annotation_sf = 1.1."))
    )
    
    if(!is.null(override_annotation_dist_P) & startsWith(chartType, "P")) {
      oad <- override_annotation_dist_P
    } else {
      oad <- override_annotation_dist
    }
    
    if(is.null(upper_annotation_sf)) {
      upper_annotation_sf <- 1 + 1/oad
    }
    
    if(is.null(lower_annotation_sf)) {
      lower_annotation_sf <- 1 - 1/oad
    }
    
  }
  
  if(is.null(upper_annotation_sf)) {
    upper_annotation_sf <- ifelse(startsWith(chartType, "P"),
                                  1.04,
                                  1.1)
  }
  
  if(is.null(lower_annotation_sf)) {
    lower_annotation_sf <- 2 - upper_annotation_sf
  }
  
  #get control limits
  #df <- dplyr::mutate(df, x = as.Date(x))
  df <- create_SPC_auto_limits_table(df, chartType = chartType, 
                                     periodMin = periodMin,
                                     runRuleLength = runRuleLength,
                                     maxNoOfExclusions  = maxNoOfExclusions,
                                     noRegrets = noRegrets,
                                     verbosity = verbosity,
                                     noRecals = noRecals,
                                     rule2Tolerance = rule2Tolerance,
                                     showLimits = showLimits,
                                     overhangingReversions = overhangingReversions,
                                     mr_screen_max_loops = mr_screen_max_loops)
  
  # chart y limit
  if(nrow(df) < periodMin) {
    ylimlow <- min(df$y,
                   na.rm = TRUE)
  } else if(chartType != "XMR") {
    ylimlow <- 0
  } else {
    ylimlow <- min(df$lcl,
                   df$y,
                   na.rm = TRUE)
    yll_sgn <- sign(ylimlow)
    if(yll_sgn != -1) {
      ylimlow <- ylimlow * 0.9
    } else {
      ylimlow <- ylimlow * 1.1
    }
  }
  
  if(nrow(df) < periodMin){
    ylimhigh <- max(df$y,
                    na.rm = TRUE)
  }else if(chartType == "C" | chartType == "C'"){
    ylimhigh <- max(df$ucl,
                    df$y,
                    na.rm = TRUE) + max(df$ucl,
                                        na.rm = TRUE)/10 + 10
  }else if (chartType == "XMR" | chartType == "MR"){
    ylimhigh <- max(df$ucl,
                    df$y,
                    na.rm = TRUE)*1.1
  }else{
    ylimhigh <- 110
  }
  
  #override y limit if specified
  if(!is.null(override_y_lim)){
    ylimhigh <- override_y_lim
  }
  
  # Ensure axis titles available
  ytitle <- switch(chartType,
                   C = "Number",
                   `C'` = "Number",
                   P = "Percentage",
                   `P'` = "Percentage",
                   XMR = "X",
                   MR = "MR")
  
  if(is.null(override_x_title)) {
    override_x_title <- "Day"
  }
  
  if(is.null(override_y_title)) {
    override_y_title <- ytitle
  }
  
  #start and end dates
  start_x <- min(df$x, na.rm = TRUE)
  end_x <- max(max(df$x, na.rm = TRUE), x_pad_end)
  
 
  #if limits are to be displayed on chart
  if(showLimits == TRUE & nrow(df) >= periodMin){
    
    df <- df %>%
      #dplyr::mutate(x = as.Date(x)) %>%
      #overlap the limit types to make the plot aesthetics work
      #(i.e. so there isn't a gap between calculation and display limits)
      dplyr::mutate(limitChange = ifelse(periodType == dplyr::lag(periodType), FALSE, TRUE)) #%>%
    #mutate(periodType = ifelse(limitChange & periodType == "calculation", lag(periodType), periodType))
    
    
    #re-convert x column back to date if necessary
    if(xType == "Date" | xType == "POSIXct" | xType == "POSIXt"){
      df <- df %>%
        dplyr::mutate(x = as.Date(x))
    }
    
    
    #store break points as vector
    breakPoints <- which(df$breakPoint)
    
    if(highlightExclusions){
      #show exclusions on chart
      df <- df %>% dplyr::mutate(highlight = ifelse(excluded == TRUE & !is.na(excluded),
                                                    "Excluded from limits calculation",
                                                    highlight))
    }
    
    
    addFloatingMedian <- switch(EXPR = floatingMedian,
                                yes = TRUE,
                                auto = any(df %>%
                                             dplyr::slice_tail(n = floatingMedian_n) %>% 
                                             dplyr::pull(rule2)),
                                FALSE)
    
    if(addFloatingMedian) {
      df <- df %>%
        dplyr::mutate(median =
                        dplyr::if_else(dplyr::row_number() >= nrow(df) - floatingMedian_n + 1L,
                                       median(df %>%
                                                dplyr::filter(dplyr::row_number() >= nrow(df) - floatingMedian_n + 1L) %>%
                                                dplyr::pull(y)),
                                       NA))
      
    }
    
    # add annotation information
    df <- add_annotation_data(df = df,
                              chartType = chartType,
                              ylimhigh = ylimhigh,
                              align_labels = align_labels,
                              flip_labels = flip_labels,
                              upper_annotation_sf = upper_annotation_sf,
                              lower_annotation_sf = lower_annotation_sf,
                              annotation_arrow_curve = annotation_arrow_curve)
    
    #create initial plot object without formatting
    pct <- ggplot2::ggplot(df %>% dplyr::filter(!is.na(y)),
                           ggplot2::aes(x,y))
    
    #get periods into groups for plotting
    df <- df %>%
      dplyr::mutate(periodStart = dplyr::if_else(limitChange == TRUE | is.na(limitChange) | breakPoint == TRUE,
                                                 dplyr::row_number(),
                                                 NA_integer_))
    
    df$periodStart <- fill_NA(df$periodStart)
    
    df <- df %>%
      dplyr::mutate(plotPeriod = paste0(periodType, periodStart))
    
    if((chartType == "XMR") & showMR) {
      mc <- match.call()
      mc[["chartType"]] <- "MR"
      if("title" %in% names(mc)) {mc[["title"]] <- NULL}
      if("subtitle" %in% names(mc)) {mc[["subtitle"]] <- NULL}
      mc[["df"]] <- rlang::expr(df_original)
      
      p_mr <- eval(mc)
    }
    
    if(plotChart){
      
      if(use_caption) {
        caption <- paste(chartType,"Shewhart Chart.","\n*Shewhart chart rules apply \nRule 1: Any point outside the control limits \nRule 2: Eight or more consecutive points all above, or all below, the centre line")
      } else {
        caption <- NULL
      }
      
      p <- format_SPC(pct, df = df, r1_col = r1_col, r2_col = r2_col) +
        ggplot2::ggtitle(title,
                         subtitle = subtitle) +
        ggplot2::labs(x = override_x_title,
                      y = override_y_title,
                      caption = paste0(caption),
                      size = 10) +
        ggplot2::scale_y_continuous(limits = c(ylimlow, ylimhigh),
                                    breaks = scales::breaks_pretty(),
                                    labels = scales::label_number(big.mark = ","))
      
      if(addFloatingMedian) {
        p <- p +
          ggplot2::geom_line(data = df, 
                             ggplot2::aes(x, median),
                             linetype = "75551555",
                             colour = "gray50",
                             linewidth = 0.5,
                             show.legend = TRUE,
                             na.rm = TRUE) +
          ggplot2::annotate("text",
                            x = df %>%
                              dplyr::filter(dplyr::row_number() == nrow(df) - floatingMedian_n + 1L) %>%
                              dplyr::pull(x),
                            y = df %>%
                              dplyr::filter(dplyr::row_number() == nrow(df) - floatingMedian_n + 1L) %>%
                              dplyr::pull(median)*0.95,
                            label = "Median",
                            size = 3,
                            colour = "gray50",
                            na.rm = TRUE)
      }
      
      
      
      if(includeAnnotations == TRUE){
        
        p <- add_annotations_to_plot(p = p,
                                     df = df,
                                     annotation_size = annotation_size,
                                     annotation_arrows = annotation_arrows,
                                     annotation_curvature = annotation_curvature)
      }
      
      #formats x axis depending on x type
      if(xType == "Date" | xType == "POSIXct" | xType == "POSIXt"){
        
        #get x axis breaks
        if(is.null(x_break)) {
          x_break <- as.numeric(difftime(as.Date(end_x), as.Date(start_x), units = "days")) / 40
        }
        
        p <- p + ggplot2::scale_x_date(labels = scales::date_format(x_date_format),
                                       breaks = seq(as.Date(start_x), as.Date(end_x), x_break),
                                       limits = c(as.Date(start_x), as.Date(end_x))
        )
        
      }else if(xType == "integer"){
        #get x axis breaks
        if(is.null(x_break)) {
          x_break <- (end_x - start_x) / 40
        }
        
        p <- p + ggplot2::scale_x_continuous(breaks = seq(start_x, end_x, 10),
                                             limits = c(start_x, end_x))
      }else{
        #get x axis breaks
        if(is.null(x_break)) {
          x_break <- (end_x - start_x) / 40
        }
        
        p <- p + ggplot2::scale_x_continuous(breaks = seq(start_x, end_x, x_break),
                                             limits = c(start_x, end_x))
      }
      
      if((chartType == "XMR") & showMR) {
        p <- p + 
          ggplot2::labs(caption = NULL,
                        x = NULL) + 
          ggplot2::theme(axis.text.x = ggplot2::element_blank(), 
                         axis.ticks.x = ggplot2::element_blank())
        
        p_mr <- p_mr + 
          ggplot2::labs(caption = caption)
        
        p <- ggpubr::ggarrange(p, p_mr,
                               ncol = 1,
                               nrow = 2,
                               legend = "right",
                               common.legend = TRUE,
                               align = "v")
      }
      
      suppressWarnings(
        return(p) # Chart output
      )
      
    }else if(writeTable == TRUE){
      # (!plotChart)
      
      title <- gsub(":", "_",title)
      subtitle <- gsub(":","_", subtitle)
      write.csv(df, paste0("tables/", gsub(" ","_",title), "_", gsub(" ","_",subtitle,), ".csv"),
                row.names = FALSE)
      
    }else{
      # (!plotChart)
      
      if(chartType == "XMR" & showMR) {
        
        df <- df %>%
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
      
      df <- df %>%
        dplyr::filter(!is.na(x))
    }
    
  }else{ # only plot timeseries without limits
    
    if(plotChart == TRUE){
      ggplot2::ggplot(df, 
                      ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_line(colour = "black",
                           linewidth = 0.5) +
        ggplot2::geom_point(colour = "black", size = 2) +
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                       panel.grid.major.x = ggplot2::element_line(colour = "grey80"),
                       panel.grid.minor = ggplot2::element_blank(),
                       panel.background = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1.0, size = 14),
                       axis.text.y = ggplot2::element_text(size = 14), axis.title = ggplot2::element_text(size = 14),
                       plot.title = ggplot2::element_text(size = 20, hjust = 0),
                       plot.subtitle = ggplot2::element_text(size = 16, face = "italic"),
                       axis.line = ggplot2::element_line(colour = "grey60"),
                       plot.caption = ggplot2::element_text(size = 10, hjust = 0.5)) +
        ggplot2::ggtitle(title,
                         subtitle = subtitle) +
        ggplot2::labs(x = override_x_title,
                      y = override_y_title,
                      size = 10) +
        ggplot2::scale_y_continuous(limits = c(ylimlow, ylimhigh),
                                    breaks = scales::breaks_pretty(),
                                    labels = scales::number_format(accuracy = 1,
                                                                   big.mark = ","))

    }else{
      
      return(df) # Table output
    }
    
  }
  
}


format_SPC <- function(cht, df, r1_col, r2_col, ymin, ymax) {
  point_colours <- c("Rule 1" = r1_col, "Rule 2" = r2_col, 
                     "None" = "black", "Excluded from limits calculation" = "grey")
  
  #get exemplar calculation and display periods
  plot_periods <- df$plotPeriod
  
  first_display_period <- plot_periods[grep("display", plot_periods)[1]]
  first_calc_period <- plot_periods[1]
  
  suppressWarnings( # to avoid the warning about using alpha for discrete vars
    cht + 
      ggplot2::geom_line(colour = "black",
                         linewidth = 0.5,
                         na.rm = TRUE) + 
      ggplot2::geom_line(data = df, 
                         ggplot2::aes(x,cl,
                                      alpha = plotPeriod),
                         linetype = "solid",
                         linewidth = 0.75,
                         na.rm = TRUE) +
      ggplot2::geom_line(data = df, 
                         ggplot2::aes(x,lcl,
                                      alpha = plotPeriod),
                         linetype = "42",
                         linewidth = 0.5,
                         show.legend = FALSE,
                         na.rm = TRUE) +
      ggplot2::geom_line(data = df, 
                         ggplot2::aes(x,ucl,
                                      alpha = plotPeriod),
                         linetype = "42",
                         linewidth = 0.5,
                         show.legend = FALSE,
                         na.rm = TRUE) +
      ggplot2::geom_point(ggplot2::aes(colour = highlight), size = 2,
                          na.rm = TRUE) +
      ggplot2::scale_color_manual("Rule triggered*", values = point_colours) + 
      ggplot2::scale_alpha_discrete("Period Type",
                                    labels = if(!is.na(first_display_period)) {
                                      c("Calculation", "Display")
                                      } else {
                                        c("Calculation")
                                      },
                                    range = if(!is.na(first_display_period)) {
                                      c(1, 0.4)
                                    } else {
                                      c(1, 1)
                                    },
                                    breaks = if(!is.na(first_display_period)) {
                                      c(first_calc_period,
                                        first_display_period)
                                      } else {
                                        c(first_calc_period)
                                        },
                                    guide = ggplot2::guide_legend(
                                      override.aes = list(alpha = if(!is.na(first_display_period)) {
                                        c(1, 0.4)
                                        } else {
                                          c(1)
                                          }
                                      )
                                    )
      ) +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                     panel.grid.major.x = ggplot2::element_line(colour = "grey80"),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1.0, size = 14),
                     axis.text.y = ggplot2::element_text(size = 14), axis.title = ggplot2::element_text(size = 14),
                     plot.title = ggplot2::element_text(size = 20, hjust = 0),
                     plot.subtitle = ggplot2::element_text(size = 16, face = "italic"),
                     axis.line = ggplot2::element_line(colour = "grey60"),
                     plot.caption = ggplot2::element_text(size = 10, hjust = 0.5)) 
  )
}

