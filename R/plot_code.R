#function to plot automated SPC charts
#' plot_auto_SPC
#'
#' @param data For a C or C' chart: a data frame with columns x, y, title (optional) 
#' and subtitle (optional)
#' For a P or P' chart: a data frame with columns x, n (total), b (number of breaches), 
#' title (optional), subtitle (optional) 
#' @param chartType the type of chart you wish to plot (e.g. "C", "C'", "P", "P'")
#' @param periodMin the minimum number of points per period.
#' @param runRuleLength the number of points above or below the centre line needed
#' for a rule 2 break
#' @param maxNoOfExclusions the maximum number of extreme points to exclude from 
#' limit calculations
#' @param highlightExclusions Boolean signifying whether excluded points are greyed out
#' @param title A chart title to override any title specified in the data
#' @param subtitle A subtitle to override any subtitle specified in the data
#' @param plotChart Boolean specifying whether to plot the chart or return the data
#' @param writeTable Boolean specifying whether to save the data as a CSV 
#' (useful for doing lots of charts at a time) 
#' @param noRegrets Boolean signifying which version of the algorithm should be used. 
#' Defines whether limits can change as more data is added or not.
#'
#' @return An SPC ggplot or corresponding data
#'
#' @importFrom magrittr %>%
#' @export
#' @examples
plot_auto_SPC <- function(df,
                          chartType = NULL,
                          periodMin = 21,
                          runRuleLength = 8,
                          maxNoOfExclusions = 3,
                          highlightExclusions = T,
                          title = NULL,
                          subtitle = NULL,
                          plotChart = T,
                          writeTable = F,
                          noRegrets = T,
                          
                          #overrides for plot aesthetics not detailed in roxygen skeleton
                          override_x_title = NULL,
                          override_y_title = NULL,
                          override_y_lim = NULL,
                          override_annotation_dist = 10,
                          override_annotation_dist_P = 25,
                          prime_chart_volume = 1500,
                          date_break = NULL,
                          r1_col = "orange",
                          r2_col = "steelblue3"
) { 
  
  #get title from data
  if(is.null(title)){
    title = df$title[1]
  }
  
  if(is.null(subtitle)){
    subtitle = df$subtitle[1]
  }
  
  #decide whether the chart is C or P depending on data format if not specified 
  if(is.null(chartType)){
    if(all(c("x", "y") %in% colnames(df))){
      #rule of thumb for when to convert use a prime chart
      chartType <- dplyr::if_else(max(df$y) > prime_chart_volume,"C'","C")
    }else if(all(c("x", "n", "b") %in% colnames(df))){
      #rule of thumb for when to convert use a prime chart
      chartType <- dplyr::if_else(max(df$n) > prime_chart_volume,"P'","P")
    }else{
      print("The data you have input is not in the correct format. For C charts, data
          must contain at least columns 'x' and 'y'. For P charts data must contain
          at least 'x', 'n' and 'b' columns.")
    }
  }

  #get control limits
  df <- dplyr::mutate(df, x = as.Date(x))
  df <- create_SPC_auto_limits_table(df, chartType = chartType, 
                                     maxNoOfExclusions  = maxNoOfExclusions,
                                     noRegrets = noRegrets)
  df <- df %>%
    dplyr::mutate(x = as.Date(x)) %>%
    #overlap the limit types to make the plot aesthetics work 
    #(i.e. so there isn't a gap between calculation and display limits)
    dplyr::mutate(limitChange = ifelse(periodType == dplyr::lag(periodType), F, T)) #%>%
    #mutate(periodType = ifelse(limitChange & periodType == "calculation", lag(periodType), periodType)) 
  
  #store break points as vector
  breakPoints <- which(df$breakPoint)
  
  if(highlightExclusions){
    #show exclusions on chart
    df <- df %>% dplyr::mutate(highlight = ifelse(excluded == T & !is.na(excluded), 
                                           "Excluded from limits calculation", 
                                           highlight))
  }
  
  pct <- ggplot2::ggplot(df, ggplot2::aes(x,y))
  
  # chart y limit
  ylimlow <- 0
  if(chartType == "C" | chartType == "C'"){
    ylimhigh <- max(df$ucl, df$y) + max(df$ucl)/10 +10 
    # ylim_choices <- c(50, 100, 200, 400, 600, 1000, 2000, 8000)
    # ylimhigh <- ylim_choices[which.min(ylim_choices - max(df$y) < 0)]
  }else{
    ylimhigh <- 110
  }
  
  ytitle <- ifelse(chartType == "C" | chartType == "C'", "Number", "Percentage within 4hrs")
  
  #start and end dates
  st.dt <- as.Date(min(df$x, na.rm = T))
  ed.dt <- as.Date(max(df$x, na.rm = T))
  
  #get date breaks
  if(is.null(date_break)){
    date_break <- as.numeric(difftime(ed.dt, st.dt, units = "days")) / 40
  }
  
  #get y limit
  if(!is.null(override_y_lim)){
    ylimhigh <- override_y_lim
  }
  
  #for annotations
  cl_start <- round(df$cl[1])
  ucl_start <- round(df$ucl[1])
  cl_end <- round(df$cl[(nrow(df)-1)])
  
  if(plotChart == T){
    
    annotation_dist_fact <- ifelse(chartType == "C" | chartType == "C'", 
                                   override_annotation_dist, 
                                   override_annotation_dist_P)
    caption <- paste(chartType,"Shewhart Chart.","\n*Shewhart chart rules apply (see Understanding the Analysis tab for more detail) \nRule 1: Any point outside the control limits \nRule 2: Eight or more consecutive points all above, or all below, the centre line")

    p <- format_SPC(pct, df = df, r1_col = r1_col, r2_col = r2_col) +
      ggplot2::scale_x_date(labels = scales::date_format("%Y-%m-%d"),
                            breaks = seq(st.dt, ed.dt, date_break),
                            limits = c(st.dt, ed.dt)) +
      ggplot2::ggtitle(title, subtitle = subtitle) +
      ggplot2::labs(x = "Day", y = ytitle,
           caption = paste0(caption),
           size = 10) +
      ggplot2::scale_y_continuous(limits = c(ylimlow, ylimhigh),
                         breaks = scales::breaks_pretty(),
                         labels = scales::number_format(accuracy = 1, big.mark = ",")) +
      ggplot2::annotate("text", x = st.dt, y = ucl_start + ucl_start/annotation_dist_fact, label = cl_start) +
      ggplot2::annotate("text", x = df$x[breakPoints] + lubridate::days(2), y = df$ucl[breakPoints] + ucl_start/annotation_dist_fact, label = round(df$cl[breakPoints]))
    
    p
    

    
  }else if(writeTable == T){
    
    title <- gsub(":", "_",title)
    subtitle <- gsub(":","_", subtitle)
    write.csv(cht_data, paste0("tables/", gsub(" ","_",title), "_", gsub(" ","_",subtitle,), ".csv"))
    
  }else{
    df
  }
}


format_SPC <- function(cht, df, r1_col, r2_col, ymin, ymax) {
  point_colours <- c("Rule 1" = r1_col, "Rule 2" = r2_col, "None" = "black", "Excluded from limits calculation" = "grey")
  cht + 
    ggplot2::geom_line(colour = "black", size = 0.5) + 
    ggplot2::geom_line(data = dplyr::mutate(df, cl = ifelse(periodType == "calculation", cl, NA)), ggplot2::aes(x,cl), size = 0.75, linetype = "solid") +
    ggplot2::geom_line(data = dplyr::mutate(df, cl = ifelse(periodType == "display", cl, NA)), ggplot2::aes(x,cl), size = 0.75, linetype = "42") +
    ggplot2::geom_line(data = dplyr::mutate(df, ucl = ifelse(periodType == "calculation", ucl, NA)), ggplot2::aes(x,ucl), size = 0.5, linetype = "solid") +
    ggplot2::geom_line(data = dplyr::mutate(df, ucl = ifelse(periodType == "display", ucl, NA)), ggplot2::aes(x,ucl), size = 0.5, linetype = "84") +
    ggplot2::geom_line(data = dplyr::mutate(df, lcl = ifelse(periodType == "calculation", lcl, NA)), ggplot2::aes(x,lcl), size = 0.5, linetype = "solid") +
    ggplot2::geom_line(data = dplyr::mutate(df, lcl = ifelse(periodType == "display", lcl, NA)), ggplot2::aes(x,lcl), size = 0.5, linetype = "84") +
  
    ggplot2::geom_point(ggplot2::aes(colour = highlight), size = 2) +
    ggplot2::scale_color_manual("Rule triggered*", values = point_colours) + 
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
  
}

