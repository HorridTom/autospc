library(tidyverse)
library(qicharts2)
library(scales)
library(zoo)
library(lubridate)
library(wktmo)
library(grid)
library(gridExtra)

source("R/spc_rules.R")
source("R/functions.R")
source("R/automation_function_interface.R")

#function to plot automated SPC charts
#' plot_auto_SPC
#'
#' @param data For a C or C' chart: a data frame with columns x, y, title (optional) 
#' and place (optional, N.B this is the subtitle ususally used for place)
#' For a P or P' chart: a data frame with columns x, n (total), b (number of breaches), 
#' title (optional), place (optional, N.B this is the subtitle ususally used for place) 
#' @param chart_typ the type of chart you wish to plot (e.g. "C", "C'", "P", "P'")
#' @param periodMin the minimum number of points per period.
#' @param runRuleLength the number of points above or below the centre line needed
#' for a rule 2 break
#' @param maxNoOfExclusions the maximum number of extreme points to exclude from 
#' limit calculations
#' @param highlightExclusions Boolean signifying whether excluded points are greyed out
#' @param cht_title A chart title to override any title specified in the data
#' @param subtitle A subtitle to override any subtitle specified in the data
#' @param plot_chart Boolean specifying whether to plot the chart or return the data
#' @param write_table Boolean specifying whether to save the data as a CSV 
#' (useful for foing lots of charts at a time) 
#'
#'
#' @return An SPC ggplot or corresponding data
#'
#'
#' @examples
plot_auto_SPC <- function(df,
                          cht_type = "C",
                          periodMin = 21,
                          runRuleLength = 8,
                          maxNoOfExclusions = 3,
                          highlightExclusions = T,
                          cht_title = NULL,
                          subtitle = NULL,
                          plot_chart = T,
                          write_table = F,
                          
                          #overrides for plot aesthetics not detailed in roxygen skeleton
                          override_y_lim = NULL,
                          override_annotation_dist = 10,
                          override_annotation_dist_P = 25,
                          r1_col = "orange",
                          r2_col = "steelblue3"
) { 
  
  #get title from data
  if(is.null(cht_title)){
    cht_title = df$title[1]
  }
  
  if(is.null(subtitle)){
    subtitle = df$place[1]
  }
  
  #get limits
  df <- mutate(df, x = as.Date(x))
  df <- create_SPC_auto_limits_table(df, cht_type = cht_type, maxNoOfExclusions  = maxNoOfExclusions)
  df <- df %>%
    mutate(x = as.Date(x)) %>%
    #overlap the limit types to make the plot aesthetics work 
    #(i.e. so there isn't a gap between calculation and display limits)
    mutate(limitChange = ifelse(periodType == dplyr::lag(periodType), F, T)) %>%
    mutate(periodType = ifelse(limitChange & periodType == "calculation", lag(periodType), periodType)) 
  
  #store break points as vector
  breakPoints <- which(df$breakPoint)
  
  if(highlightExclusions){
    #show exclusions on chart
    df <- df %>% mutate(highlight = ifelse(excluded == T & !is.na(excluded), 
                                           "Excluded from limits calculation", 
                                           highlight))
  }
  
  pct <- ggplot(df, aes(x,y))
  
  # chart y limit
  ylimlow <- 0
  ylimhigh <- max(df$ucl, df$y) + max(df$ucl)/10 +10 
  ytitle <- ifelse(cht_type == "C" | cht_type == "C'", "Number", "Percentage within 4hrs")
  
  #start and end dates
  st.dt <- as.Date(min(df$x, na.rm = T))
  ed.dt <- as.Date(max(df$x, na.rm = T))
  
  if(!is.null(override_y_lim)){
    ylimhigh <- override_y_lim
  }
  
  #for annotations
  cl_start <- round(df$cl[1])
  ucl_start <- round(df$ucl[1])
  cl_end <- round(df$cl[(nrow(df)-1)])
  
  if(plot_chart == T){
    
    annotation_dist_fact <- ifelse(cht_type == "C" | cht_type == "C'", 
                                   override_annotation_dist, 
                                   override_annotation_dist_P)
    caption <- paste(cht_type,"Shewhart Chart.","\n*Shewhart chart rules apply (see Understanding the Analysis tab for more detail) \nRule 1: Any point outside the control limits \nRule 2: Eight or more consecutive points all above, or all below, the centre line")

    p <- format_SPC(pct, df = df, r1_col = r1_col, r2_col = r2_col) +
      scale_x_date(labels = date_format("%Y-%m-%d"), breaks = seq(st.dt, ed.dt, date_break),
                   limits = c(st.dt, ed.dt)) +
      ggtitle(cht_title, subtitle = subtitle) +
      labs(x = "Day", y = ytitle,
           caption = paste0(caption),
           size = 10) +
      scale_y_continuous(limits = c(ylimlow, ylimhigh),
                         breaks = breaks_pretty(),
                         labels = number_format(accuracy = 1, big.mark = ",")) +
      annotate("text", x = st.dt, y = ucl_start + ucl_start/annotation_dist_fact, label = cl_start) +
      annotate("text", x = df$x[breakPoints] + days(2), y = df$ucl[breakPoints] + ucl_start/annotation_dist_fact, label = round(df$cl[breakPoints]))
    
    p
    

    
  }else if(write_table == T){
    
    cht_title <- gsub(":", "_",cht_title)
    subtitle <- gsub(":","_", subtitle)
    write.csv(cht_data, paste0("tables/", gsub(" ","_",cht_title), "_", gsub(" ","_",subtitle,), ".csv"))
    
  }else{
    df
  }
}


format_SPC <- function(cht, df, r1_col, r2_col, ymin, ymax) {
  point_colours <- c("Rule 1" = r1_col, "Rule 2" = r2_col, "None" = "black", "Excluded from limits calculation" = "grey")
  cht + 
    geom_line(colour = "black", size = 0.5) + 
    geom_line(data = mutate(df, cl = ifelse(periodType == "calculation", cl, NA)), aes(x,cl), size = 0.75, linetype = "solid") +
    geom_line(data = mutate(df, cl = ifelse(periodType == "display", cl, NA)), aes(x,cl), size = 0.75, linetype = "42") +
    geom_line(data = mutate(df, ucl = ifelse(periodType == "calculation", ucl, NA)), aes(x,ucl), size = 0.5, linetype = "solid") +
    geom_line(data = mutate(df, ucl = ifelse(periodType == "display", ucl, NA)), aes(x,ucl), size = 0.5, linetype = "84") +
    geom_line(data = mutate(df, lcl = ifelse(periodType == "calculation", lcl, NA)), aes(x,lcl), size = 0.5, linetype = "solid") +
    geom_line(data = mutate(df, lcl = ifelse(periodType == "display", lcl, NA)), aes(x,lcl), size = 0.5, linetype = "84") +
  
    geom_point(aes(colour = highlight), size = 2) +
    scale_color_manual("Rule triggered*", values = point_colours) + 
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "grey80"),
          panel.grid.minor = element_blank(), panel.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.0, size = 14),
          axis.text.y = element_text(size = 14), axis.title = element_text(size = 14),
          plot.title = element_text(size = 20, hjust = 0),
          plot.subtitle = element_text(size = 16, face = "italic"),
          axis.line = element_line(colour = "grey60"),
          plot.caption = element_text(size = 10, hjust = 0.5)) 
  
}

