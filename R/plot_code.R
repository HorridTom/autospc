library(tidyverse)
library(dplyr)
library(qicharts2)
library(ggplot2)
library(scales)
library(zoo)
library(lubridate)
library(wktmo)
library(grid)
library(gridExtra)

#source("spc_rules.R")

plot_auto_SPC <- function(df, 
                     r1_col = "orange", r2_col = "steelblue3", 
                     cht_title = NULL,
                     place_title = NULL,
                     cht_type = "C",
                     plot_chart = T,
                     write_table = F,
                     override_y_lim = NULL,
                     override_annotation_dist = 10,
                     override_annotation_dist_P = 25,
                     add_lines = F,
                     line_height = 100,
                     date_break = 7,
                     df_name = df
) { 
  
  #get title
  if(is.null(cht_title)){
    cht_title = df$title[1]
  }
  
  if(is.null(place_title)){
    place_title = df$place[1]
  }
  
  #get limits
  df <- mutate(df, x = as.Date(x))
  df <- create_SPC_auto_limits_table(df, cht_type = cht_type)
  df <- df %>%
    mutate(date = as.Date(x)) %>%
    mutate(x = as.Date(x)) %>%
    #overlap the limit types to make the plot aesthetics work
    mutate(limitChange = ifelse(periodType == dplyr::lag(periodType), F, T)) %>%
    mutate(periodType = ifelse(limitChange & periodType == "calculation", lag(periodType), periodType)) %>%
    #get break points
    mutate(breakPoint = ifelse(cl == dplyr::lag(cl), F, T))
  
  #store break points as vector
  breakPoints <- which(df$breakPoint)
  

  pct <- ggplot(df, aes(x,y))
  
  # chart y limit
  ylimlow <- 0
  ylimhigh <- max(df$y) + max(df$y)/10 +10
  ytitle <- "Number"
  
  #start and end dates
  st.dt <- as.Date(min(df$date))
  ed.dt <- as.Date(max(df$date))
  
  if(!is.null(override_y_lim)){
    ylimhigh <- override_y_lim
  }
  
  cl_start <- round(df$cl[1])
  cl_end <- round(df$cl[(nrow(df)-1)])
  
  if(plot_chart == T){
    
    annotation_dist_fact <- ifelse(cht_type == "C" | cht_type == "C'", override_annotation_dist, override_annotation_dist_P)
    caption <- paste(cht_type,"Shewhart Chart.","\n*Shewhart chart rules apply (see Understanding the Analysis tab for more detail) \nRule 1: Any point outside the control limits \nRule 2: Eight or more consecutive points all above, or all below, the centre line")
    lines_caption <- ifelse(add_lines == T, "\n**hospitality re-opened, gatherings up to 30 allowed.", "")
    
    p <- format_SPC(pct, df = df, r1_col = r1_col, r2_col = r2_col) +
      scale_x_date(labels = date_format("%Y-%m-%d"), breaks = seq(st.dt, ed.dt, date_break),
                   limits = c(st.dt, ed.dt)) +
      ggtitle(cht_title, subtitle = place_title) +
      labs(x = "Day", y = ytitle,
           caption = paste0(caption, lines_caption),
           size = 10) +
      scale_y_continuous(limits = c(ylimlow, ylimhigh),
                         breaks = breaks_pretty(),
                         labels = number_format(accuracy = 1, big.mark = ",")) +
      annotate("text", x = st.dt, y = cl_start + cl_start/annotation_dist_fact, label = cl_start) +
      #annotate("text", x = ed.dt, y = cl_end + cl_start/annotation_dist_fact, label = cl_end) +
      annotate("text", x = df$x[breakPoints] + days(2), y = df$cl[breakPoints] + cl_start/annotation_dist_fact, label = round(df$cl[breakPoints]))
    
    p
    

    
  }else if(write_table == T){
    
    cht_title <- gsub(":", "_",cht_title)
    place_title <- gsub(":","_", place_title)
    write.csv(cht_data, paste0("tables/", gsub(" ","_",cht_title), "_", gsub(" ","_",place_title,), ".csv"))
    
  }else{
    df
  }
}


format_SPC <- function(cht, df, r1_col, r2_col, ymin, ymax) {
  point_colours <- c("Rule 1" = r1_col, "Rule 2" = r2_col, "None" = "black")
  cht + 
    geom_line(colour = "black", size = 0.5) + 
    #geom_line(data = df, aes(x,cl, linetype = periodType), size = 0.75) +

    geom_line(data = mutate(df, cl = ifelse(periodType == "calculation", cl, NA)), aes(x,cl), size = 0.75, linetype = "solid") +
    geom_line(data = mutate(df, cl = ifelse(periodType == "display", cl, NA)), aes(x,cl), size = 0.75, linetype = "dashed") +
    geom_line(data = mutate(df, ucl = ifelse(periodType == "calculation", ucl, NA)), aes(x,ucl), size = 0.75, linetype = "solid") +
    geom_line(data = mutate(df, ucl = ifelse(periodType == "display", ucl, NA)), aes(x,ucl), size = 0.75, linetype = "dashed") +
    geom_line(data = mutate(df, lcl = ifelse(periodType == "calculation", lcl, NA)), aes(x,lcl), size = 0.75, linetype = "solid") +
    geom_line(data = mutate(df, lcl = ifelse(periodType == "display", lcl, NA)), aes(x,lcl), size = 0.75, linetype = "dashed") +
  
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

