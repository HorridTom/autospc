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

plot_SPC <- function(df, 
                     r1_col = "orange", r2_col = "steelblue3", 
                     cht_title = "title",
                     place_title = "",
                     chart_typ = "C",
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
  
  #get limits
  df <- mutate(df, x = as.Date(x))
  df <- create_SPC_auto_limits_table(df)
  df <- df %>%
    mutate(date = as.Date(x)) %>%
    mutate(x = as.Date(x))
  

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
    
    annotation_dist_fact <- ifelse(chart_typ == "C" | chart_typ == "C'", override_annotation_dist, override_annotation_dist_P)
    caption <- paste(chart_typ,"Shewhart Chart.","\n*Shewhart chart rules apply (see Understanding the Analysis tab for more detail) \nRule 1: Any point outside the control limits \nRule 2: Eight or more consecutive points all above, or all below, the centre line")
    lines_caption <- ifelse(add_lines == T, "\n**hospitality re-opened, gatherings up to 30 allowed.", "")
    
    p <- format_SPC(pct, r1_col = r1_col, r2_col = r2_col) +
      scale_x_date(labels = date_format("%Y-%m-%d"), breaks = seq(st.dt, ed.dt, date_break),
                   limits = c(st.dt, ed.dt)) +
      ggtitle(cht_title, subtitle = place_title) +
      labs(x = "Day", y = ytitle,
           caption = paste0(caption, lines_caption),
           size = 10) +
      scale_y_continuous(limits = c(ylimlow, ylimhigh),
                         breaks = breaks_pretty(),
                         labels = number_format(accuracy = 1, big.mark = ",")) +
      annotate("text", x=st.dt, y=cl_start + cl_start/annotation_dist_fact, label = cl_start) +
      annotate("text", x=ed.dt, y=cl_end + cl_start/annotation_dist_fact, label = cl_end)
    
    p
    

    
  }else if(write_table == T){
    
    cht_title <- gsub(":", "_",cht_title)
    place_title <- gsub(":","_", place_title)
    write.csv(cht_data, paste0("tables/", gsub(" ","_",cht_title), "_", gsub(" ","_",place_title,), ".csv"))
    
  }else{
    cht_data
  }
}


format_SPC <- function(cht, r1_col, r2_col, ymin, ymax) {
  point_colours <- c("Rule 1" = r1_col, "Rule 2" = r2_col, "None" = "black")
  cht + 
    geom_line(colour = "black", size = 0.5) + 
    geom_line(aes(x,cl), size = 0.75) +
    geom_line(aes(x,ucl), size = 0.5) +
    geom_line(aes(x,lcl), size = 0.5) +
  
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

