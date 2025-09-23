create_spc_plot <- function(df,
                            p_mr = NA,
                            chartType = NULL,
                            xType,
                            start_x,
                            end_x,
                            x_max,
                            ylimlow,
                            ylimhigh,
                            num_non_missing_y,
                            periodMin = 21,
                            title = NULL,
                            subtitle = NULL,
                            use_caption = TRUE,
                            override_x_title = NULL,
                            override_y_title = NULL,
                            r1_col = "orange",
                            r2_col = "steelblue3",
                            point_size = 2,
                            line_width_sf = 1,
                            includeAnnotations = TRUE,
                            annotation_size = 3,
                            annotation_arrows = FALSE,
                            annotation_curvature = 0.3,
                            floatingMedian_n = 12L,
                            showMR = TRUE,
                            x_break = NULL,
                            x_date_format = "%Y-%m-%d",
                            split_rows = NULL) {
  
  # Create initial plot object without formatting
  pct <- ggplot2::ggplot(df %>% dplyr::filter(!is.na(y)),
                         ggplot2::aes(x,y))
  
  if(use_caption) {
    caption <- paste(chartType,
                     "Shewhart Chart.",
                     "\n*Shewhart chart rules apply \nRule 1: Any point",
                     "outside the control limits \nRule 2: Eight or more",
                     "consecutive points all above, or all below, the centre",
                     "line")
  } else {
    caption <- NULL
  }
  
  p <- format_SPC(pct,
                  df = df,
                  r1_col = r1_col,
                  r2_col = r2_col,
                  point_size = point_size,
                  line_width_sf = line_width_sf) +
    ggplot2::ggtitle(title,
                     subtitle = subtitle) +
    ggplot2::labs(x = override_x_title,
                  y = override_y_title,
                  caption = paste0(caption),
                  size = 10) +
    ggplot2::scale_y_continuous(limits = c(ylimlow, ylimhigh),
                                breaks = scales::breaks_pretty(),
                                labels = scales::label_number(big.mark = ","))
  
  if("median" %in% colnames(df)) {
    p <- add_floating_median(p = p,
                             df = df,
                             floatingMedian_n = floatingMedian_n)
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
      x_break <- as.numeric(difftime(as.Date(end_x),
                                     as.Date(start_x),
                                     units = "days")) / 40
    }
    
    p <- p + ggplot2::scale_x_date(labels = scales::date_format(x_date_format),
                                   breaks = seq(as.Date(start_x),
                                                as.Date(end_x),
                                                x_break),
                                   limits = c(as.Date(start_x),
                                              as.Date(end_x))
    )
    
  } else if(xType == "integer") {
    # get x axis breaks
    if(is.null(x_break)) {
      x_break <- (end_x - start_x) / 40
    }
    
    p <- p + ggplot2::scale_x_continuous(breaks = seq(start_x, end_x, 10),
                                         limits = c(start_x, end_x))
  } else {
    # get x axis breaks
    if(is.null(x_break)) {
      x_break <- (end_x - start_x) / 40
    }
    
    p <- p + ggplot2::scale_x_continuous(breaks = seq(start_x, end_x, x_break),
                                         limits = c(start_x, end_x))
  }
  
  if(!is.null(split_rows)) {
    p <- p +
      ggplot2::facet_wrap(facets = ggplot2::vars(stage),
                          ncol = 1L)
    
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
  
  return(p)
  
}


create_timeseries_plot <- function(df,
                                   title,
                                   subtitle,
                                   override_x_title,
                                   override_y_title,
                                   ylimlow,
                                   ylimhigh,
                                   point_size,
                                   line_width_sf) {
  
  time_series_plot <- ggplot2::ggplot(df, 
                                      ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(colour = "black",
                       linewidth = 0.5*line_width_sf) +
    ggplot2::geom_point(colour = "black", size = point_size) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(
                     colour = "grey80"
                     ),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 45,
                                                       hjust = 1,
                                                       vjust = 1.0,
                                                       size = 14),
                   axis.text.y = ggplot2::element_text(size = 14),
                   axis.title = ggplot2::element_text(size = 14),
                   plot.title = ggplot2::element_text(size = 20,
                                                      hjust = 0),
                   plot.subtitle = ggplot2::element_text(size = 16,
                                                         face = "italic"),
                   axis.line = ggplot2::element_line(colour = "grey60"),
                   plot.caption = ggplot2::element_text(size = 10,
                                                        hjust = 0.5)) +
    ggplot2::ggtitle(title,
                     subtitle = subtitle) +
    ggplot2::labs(x = override_x_title,
                  y = override_y_title,
                  size = 10) +
    ggplot2::scale_y_continuous(limits = c(ylimlow, ylimhigh),
                                breaks = scales::breaks_pretty(),
                                labels = scales::number_format(accuracy = 1,
                                                               big.mark = ","))
  return(time_series_plot)
  
}


format_SPC <- function(cht,
                       df,
                       r1_col,
                       r2_col,
                       point_size,
                       line_width_sf,
                       ymin,
                       ymax) {
  point_colours <- c("Rule 1" = r1_col,
                     "Rule 2" = r2_col, 
                     "None" = "black",
                     "Excluded from limits calculation" = "grey")
  
  #get exemplar calculation and display periods
  plot_periods <- df$plotPeriod
  
  first_display_period <- plot_periods[grep("display",
                                            plot_periods)[1]]
  first_calc_period <- plot_periods[1]
  
  suppressWarnings( # to avoid the warning about using alpha for discrete vars
    cht + 
      ggplot2::geom_line(colour = "black",
                         linewidth = 0.5*line_width_sf,
                         na.rm = TRUE) + 
      ggplot2::geom_line(data = df, 
                         ggplot2::aes(x,cl,
                                      alpha = plotPeriod),
                         linetype = "solid",
                         linewidth = 0.75*line_width_sf,
                         na.rm = TRUE) +
      ggplot2::geom_line(data = df, 
                         ggplot2::aes(x,lcl,
                                      alpha = plotPeriod),
                         linetype = "42",
                         linewidth = 0.5*line_width_sf,
                         show.legend = FALSE,
                         na.rm = TRUE) +
      ggplot2::geom_line(data = df, 
                         ggplot2::aes(x,ucl,
                                      alpha = plotPeriod),
                         linetype = "42",
                         linewidth = 0.5*line_width_sf,
                         show.legend = FALSE,
                         na.rm = TRUE) +
      ggplot2::geom_point(ggplot2::aes(colour = highlight),
                          size = point_size,
                          na.rm = TRUE) +
      ggplot2::scale_color_manual("Rule triggered*",
                                  values = point_colours) + 
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
                                      override.aes = list(
                                        alpha = if(
                                          !is.na(first_display_period)
                                          ) {
                                        c(1, 0.4)
                                      } else {
                                        c(1)
                                      }
                                      )
                                    )
      ) +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                     panel.grid.major.x = ggplot2::element_line(
                       colour = "grey80"
                       ),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(angle = 45,
                                                         hjust = 1,
                                                         vjust = 1.0,
                                                         size = 14),
                     axis.text.y = ggplot2::element_text(size = 14),
                     axis.title = ggplot2::element_text(size = 14),
                     plot.title = ggplot2::element_text(size = 20,
                                                        hjust = 0),
                     plot.subtitle = ggplot2::element_text(size = 16,
                                                           face = "italic"),
                     axis.line = ggplot2::element_line(colour = "grey60"),
                     plot.caption = ggplot2::element_text(size = 10,
                                                          hjust = 0.5)) 
  )
}

