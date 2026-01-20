create_spc_plot <- function(df,
                            p_mr = NA,
                            chart_type = NULL,
                            xType,
                            start_x,
                            end_x,
                            x_max,
                            ylimlow,
                            ylimhigh,
                            num_non_missing_y,
                            period_min = 21,
                            title = NULL,
                            subtitle = NULL,
                            use_caption = TRUE,
                            override_x_title = NULL,
                            override_y_title = NULL,
                            r1_col = "orange",
                            r2_col = "steelblue3",
                            point_size = 2,
                            line_width_sf = 1,
                            include_annotations = TRUE,
                            basic_annotations = FALSE,
                            annotation_size = 3,
                            annotation_arrows = FALSE,
                            annotation_curvature = 0.3,
                            floating_median_n = 12L,
                            show_mr = TRUE,
                            x_break = NULL,
                            x_date_format = "%Y-%m-%d",
                            split_rows = NULL) {
  
  df_long <- df %>%
    tidyr::pivot_longer(cols = c(y, cl, ucl, lcl),
                        names_to = "series",
                        values_to = "value")
  
  df_long <- df_long %>%
    dplyr::select(x,
                  series,
                  value,
                  everything())
  
  df_long <- add_limit_connectors(df_long)
  
  # Create initial plot object without formatting
  pct <- ggplot2::ggplot(df_long %>%
                           dplyr::filter(!is.na(value)),
                         ggplot2::aes(x = x,
                                      y = value))
  
  if(use_caption) {
    caption <- paste(chart_type,
                     "Shewhart Chart.",
                     "\n*Shewhart chart rules apply \nRule 1: Any point",
                     "outside the control limits \nRule 2: Eight or more",
                     "consecutive points all above, or all below, the centre",
                     "line")
    rule_title <- "Rule triggered*"
  } else {
    caption <- NULL
    rule_title <- "Rule triggered"
  }
  
  # Apply autospc formatting
  p <- format_SPC(pct,
                  df_long = df_long,
                  r1_col = r1_col,
                  r2_col = r2_col,
                  point_size = point_size,
                  rule_title = rule_title,
                  line_width_sf = line_width_sf) +
    ggplot2::ggtitle(title,
                     subtitle = subtitle) +
    ggplot2::labs(x = override_x_title,
                  y = override_y_title,
                  caption = paste0(caption)) +
    ggplot2::scale_y_continuous(limits = c(ylimlow, ylimhigh),
                                breaks = scales::breaks_pretty(),
                                labels = scales::label_number(big.mark = ","))
  
  # Add floating median to chart if needed
  if("median" %in% colnames(df)) {
    p <- add_floating_median(p = p,
                             df = df_long,
                             floating_median_n = floating_median_n)
  }
  
  # Add annotations to chart if needed
  if(include_annotations == TRUE){
    
    p <- add_annotations_to_plot(p = p,
                                 df = df_long,
                                 basic_annotations = basic_annotations,
                                 annotation_size = annotation_size,
                                 annotation_arrows = annotation_arrows,
                                 annotation_curvature = annotation_curvature)
  }
  
  # Format x-axis depending on x type
  p <- format_x_axis(p = p,
                     xType = xType,
                     x_break = x_break,
                     x_date_format = x_date_format,
                     start_x = start_x,
                     end_x = end_x)
  
  # Facet by stages if needed
  if(!is.null(split_rows)) {
    p <- p +
      ggplot2::facet_wrap(facets = ggplot2::vars(stage),
                          ncol = 1L)
    
  }
  
  # Combine X and MR charts if needed
  if((chart_type == "XMR") & show_mr) {
    p <- p + 
      ggplot2::labs(caption = NULL,
                    x = NULL) + 
      ggplot2::theme(axis.text.x = ggplot2::element_blank(), 
                     axis.ticks.x = ggplot2::element_blank())
    
    p_mr <- p_mr + 
      ggplot2::labs(caption = caption)
    
    legend <- cowplot::get_legend(p)
    
    p_no_legend <- p + 
      ggplot2::theme(legend.position = "none")
    p_mr_no_legend <- p_mr + 
      ggplot2::theme(legend.position = "none")
    
    p <- cowplot::plot_grid(
      cowplot::plot_grid(p_no_legend, p_mr_no_legend, 
                         ncol = 1, 
                         align = "v"),
      legend,
      ncol = 2,
      rel_widths = c(1, 0.2)
    )
    
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
    theme_autospc() +
    ggplot2::ggtitle(title,
                     subtitle = subtitle) +
    ggplot2::labs(x = override_x_title,
                  y = override_y_title) +
    ggplot2::scale_y_continuous(limits = c(ylimlow, ylimhigh),
                                breaks = scales::breaks_pretty(),
                                labels = scales::number_format(accuracy = 1,
                                                               big.mark = ","))
  return(time_series_plot)
  
}


format_SPC <- function(cht,
                       df_long,
                       r1_col,
                       r2_col,
                       point_size,
                       line_width_sf,
                       rule_title,
                       ymin,
                       ymax) {
  point_colours <- c("Rule 1" = r1_col,
                     "Rule 2" = r2_col, 
                     "None" = "black",
                     "Excluded from limits calculation" = "grey")
  
  line_colours <- c("Calculation" = "black",
                    "Display" = "grey50")
  
  # Prepare information on plot periods
  plot_periods <- df_long$plotPeriod
  
  first_display_period <- plot_periods[grep("display",
                                            plot_periods)[1]]
  first_calc_period <- plot_periods[1]
  
  list_of_plot_periods <- unique(plot_periods)
  
  linecolour_scale <- grepl("calculation",
                            list_of_plot_periods) %>%
    ifelse(line_colours["Calculation"],
           line_colours["Display"])
  
  names(linecolour_scale) <- list_of_plot_periods
  
  # Create spc plot components
  cht <- cht + 
    ggplot2::geom_line(data = . %>% dplyr::filter(
      series %in% c("cl", "ucl", "lcl")),
      ggplot2::aes(colour = plotPeriod,
                   linetype = series,
                   linewidth = series),
      na.rm = TRUE) + 
    ggplot2::geom_line(data = . %>% dplyr::filter(series %in% c("y")),
                       ggplot2::aes(linetype = series,
                                    linewidth = series),
                       show.legend = FALSE,
                       na.rm = TRUE) +
    ggplot2::scale_colour_manual(
      "Period Type",
      values = linecolour_scale,
      breaks = if(!is.na(first_display_period)) {
        c(first_calc_period,
          first_display_period)
      } else {
        c(first_calc_period)
      },
      labels = if(!is.na(first_display_period)) {
        c("Calculation", "Display")
      } else {
        c("Calculation")
      }
    ) +
    ggplot2::scale_linetype_manual(values = c("solid", "42", "42", "solid"),
                                   guide = "none") +
    ggplot2::scale_linewidth_manual(values =
                                      c(0.75, 0.5, 0.5, 0.5)*line_width_sf,
                                    guide = "none") +
    ggnewscale::new_scale_colour() +
    ggplot2::geom_point(data = . %>% dplyr::filter(series == "y"),
                        ggplot2::aes(colour = highlight),
                        size = point_size,
                        na.rm = TRUE) +
    ggplot2::scale_color_manual(rule_title,
                                values = point_colours) + 
    theme_autospc()
  
  return(cht)
}


theme_autospc <- function(){
  
  thm_aspc <- ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
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
  
  return(thm_aspc)
  
}


format_x_axis <- function(p,
                          xType,
                          x_break,
                          x_date_format,
                          start_x,
                          end_x) {
  
  if(any(xType == "Date")) {
    if(is.null(x_break)) {
      p <- p + 
        ggplot2::scale_x_date(labels = scales::date_format(x_date_format),
                              breaks = scales::breaks_pretty(),
                              limits = c(as.Date(start_x),
                                         as.Date(end_x)))
    } else {
      p <- p + 
        ggplot2::scale_x_date(labels = scales::date_format(x_date_format),
                              breaks = seq(as.Date(start_x),
                                           as.Date(end_x),
                                           x_break),
                              limits = c(as.Date(start_x),
                                         as.Date(end_x)))
    }
  } else if(any(xType == "integer")) {
    if(is.null(x_break)) {
      p <- p + 
        ggplot2::scale_x_continuous(breaks = scales::breaks_extended(),
                                    limits = c(start_x,
                                               end_x))
    } else {
      p <- p + 
        ggplot2::scale_x_continuous(breaks = seq(start_x,
                                                 end_x,
                                                 x_break),
                                    limits = c(start_x,
                                               end_x))
    }
  } else if(any(xType == "POSIXct")) {
    if(is.null(x_break)) {
      p <- p + 
        ggplot2::scale_x_datetime(breaks = scales::breaks_pretty(),
                                  limits = c(start_x, end_x))
    } else {
      if(any(class(x_break) != "difftime")) {
        rlang::abort(paste("Please specify x_break as a difftime object when",
                           "x is POSIXct."))
      }
      p <- p + 
        ggplot2::scale_x_datetime(breaks = seq(start_x, end_x, x_break),
                                  limits = c(start_x, end_x))
    }
  } else {
    if(is.null(x_break)) {
      p <- p + 
        ggplot2::scale_x_continuous(breaks = scales::breaks_extended(),
                                    limits = c(start_x, end_x))
    } else {
      p <- p + 
        ggplot2::scale_x_continuous(breaks = seq(start_x, end_x, x_break),
                                    limits = c(start_x, end_x))
    }
  }
  
  return(p)
  
}


add_limit_connectors <- function(df_long) {
  
  x_sequence <- df_long %>%
    dplyr::distinct(x) %>%
    dplyr::arrange(x) %>%
    dplyr::pull(x)
  
  display_periods <- df_long %>%
    dplyr::filter(periodType == "display") %>%
    dplyr::distinct(plotPeriod,
                    x) %>%
    dplyr::group_by(plotPeriod) %>%
    dplyr::summarise(x = dplyr::first(x)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(prev_x = x_sequence[which(x_sequence == x) - 1L]) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(df_long %>%
                       dplyr::distinct(x, series, value) %>%
                       dplyr::rename(prev_value = value),
                     by = c("prev_x" = "x")) 
  
  display_starts <- df_long %>%
    dplyr::inner_join(display_periods %>%
                        dplyr::select(-plotPeriod),
                      by = c("x" = "x",
                             "series" = "series")) %>%
    dplyr::filter(series %in% c("cl", "ucl", "lcl")) %>%
    dplyr::mutate(x = prev_x,
                  value = prev_value) %>%
    dplyr::select(-prev_x,
                  -prev_value)
  
  df_long <- df_long %>% 
    dplyr::bind_rows(display_starts)
  
  return(df_long)
  
}

