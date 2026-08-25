#' Draw an SPC chart, or an XmR pair
#'
#' The chart is drawn from the first frame. An XmR pair is drawn as two panels
#' stacked by `cowplot::plot_grid()`, the moving range one drawn here from the
#' second frame.
#'
#' @param charts The analysed chart, or the pair, or one per facet, in a list.
#' @param frames The drawable frames, as `drawable_frame()` gives them: one per
#'   chart for a pair, and one for a faceted plot, holding every facet.
#' @param passed The presentation parameters, with the axis titles resolved.
#' @param split_rows Non-NULL to facet by stage.
#'
#' @return A ggplot.
#' @noRd
create_spc_plot <- function(charts,
                            frames,
                            passed,
                            split_rows = NULL) {

  data    <- frames[[1]]$data
  derived <- frames[[1]]$derived

  pair <- is_xmr_pair(charts)

  chart_type <- if(pair) "XMR" else chart_type_label(charts[[1]])

  df_long <- data %>%
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
  
  if(passed$use_caption) {
    caption <- paste(chart_type,
                     "Shewhart Chart.",
                     "\n*Shewhart chart rules apply",
                     "\nRule 1: Any point outside the control limits", 
                     paste( 
                       "\nRule 2:",
                       word_for_number(charts[[1]]$shift_rule_threshold),
                       "or more consecutive points all above, or all below, the centre line"
                       )
    )
    rule_title <- "Rule triggered*"
  } else {
    caption <- NULL
    rule_title <- "Rule triggered"
  }
  
  # Apply autospc formatting
  p <- format_SPC(pct,
                  df_long = df_long,
                  r1_col = passed$r1_col,
                  r2_col = passed$r2_col,
                  point_size = passed$point_size,
                  rule_title = rule_title,
                  line_width_sf = passed$line_width_sf) +
    ggplot2::ggtitle(passed$title,
                     subtitle = passed$subtitle) +
    ggplot2::labs(x = passed$override_x_title,
                  y = passed$override_y_title,
                  caption = paste0(caption)) +
    ggplot2::scale_y_continuous(limits = c(derived$ylimlow, derived$ylimhigh),
                                breaks = scales::breaks_pretty(),
                                labels = scales::label_number(big.mark = ","))
  
  # Add floating median to chart if needed
  if("median" %in% colnames(data)) {
    p <- add_floating_median(p = p,
                             df = df_long,
                             floating_median_n = charts[[1]]$floating_median_n)
  }
  
  # Add annotations to chart if needed
  if(passed$include_annotations == TRUE){

    p <- add_annotations_to_plot(
      p = p,
      df = df_long,
      basic_annotations = passed$basic_annotations,
      annotation_size = passed$annotation_size,
      annotation_arrows = passed$annotation_arrows,
      annotation_arrow_curve = passed$annotation_arrow_curve)
  }
  
  # Format x-axis depending on x type
  p <- format_x_axis(p = p,
                     xType = class(data$x),
                     x_break = passed$x_break,
                     x_date_format = passed$x_date_format,
                     start_x = derived$start_x,
                     end_x = derived$end_x)
  
  # Facet by stages if needed
  if(!is.null(split_rows)) {
    p <- p +
      ggplot2::facet_wrap(facets = ggplot2::vars(stage),
                          ncol = 1L)
    
  }
  
  # Combine X and MR charts if needed
  if(pair) {
    p <- p + 
      ggplot2::labs(caption = NULL,
                    x = NULL) + 
      ggplot2::theme(axis.text.x = ggplot2::element_blank(), 
                     axis.ticks.x = ggplot2::element_blank())

    p_mr <- draw_mr_panel(frame = frames[[2]],
                          passed = passed) +
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


#' Draw the moving range panel of an XmR pair
#'
#' The panel carries no title or subtitle, and the axis titles are the moving
#' range chart's own. Called by `create_spc_plot()` for a pair.
#'
#' @param frame The moving range chart's drawable frame.
#' @param passed The presentation parameters, shared with the X chart.
#'
#' @return A ggplot.
#' @noRd
draw_mr_panel <- function(frame,
                          passed) {

  passed["title"]            <- list(NULL)
  passed["subtitle"]         <- list(NULL)
  passed["override_x_title"] <- list(frame$axis_titles$x)
  passed["override_y_title"] <- list(frame$axis_titles$y)

  if(!centre_line_present(frame$data)) {

    return(create_timeseries_plot(data = frame$data,
                                  passed = passed,
                                  derived = frame$derived))

  }

  return(create_spc_plot(charts = list(frame$chart),
                         frames = list(frame),
                         passed = passed))

}


#' Draw a series with no limits
#'
#' What is drawn when the algorithm produced no calculation period, or when the
#' caller asked for no limits.
#'
#' @return A ggplot.
#' @noRd
create_timeseries_plot <- function(data,
                                   passed,
                                   derived) {

  time_series_plot <- ggplot2::ggplot(data, 
                                      ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(colour = "black",
                       linewidth = 0.5*passed$line_width_sf) +
    ggplot2::geom_point(colour = "black", size = passed$point_size) +
    theme_autospc() +
    ggplot2::ggtitle(passed$title,
                     subtitle = passed$subtitle) +
    ggplot2::labs(x = passed$override_x_title,
                  y = passed$override_y_title) +
    ggplot2::scale_y_continuous(limits = c(derived$ylimlow, derived$ylimhigh),
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

word_for_number <- function(n) {
  stopifnot(
    length(n) == 1,
    is.numeric(n),
    n == as.integer(n),
    n > 0
  )
  
  words <- c(
    "One", "Two", "Three", "Four", "Five",
    "Six", "Seven", "Eight", "Nine"
  )
  
  if (n >= 1 && n <= 5) {
    as.character(n)
  } else if (n >= 6 && n <= 9) {
    words[n]
  } else {
    as.character(n)
  }
}


add_limit_connectors <- function(df_long) {
  
  x_sequence <- df_long %>%
    dplyr::distinct(x) %>%
    dplyr::arrange(x) %>%
    dplyr::pull(x)
  
  # Dataframe listing each display period in the data, with information on
  # first x value in period, and previous x value to that, along with series
  # values for that previous point
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
  
  # Create additional rows to be added into df_long with series values at the 
  # point immediately before the start of each display period. This has the
  # effect of creating an additional point for the control limits and centre
  # line to connect with the preceding calculation period limits and centre line
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
  
  # Add the extra rows into the data, and sort into x order
  df_long <- df_long %>% 
    dplyr::bind_rows(display_starts) %>%
    dplyr::arrange(x,
                   series)
  
  return(df_long)
  
}
