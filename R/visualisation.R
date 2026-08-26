#' Draw an SPC chart, or an XmR pair
#'
#' An XmR pair is drawn as two panels stacked by `cowplot::plot_grid()`, the
#' moving range one drawn here from the dispersion chart's plot data.
#'
#' `main` below is the plot data the plot is drawn from: the location chart of a
#' pair, the only chart of a single chart plot, or every facet at once of a
#' faceted one. It carries the chart it came from.
#'
#' @param plot_data The plot data to draw, as `build_plot_data()` gives it: a
#' chart object, plus other information needed for plotting. One
#' element per chart for a pair, and just one for a faceted plot, holding every
#' facet.
#' @param visualisation_params The visualisation parameters, with the axis
#'   titles resolved.
#' @param split_rows Non-NULL to facet by stage.
#'
#' @return A ggplot.
#' @noRd
create_spc_plot <- function(plot_data,
                            visualisation_params,
                            split_rows = NULL) {

  main <- plot_data[[1]]

  chart   <- main$chart
  table    <- main$table
  axis_extents <- main$axis_extents

  pair <- is_xmr_pair(lapply(plot_data, function(each) each$chart))

  chart_type <- if(pair) "XMR" else chart_type_label(chart)

  long_table <- table %>%
    tidyr::pivot_longer(cols = c(y, cl, ucl, lcl),
                        names_to = "series",
                        values_to = "value")
  
  long_table <- long_table %>%
    dplyr::select(x,
                  series,
                  value,
                  everything())
  
  long_table <- add_limit_connectors(long_table)
  
  # Create initial plot object without formatting
  plot_unformatted <- ggplot2::ggplot(long_table %>%
                                        dplyr::filter(!is.na(value)),
                                      ggplot2::aes(x = x,
                                                   y = value))
  
  if(visualisation_params$use_caption) {
    caption <- paste(chart_type,
                     "Shewhart Chart.",
                     "\n*Shewhart chart rules apply",
                     "\nRule 1: Any point outside the control limits", 
                     paste( 
                       "\nRule 2:",
                       word_for_number(chart$shift_rule_threshold),
                       "or more consecutive points all above, or all below, the centre line"
                       )
    )
    rule_title <- "Rule triggered*"
  } else {
    caption <- NULL
    rule_title <- "Rule triggered"
  }
  
  # Apply autospc formatting
  spc_plot <- format_spc_plot(
    plot_unformatted,
    long_table = long_table,
    r1_col = visualisation_params$r1_col,
    r2_col = visualisation_params$r2_col,
    point_size = visualisation_params$point_size,
    rule_title = rule_title,
    line_width_sf = visualisation_params$line_width_sf) +
    ggplot2::ggtitle(visualisation_params$title,
                     subtitle = visualisation_params$subtitle) +
    ggplot2::labs(x = visualisation_params$override_x_title,
                  y = visualisation_params$override_y_title,
                  caption = paste0(caption)) +
    ggplot2::scale_y_continuous(limits = c(axis_extents$ylimlow,
                                           axis_extents$ylimhigh),
                                breaks = scales::breaks_pretty(),
                                labels = scales::label_number(big.mark = ","))
  
  # Add floating median to chart if needed
  if("median" %in% colnames(table)) {
    spc_plot <- add_floating_median(spc_plot = spc_plot,
                                    table = long_table,
                                    floating_median_n = chart$floating_median_n)
  }
  
  # Add annotations to chart if needed
  if(visualisation_params$include_annotations == TRUE){

    spc_plot <- add_annotations_to_plot(
      spc_plot = spc_plot,
      table = long_table,
      basic_annotations = visualisation_params$basic_annotations,
      annotation_size = visualisation_params$annotation_size,
      annotation_arrows = visualisation_params$annotation_arrows,
      annotation_arrow_curve = visualisation_params$annotation_arrow_curve)
  }
  
  # Format x-axis depending on x type
  spc_plot <- format_x_axis(spc_plot = spc_plot,
                            x_class = class(table$x),
                            x_break = visualisation_params$x_break,
                            x_date_format = visualisation_params$x_date_format,
                            start_x = axis_extents$start_x,
                            end_x = axis_extents$end_x)
  
  # Facet by stages if needed
  if(!is.null(split_rows)) {
    spc_plot <- spc_plot +
      ggplot2::facet_wrap(facets = ggplot2::vars(stage),
                          ncol = 1L)
    
  }
  
  # Combine X and MR charts if needed
  if(pair) {
    spc_plot <- spc_plot + 
      ggplot2::labs(caption = NULL,
                    x = NULL) + 
      ggplot2::theme(axis.text.x = ggplot2::element_blank(), 
                     axis.ticks.x = ggplot2::element_blank())

    p_mr <- draw_mr_panel(plot_data = plot_data$dispersion,
                          visualisation_params = visualisation_params) +
      ggplot2::labs(caption = caption)
    
    legend <- cowplot::get_legend(spc_plot)
    
    spc_plot_no_legend <- spc_plot + 
      ggplot2::theme(legend.position = "none")
    p_mr_no_legend <- p_mr + 
      ggplot2::theme(legend.position = "none")
    
    spc_plot <- cowplot::plot_grid(
      cowplot::plot_grid(spc_plot_no_legend, p_mr_no_legend, 
                         ncol = 1, 
                         align = "v"),
      legend,
      ncol = 2,
      rel_widths = c(1, 0.2)
    )
    
  }
  
  return(spc_plot)
  
}


#' Draw the moving range panel of an XmR pair
#'
#' The panel carries no title or subtitle, and the axis titles are the moving
#' range chart's own. Called by `create_spc_plot()` for a pair.
#'
#' @param plot_data The moving range chart's plot data.
#' @param visualisation_params The visualisation parameters, shared with the X
#'   chart.
#'
#' @return A ggplot.
#' @noRd
draw_mr_panel <- function(plot_data,
                          visualisation_params) {

  visualisation_params["title"]            <- list(NULL)
  visualisation_params["subtitle"]         <- list(NULL)
  visualisation_params["override_x_title"] <- list(plot_data$axis_titles$x)
  visualisation_params["override_y_title"] <- list(plot_data$axis_titles$y)

  if(!centre_line_present(plot_data$table)) {

    return(create_timeseries_plot(table = plot_data$table,
                                  visualisation_params = visualisation_params,
                                  axis_extents = plot_data$axis_extents))

  }

  return(create_spc_plot(plot_data = list(plot_data),
                         visualisation_params = visualisation_params))

}


#' Draw a series with no limits
#'
#' What is drawn when the algorithm produced no calculation period, or when the
#' caller asked for no limits.
#'
#' @return A ggplot.
#' @noRd
create_timeseries_plot <- function(table,
                                   visualisation_params,
                                   axis_extents) {

  time_series_plot <- ggplot2::ggplot(table, 
                                      ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(colour = "black",
                       linewidth = 0.5*visualisation_params$line_width_sf) +
    ggplot2::geom_point(colour = "black",
                        size = visualisation_params$point_size) +
    theme_autospc() +
    ggplot2::ggtitle(visualisation_params$title,
                     subtitle = visualisation_params$subtitle) +
    ggplot2::labs(x = visualisation_params$override_x_title,
                  y = visualisation_params$override_y_title) +
    ggplot2::scale_y_continuous(limits = c(axis_extents$ylimlow,
                                           axis_extents$ylimhigh),
                                breaks = scales::breaks_pretty(),
                                labels = scales::number_format(accuracy = 1,
                                                               big.mark = ","))
  return(time_series_plot)
  
}


format_spc_plot <- function(plot_unformatted,
                            long_table,
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
  plot_periods <- long_table$plot_period
  
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
  plot_unformatted <- plot_unformatted + 
    ggplot2::geom_line(data = . %>% dplyr::filter(
      series %in% c("cl", "ucl", "lcl")),
      ggplot2::aes(colour = plot_period,
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
  
  return(plot_unformatted)
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


format_x_axis <- function(spc_plot,
                          x_class,
                          x_break,
                          x_date_format,
                          start_x,
                          end_x) {
  
  if(any(x_class == "Date")) {
    if(is.null(x_break)) {
      spc_plot <- spc_plot + 
        ggplot2::scale_x_date(labels = scales::date_format(x_date_format),
                              breaks = scales::breaks_pretty(),
                              limits = c(as.Date(start_x),
                                         as.Date(end_x)))
    } else {
      spc_plot <- spc_plot + 
        ggplot2::scale_x_date(labels = scales::date_format(x_date_format),
                              breaks = seq(as.Date(start_x),
                                           as.Date(end_x),
                                           x_break),
                              limits = c(as.Date(start_x),
                                         as.Date(end_x)))
    }
  } else if(any(x_class == "integer")) {
    if(is.null(x_break)) {
      spc_plot <- spc_plot + 
        ggplot2::scale_x_continuous(breaks = scales::breaks_extended(),
                                    limits = c(start_x,
                                               end_x))
    } else {
      spc_plot <- spc_plot + 
        ggplot2::scale_x_continuous(breaks = seq(start_x,
                                                 end_x,
                                                 x_break),
                                    limits = c(start_x,
                                               end_x))
    }
  } else if(any(x_class == "POSIXct")) {
    if(is.null(x_break)) {
      spc_plot <- spc_plot + 
        ggplot2::scale_x_datetime(breaks = scales::breaks_pretty(),
                                  limits = c(start_x, end_x))
    } else {
      if(any(class(x_break) != "difftime")) {
        rlang::abort(paste("Please specify x_break as a difftime object when",
                           "x is POSIXct."))
      }
      spc_plot <- spc_plot + 
        ggplot2::scale_x_datetime(breaks = seq(start_x, end_x, x_break),
                                  limits = c(start_x, end_x))
    }
  } else {
    if(is.null(x_break)) {
      spc_plot <- spc_plot + 
        ggplot2::scale_x_continuous(breaks = scales::breaks_extended(),
                                    limits = c(start_x, end_x))
    } else {
      spc_plot <- spc_plot + 
        ggplot2::scale_x_continuous(breaks = seq(start_x, end_x, x_break),
                                    limits = c(start_x, end_x))
    }
  }
  
  return(spc_plot)
  
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


add_limit_connectors <- function(long_table) {
  
  x_sequence <- long_table %>%
    dplyr::distinct(x) %>%
    dplyr::arrange(x) %>%
    dplyr::pull(x)
  
  # Dataframe listing each display period in the data, with information on
  # first x value in period, and previous x value to that, along with series
  # values for that previous point
  display_periods <- long_table %>%
    dplyr::filter(period_type == "display") %>%
    dplyr::distinct(plot_period,
                    x) %>%
    dplyr::group_by(plot_period) %>%
    dplyr::summarise(x = dplyr::first(x)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(prev_x = x_sequence[which(x_sequence == x) - 1L]) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(long_table %>%
                       dplyr::distinct(x, series, value) %>%
                       dplyr::rename(prev_value = value),
                     by = c("prev_x" = "x")) 
  
  # Create additional rows to be added into long_table, with series values at
  # the point immediately before the start of each display period. This has the
  # effect of creating an additional point for the control limits and centre
  # line to connect with the preceding calculation period limits and centre line
  display_starts <- long_table %>%
    dplyr::inner_join(display_periods %>%
                        dplyr::select(-plot_period),
                      by = c("x" = "x",
                             "series" = "series")) %>%
    dplyr::filter(series %in% c("cl", "ucl", "lcl")) %>%
    dplyr::mutate(x = prev_x,
                  value = prev_value) %>%
    dplyr::select(-prev_x,
                  -prev_value)
  
  # Add the extra rows into the data, and sort into x order
  long_table <- long_table %>% 
    dplyr::bind_rows(display_starts) %>%
    dplyr::arrange(x,
                   series)
  
  return(long_table)
  
}
