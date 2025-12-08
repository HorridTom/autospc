# Postprocessing functions

# General postprocessing, required regardless of whether limits
# are to be displayed
postprocess <- function(
    df,
    chart_type = NULL,
    period_min = 21,
    show_limits = TRUE,
    override_x_title = NULL,
    override_y_title = NULL,
    override_y_lim = NULL,
    x_pad_end = NULL,
    extend_limits_to = NULL,
    xType
) {
  
  num_non_missing_y <- df %>%
    dplyr::filter(!is.na(y)) %>%
    nrow()
  
  if(chart_type == "MR") {
    num_non_missing_y <- num_non_missing_y + 1L
  }
  
  # Start and end dates
  if(!is.null(extend_limits_to) && is.null(x_pad_end)) {
    x_pad_end = extend_limits_to
  }
  start_x <- min(df$x, na.rm = TRUE)
  x_max <- max(df$x, na.rm = TRUE)
  end_x <- max(x_max, x_pad_end)
  
  # Chart y limit
  if(num_non_missing_y < period_min) {
    ylimlow <- min(df$y,
                   na.rm = TRUE)
  } else if(chart_type != "XMR") {
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
  
  if(num_non_missing_y < period_min) {
    ylimhigh <- max(df$y,
                    na.rm = TRUE)
  } else if(chart_type == "C" | chart_type == "C'") {
    ylimhigh <- max(df$ucl,
                    df$y,
                    na.rm = TRUE) + max(df$ucl,
                                        na.rm = TRUE)/10 + 10
  } else if (chart_type == "XMR" | chart_type == "MR") {
    ylimhigh <- max(df$ucl,
                    df$y,
                    na.rm = TRUE)*1.1
  } else {
    ylimhigh <- 110
  }
  
  #Override y limit if specified
  if(!is.null(override_y_lim)) {
    ylimhigh <- override_y_lim
  }
  
  # Ensure axis titles available
  ytitle <- switch(chart_type,
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
  
  # Convert x column back to date if necessary
  if(any(xType == "Date")) { 
    df <- df %>%
      dplyr::mutate(x = as.Date(x))
  }
  
  return(list(
    df = df,
    override_x_title = override_x_title,
    override_y_title = override_y_title,
    num_non_missing_y = num_non_missing_y,
    start_x = start_x,
    x_max = x_max,
    end_x = end_x,
    ylimhigh = ylimhigh,
    ylimlow = ylimlow
  ))
}

# Additional postprocessing, required if limits are to 
# be displayed
postprocess_spc <- function(
    df,
    chart_type,
    highlight_exclusions,
    floating_median,
    floating_median_n,
    extend_limits_to,
    align_labels,
    flip_labels,
    upper_annotation_sf,
    lower_annotation_sf,
    annotation_arrow_curve,
    ylimhigh,
    x_max
) {
  
  df <- df %>%
    dplyr::mutate(limitChange = ifelse(
      periodType == dplyr::lag(periodType),
      FALSE,
      TRUE))
  
  # ??NEEDED?? Store break points as vector
  breakPoints <- which(df$breakPoint)
  
  if(highlight_exclusions) {
    # Show exclusions on chart
    df <- df %>% dplyr::mutate(
      highlight = ifelse(excluded & !is.na(excluded),
                         "Excluded from limits calculation",
                         highlight)
    )
  }
  
  # add floating median column if needed
  df <- floating_median_column(df = df,
                               floating_median = floating_median,
                               floating_median_n = floating_median_n)
  
  # add annotation information
  df <- add_annotation_data(df = df,
                            chart_type = chart_type,
                            ylimhigh = ylimhigh,
                            align_labels = align_labels,
                            flip_labels = flip_labels,
                            upper_annotation_sf = upper_annotation_sf,
                            lower_annotation_sf = lower_annotation_sf,
                            annotation_arrow_curve = annotation_arrow_curve)
  
  # Get periods into groups for plotting
  df <- df %>%
    dplyr::mutate(
      periodStart = dplyr::if_else(limitChange == TRUE |
                                     is.na(limitChange) |
                                     breakPoint == TRUE,
                                   dplyr::row_number(),
                                   NA_integer_))
  
  df$periodStart <- fill_NA(df$periodStart)
  
  df <- df %>%
    dplyr::mutate(plotPeriod = paste0(periodType,
                                      periodStart))
  
  # Extend display limits
  df <- extend_limits(df = df,
                      chart_type = chart_type,
                      extend_limits_to = extend_limits_to,
                      x_max = x_max)
  
  return(df)
  
}


