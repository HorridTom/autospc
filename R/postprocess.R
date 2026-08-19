# Postprocessing functions

# General postprocessing, required regardless of whether limits
# are to be displayed
postprocess <- function(
    df,
    chart_type = NULL,
    chart,
    override_x_title = NULL,
    override_y_title = NULL,
    override_y_lim = NULL,
    x_pad_end = NULL,
    extend_limits_to = NULL,
    xType
) {
  
  # Start and end dates
  if(!is.null(extend_limits_to) && is.null(x_pad_end)) {
    x_pad_end = extend_limits_to
  }
  start_x <- min(df$x, na.rm = TRUE)
  x_max <- max(df$x, na.rm = TRUE)
  end_x <- max(x_max, x_pad_end)
  
  # Chart y limit
  if(!centre_line_present(df)) {
    ylimlow <- min(df$y,
                   na.rm = TRUE)
    ylimhigh <- max(df$y,
                    na.rm = TRUE)
  } else {
    y_range <- y_axis_range(chart = chart,
                            data = df)
    ylimlow <- y_range$low
    ylimhigh <- y_range$high
  }
  
  #Override y limit if specified
  if(!is.null(override_y_lim)) {
    ylimhigh <- override_y_lim
  }
  
  # Ensure axis titles available
  ytitle <- y_axis_title(chart)
  
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
    chart,
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
                            chart = chart,
                            ylimhigh = ylimhigh,
                            align_labels = align_labels,
                            flip_labels = flip_labels,
                            upper_annotation_sf = upper_annotation_sf,
                            lower_annotation_sf = lower_annotation_sf,
                            annotation_arrow_curve = annotation_arrow_curve)
  
  # Extend display limits
  df <- extend_limits(df = df,
                      chart = chart,
                      extend_limits_to = extend_limits_to,
                      x_max = x_max)
  
  return(df)
  
}


#' Does this table carry a centre line?
#'
#' The algorithm returns a table with no `cl`, `ucl` or `lcl` when there were
#' too few points to form a period, so the presence of `cl` answers whether it
#' produced anything to draw.
#'
#' @return TRUE or FALSE
#' @noRd
centre_line_present <- function(data) {
  
  return("cl" %in% colnames(data))
  
}

