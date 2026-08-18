# Function to form calculation limits for a period
# data has columns x and y
form_calculation_limits <- function(data,
                                    counter,
                                    chart){
  
  #force columns into the correct type
  if("y" %in% colnames(data)){
    data$y <- as.double(data$y)
  }
  if("n" %in% colnames(data)){
    data$n <- as.double(data$n)
  }
  
  if(counter == 1L & !is.null(chart$baseline_length)) {
    periodLength <- baseline_period_length(chart, data)
  } else {
    periodLength <- chart$period_min
  }
  
  exclusion_points <- find_extremes(
    data = data,
    chart = chart,
    counter = counter,
    period_length = periodLength)
  
  calculation_period <- data[counter:(counter + periodLength - 1),]
  
  # Calculation of limits excluding extremes for selected section of data
  limits_list <- calculate_limits(chart = chart,
                                  period = calculation_period,
                                  exclusion_points = exclusion_points)
  
  calculation_period$cl <- limits_list$cl
  calculation_period$ucl <- limits_list$ucl
  calculation_period$lcl <- limits_list$lcl
  
  extra_columns <- limits_table_columns(chart)

  calculation_period <- calculation_period %>%
    dplyr::select(x, y, ucl,lcl, cl) %>%
    dplyr::mutate(periodType = "calculation") %>%
    dplyr::mutate(excluded = ifelse(dplyr::row_number() %in% exclusion_points, T, F))
  
  
  # First period does not already have the additional columns
  if(counter == 1){
    
    # Joins limits to the existing data
    limits_table <- data %>%
      dplyr::left_join(calculation_period, by = "x") %>%
      dplyr::mutate(y = dplyr::if_else(is.na(y.y), y.x, y.y)) 
    
    limits_table <- limits_table %>%
      dplyr::select(x, y, dplyr::all_of(extra_columns), ucl, lcl, cl,
                    periodType, excluded,
                    dplyr::any_of("log"))
    # Add the breakPoint column to keep track of break points as they are
    # added. For compatibility with (at least)
    # add_rule_breaks_respecting_periods, the first point is not classed as a 
    # break point.
    limits_table <- limits_table %>%
      dplyr::mutate(breakPoint = dplyr::if_else(dplyr::row_number() == counter,
                                                NA,
                                                FALSE))
    
  } else {
    
    #joins limits to the existing data, overwriting display limits 
    limits_table <- data %>%
      dplyr::left_join(calculation_period, by = "x") %>%
      dplyr::mutate(y = dplyr::if_else(is.na(y.y), y.x, y.y)) %>%
      dplyr::mutate(ucl = dplyr::if_else(is.na(ucl.y), ucl.x, ucl.y)) %>%
      dplyr::mutate(lcl = dplyr::if_else(is.na(lcl.y), lcl.x, lcl.y)) %>%
      dplyr::mutate(cl = dplyr::if_else(is.na(cl.y), cl.x, cl.y)) %>%
      dplyr::mutate(periodType = dplyr::if_else(is.na(periodType.y), periodType.x, periodType.y)) %>%
      dplyr::mutate(excluded = dplyr::if_else(is.na(excluded.y), excluded.x, excluded.y)) 
    
    limits_table <- limits_table %>% 
      dplyr::mutate(breakPoint = (breakPoint |
                                    dplyr::row_number() == counter))
    
    limits_table <- limits_table %>%
      dplyr::select(x, y, dplyr::all_of(extra_columns), ucl, lcl, cl,
                    periodType, excluded,
                    dplyr::contains("breakPoint"),
                    dplyr::contains("rule"),
                    dplyr::contains("aboveOrBelow"),
                    dplyr::contains("highlight"),
                    dplyr::contains("run"),
                    dplyr::any_of("log"))
  }
  
  return(limits_table)
}


# Function to form display limits (period extension)
form_display_limits <- function(limits_table, counter, chart){
  
  if(counter > nrow(limits_table)) {
    # No display limits needed - no data beyond calculation period
    return(limits_table)
  }
  
  limits_table <- extend_display_limits(chart = chart,
                                        limits_table = limits_table,
                                        counter = counter)
  
  return(limits_table)
}


# Function to create limits for new calculation and display period with rule
# breaks
form_calculation_and_display_limits <- function(
    data, 
    counter_at_period_start, 
    chart){
  
  #form calculation limits for first period
  limits_table <- form_calculation_limits(
    data = data,
    counter = counter_at_period_start,
    chart = chart)
  
  
  #extend display limits to end 
  
  if(counter_at_period_start == 1L & !is.null(chart$baseline_length)) {
    periodLength <- baseline_period_length(chart, data)
  } else {
    periodLength <- chart$period_min
  }
  
  limits_table <- form_display_limits(limits_table = limits_table, 
                                      counter = counter_at_period_start +
                                        periodLength,
                                      chart = chart)
  
  #add rule breaks considering where periods are
  limits_table <- add_rule_breaks_respecting_periods(
    limits_table = limits_table, 
    counter = counter_at_period_start,
    centre_line_tolerance = chart$centre_line_tolerance,
    shift_rule_threshold = chart$shift_rule_threshold)
  
  return(limits_table)
}


extend_limits <- function(df,
                          chart,
                          extend_limits_to,
                          x_max) {
  
  if(!is.null(extend_limits_to)) {
    
    if(extend_limits_to <= x_max) {
      stop("Limits can only be extended to a point beyond the end of the data.")
    }
    
    last_calc_period <- df %>%
      dplyr::filter(periodType == "calculation") %>%
      dplyr::slice_tail(n = 1L) %>%
      dplyr::pull(plotPeriod)
    
    final_period <- df %>%
      dplyr::filter(plotPeriod == last_calc_period)
    
    ext_limits <- extrapolate_limits(chart = chart,
                                     period = final_period)
    
    df_ext_first_row <- df %>%
      dplyr::filter(dplyr::row_number() == max(dplyr::row_number())) %>% 
      dplyr::mutate(x = x_max + 1,
                    y = NA_real_,
                    cl = ext_limits$cl,
                    lcl = ext_limits$lcl,
                    ucl = ext_limits$ucl,
                    periodType = "display",
                    excluded = NA,
                    breakPoint = FALSE,
                    rule1 = FALSE,
                    rule2 = FALSE,
                    aboveOrBelowCl = 0,
                    highlight = "None")
    
    df_ext_last_row <- df %>%
      dplyr::filter(dplyr::row_number() == max(dplyr::row_number())) %>% 
      dplyr::mutate(x = extend_limits_to,
                    y = NA_real_,
                    cl = ext_limits$cl,
                    lcl = ext_limits$lcl,
                    ucl = ext_limits$ucl,
                    periodType = "display",
                    excluded = NA,
                    breakPoint = FALSE,
                    rule1 = FALSE,
                    rule2 = FALSE,
                    aboveOrBelowCl = 0,
                    highlight = "None")
    
    df <- df %>% 
      dplyr::bind_rows(df_ext_first_row,
                       df_ext_last_row)
  }
  
  # Re-derive plotPeriod to ensure consistent with extension period type display
  df <- df %>%
    dplyr::mutate(plotPeriod = paste0(periodType,
                                      periodStart))
  
  return(df)
}


