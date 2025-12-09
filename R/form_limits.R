# Function to form calculation limits for a period
# data has columns x and y
form_calculation_limits <- function(data,
                                    counter,
                                    period_min,
                                    baseline_length,
                                    chart_type = "C",
                                    max_exclusions = 3,
                                    centre_line_tolerance,
                                    shift_rule_threshold,
                                    mr_screen_max_loops){
  
  #force columns into the correct type
  if("y" %in% colnames(data)){
    data$y <- as.double(data$y)
  }
  if("n" %in% colnames(data)){
    data$n <- as.double(data$n)
  }
  
  if(counter == 1L & !is.null(baseline_length)) {
    periodLength <- baseline_length
  } else {
    periodLength <- period_min
  }
  
  exclusion_points <- find_extremes(
    data = data,
    chart_type = chart_type,
    counter = counter,
    period_min = periodLength,
    max_exclusions = max_exclusions,
    centre_line_tolerance = centre_line_tolerance,
    shift_rule_threshold = shift_rule_threshold,
    mr_screen_max_loops = mr_screen_max_loops)
  
  calculation_period <- data[counter:(counter + periodLength - 1),]
  
  # Calculation of limits excluding extremes for selected section of data
  if(chart_type == "C"){
    limits_list <- get_c_limits(y = calculation_period$y,
                                exclusion_points = exclusion_points)
  } else if(chart_type == "C'"){
    limits_list <- get_cp_limits(y = calculation_period$y,
                                 exclusion_points = exclusion_points,
                                 mr_screen_max_loops = mr_screen_max_loops)
    
  } else if(chart_type == "P"){
    limits_list <- get_p_limits(y = calculation_period$y_numerator,
                                n = calculation_period$n,
                                exclusion_points = exclusion_points,
                                multiply = 100)
    
  } else if(chart_type == "P'"){
    limits_list <- get_pp_limits(y = calculation_period$y_numerator,
                                 n = calculation_period$n,
                                 exclusion_points = exclusion_points,
                                 multiply = 100,
                                 mr_screen_max_loops = mr_screen_max_loops)
    
  } else if(chart_type == "XMR"){
    limits_list <- get_i_limits(y = calculation_period$y,
                                exclusion_points = exclusion_points,
                                mr_screen_max_loops = mr_screen_max_loops)
    
  } else if(chart_type == "MR"){
    limits_list <- get_mr_limits(mr = calculation_period$y,
                                 exclusion_points = exclusion_points,
                                 mr_screen_max_loops = 0L)  
  }
  
  calculation_period$cl <- limits_list$cl
  calculation_period$ucl <- limits_list$ucl
  calculation_period$lcl <- limits_list$lcl
  
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
    
    # Only selects n if P chart
    if(chart_type == "P" | chart_type == "P'"){
      limits_table <- limits_table %>%
        dplyr::select(x, y, n, y_numerator, ucl, lcl, cl, periodType, excluded,
                      dplyr::any_of("log"))
    } else {
      limits_table <- limits_table %>%
        dplyr::select(x, y, ucl, lcl, cl, periodType, excluded,
                      dplyr::any_of("log"))
    }
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
    
    #only selects n if P chart
    if(chart_type == "P" | chart_type == "P'"){
      limits_table <- limits_table %>%
        dplyr::select(x, y, n, y_numerator, ucl, lcl, cl, periodType, excluded, 
                      dplyr::contains("breakPoint"),
                      dplyr::contains("rule"),
                      dplyr::contains("aboveOrBelow"),
                      dplyr::contains("highlight"),
                      dplyr::contains("run"),
                      dplyr::any_of("log"))
    } else {
      limits_table <- limits_table %>%
        dplyr::select(x, y, ucl, lcl, cl, periodType, excluded, 
                      dplyr::contains("breakPoint"),
                      dplyr::contains("rule"),
                      dplyr::contains("aboveOrBelow"),
                      dplyr::contains("highlight"),
                      dplyr::contains("run"),
                      dplyr::any_of("log"))
    }
  }
  
  return(limits_table)
}


# Function to form display limits (period extension)
form_display_limits <- function(limits_table, counter, chart_type = "C'"){
  
  if(counter > nrow(limits_table)) {
    # No display limits needed - no data beyond calculation period
    return(limits_table)
  }
  
  if(chart_type == "C" | chart_type == "C'" | chart_type == "XMR" |
     chart_type == "MR"){
    limits_table[counter:nrow(limits_table), "ucl"] <-
      limits_table[(counter - 1), "ucl"]
    limits_table[counter:nrow(limits_table), "lcl"] <-
      limits_table[(counter - 1), "lcl"]
    limits_table[counter:nrow(limits_table), "cl"] <-
      limits_table[(counter - 1), "cl"]
    limits_table[counter:nrow(limits_table), "periodType"] <- "display"
    
  } else {
    #constant from P' chart calc = (UCL - CL)sqrt(n)
    constant <- (limits_table[(counter - 1), "ucl"] -
                   limits_table[(counter - 1), "cl"]) *
      sqrt(limits_table[(counter - 1), "n"])
    pbar <- limits_table[(counter - 1), "cl"]
    
    limits_table[counter:nrow(limits_table), "cl"] <- 
      limits_table[(counter - 1), "cl"]
    limits_table[counter:nrow(limits_table), "periodType"] <- "display"
    
    #splits limits table to just the section that we want
    limits_table_top <- limits_table[1:(counter - 1),]
    limits_table_bottom <- limits_table[counter:nrow(limits_table),]
    
    limits_table_bottom <- limits_table_bottom %>%
      dplyr::mutate(constant = as.numeric(constant)) %>%
      dplyr::mutate(pbar = as.numeric(pbar)) %>%
      dplyr::mutate(ucl_display = pbar + (constant/sqrt(n)) ) %>%
      dplyr::mutate(lcl_display = pbar - (constant/sqrt(n)) ) %>%
      dplyr::mutate(ucl = dplyr::if_else(periodType == "display",
                                         ucl_display,
                                         ucl)) %>%
      dplyr::mutate(lcl = dplyr::if_else(periodType == "display",
                                         lcl_display,
                                         lcl)) %>%
      dplyr::mutate(ucl = dplyr::if_else(ucl >= 100, 100, ucl)) %>%
      dplyr::mutate(lcl = dplyr::if_else(lcl <= 0, 0, lcl))
    
    limits_table <- dplyr::bind_rows(limits_table_top, limits_table_bottom)
    
    
  }
  
  return(limits_table)
}


# Function to create limits for new calculation and display period with rule
# breaks
form_calculation_and_display_limits <- function(
    data, 
    period_min,
    baseline_length,
    counter_at_period_start, 
    chart_type,
    max_exclusions,
    centre_line_tolerance,
    shift_rule_threshold,
    mr_screen_max_loops){
  
  #form calculation limits for first period
  limits_table <- form_calculation_limits(
    data = data,
    period_min = period_min,
    baseline_length = baseline_length,
    counter = counter_at_period_start,
    chart_type = chart_type,
    max_exclusions = max_exclusions,
    centre_line_tolerance = centre_line_tolerance,
    shift_rule_threshold = shift_rule_threshold,
    mr_screen_max_loops = mr_screen_max_loops)
  
  
  #extend display limits to end 
  
  if(counter_at_period_start == 1L & !is.null(baseline_length)) {
    periodLength <- baseline_length
  } else {
    periodLength <- period_min
  }
  
  limits_table <- form_display_limits(limits_table = limits_table, 
                                      counter = counter_at_period_start +
                                        periodLength,
                                      chart_type = chart_type)
  
  #add rule breaks considering where periods are
  limits_table <- add_rule_breaks_respecting_periods(
    limits_table = limits_table, 
    counter = counter_at_period_start,
    centre_line_tolerance = centre_line_tolerance,
    shift_rule_threshold = shift_rule_threshold)
  
  return(limits_table)
}


extend_limits <- function(df,
                          chart_type,
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
    
    # For extension, chart types whose limits may vary within a period due to
    # varying denominators use limit values derived from the mean denominator 
    # for the final period. Other chart types use the (constant) limits from the 
    # final period.
    switch(chart_type,
           P = {
             ext_calc_data <- df %>%
               dplyr::filter(plotPeriod == last_calc_period) %>%
               dplyr::mutate(y = (y/100)*n,
                             n = dplyr::if_else(is.na(n),
                                                NA_real_,
                                                mean(n,
                                                     na.rm = TRUE)))
             
             exclusion_points <- ext_calc_data %>%
               dplyr::pull(excluded) %>%
               which()
             
             ext_limits <- get_p_limits(y = ext_calc_data$y,
                                        n = ext_calc_data$n,
                                        exclusion_points = exclusion_points,
                                        multiply = 100) %>%
               lapply("[[", 1L)
             
           },
           `P'` = {
             ext_calc_data <- df %>%
               dplyr::filter(plotPeriod == last_calc_period) %>%
               dplyr::mutate(y = (y/100)*n)
             
             exclusion_points <- ext_calc_data %>%
               dplyr::pull(excluded) %>%
               which()
             
             ext_limits <- get_pp_limits(y = ext_calc_data$y,
                                         n = ext_calc_data$n,
                                         exclusion_points = exclusion_points,
                                         multiply = 100,
                                         use_nbar_for_stdev = TRUE) %>% 
               lapply("[[", 1L)
           },
           {
             ext_limits <- df %>% 
               dplyr::filter(plotPeriod == last_calc_period) %>% 
               dplyr::select(cl, lcl, ucl) %>% 
               dplyr::summarise(dplyr::across(dplyr::everything(),
                                              ~ mean(.x,
                                                     na.rm = TRUE))) %>%
               as.list()
           }
    )
    
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


