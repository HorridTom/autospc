#function to determine whether there are enough data points left to form a new period
enough_data_for_new_period <- function(data, periodMin, counter){
   
  if((nrow(data) - counter + 1) >= periodMin){
    TRUE
  }else{
    FALSE
  }
}

#function to form calculation limits for a period
#data has columns x and y
form_calculation_limits <- function(data,
                                    counter,
                                    periodMin,
                                    chartType = "C",
                                    maxNoOfExclusions = 3,
                                    rule2Tolerance,
                                    runRuleLength,
                                    mr_screen_max_loops){
  
  #force columns into the correct type
  if("y" %in% colnames(data)){
    data$y <- as.double(data$y)
  }
  if("n" %in% colnames(data)){
    data$n <- as.double(data$n)
  }
  
  exclusion_points <- find_extremes(data,
                                    chartType,
                                    counter,
                                    periodMin,
                                    maxNoOfExclusions,
                                    rule2Tolerance,
                                    runRuleLength = runRuleLength,
                                    mr_screen_max_loops = mr_screen_max_loops)
  
  calculation_period <- data[counter:(counter + periodMin - 1),]
  
  #run the calculation of limits excluding extremes for selected section of data
  if(chartType == "C"){
    limits_list <- get_c_limits(y = calculation_period$y, exclusion_points = exclusion_points)
    
  }else if(chartType == "C'"){
    limits_list <- get_cp_limits(y = calculation_period$y, exclusion_points = exclusion_points, mr_screen_max_loops = mr_screen_max_loops)
    
  }else if(chartType == "P"){
    limits_list <- get_p_limits(y = calculation_period$y_numerator, n = calculation_period$n, exclusion_points = exclusion_points, multiply = 100)
    
  }else if(chartType == "P'"){
    limits_list <- get_pp_limits(y = calculation_period$y_numerator, n = calculation_period$n, exclusion_points = exclusion_points, multiply = 100, mr_screen_max_loops = mr_screen_max_loops)
    
  }else if(chartType == "XMR"){
    limits_list <- get_i_limits(y = calculation_period$y, exclusion_points = exclusion_points, mr_screen_max_loops = mr_screen_max_loops)
    
  }else if(chartType == "MR"){
    limits_list <- get_mr_limits(mr = calculation_period$y, exclusion_points = exclusion_points, mr_screen_max_loops = 0L)  
    
  }
  
  calculation_period$cl <- limits_list$cl
  calculation_period$ucl <- limits_list$ucl
  calculation_period$lcl <- limits_list$lcl

  calculation_period <- calculation_period %>%
    dplyr::select(x, y, ucl,lcl, cl) %>%
    dplyr::mutate(periodType = "calculation") %>%
    dplyr::mutate(excluded = ifelse(dplyr::row_number() %in% exclusion_points, T, F))


  #first period does not already have the additional columns
  if(counter == 1){
    
    #joins limits to the existing data
    limits_table <- data %>%
      dplyr::left_join(calculation_period, by = "x") %>%
      dplyr::mutate(y = dplyr::if_else(is.na(y.y), y.x, y.y)) 
    
    #only selects n if P chart
    if(chartType == "P" | chartType == "P'"){
      limits_table <- limits_table %>%
        dplyr::select(x, y, n, y_numerator, ucl, lcl, cl, periodType, excluded)
    }else{
      limits_table <- limits_table %>%
        dplyr::select(x, y, ucl, lcl, cl, periodType, excluded)
    }
    
  }else{
    
    #joins limits to the existing data, overwriting display limits 
    limits_table <- data %>%
      dplyr::left_join(calculation_period, by = "x") %>%
      dplyr::mutate(y = dplyr::if_else(is.na(y.y), y.x, y.y)) %>%
      dplyr::mutate(ucl = dplyr::if_else(is.na(ucl.y), ucl.x, ucl.y)) %>%
      dplyr::mutate(lcl = dplyr::if_else(is.na(lcl.y), lcl.x, lcl.y)) %>%
      dplyr::mutate(cl = dplyr::if_else(is.na(cl.y), cl.x, cl.y)) %>%
      dplyr::mutate(periodType = dplyr::if_else(is.na(periodType.y), periodType.x, periodType.y)) %>%
      dplyr::mutate(excluded = dplyr::if_else(is.na(excluded.y), excluded.x, excluded.y)) 
    
    #only selects n if P chart
    if(chartType == "P" | chartType == "P'"){
      limits_table <- limits_table %>%
        dplyr::select(x, y, n, y_numerator, ucl, lcl, cl, periodType, excluded, contains("highlight"), 
                      contains("breakPoint"))
    }else{
      limits_table <- limits_table %>%
        dplyr::select(x, y, ucl, lcl, cl, periodType, excluded, contains("highlight"), 
                      contains("breakPoint"))
    }
  }
  
}



#function to find most extreme points outside of control limits and return their positions
find_extremes <- function(data,
                          chartType,
                          counter,
                          periodMin,
                          maxNoOfExclusions,
                          rule2Tolerance,
                          runRuleLength,
                          mr_screen_max_loops){
  
  #initialise variables
  i <- 1
  exclusion_points <- NULL
  furthest_extremes <- NULL
  
  while(i <= maxNoOfExclusions){
    
    calculation_period <- data[counter:(counter + periodMin - 1),]
    
    if(chartType == "C"){
      limits_list <- get_c_limits(y = calculation_period$y, exclusion_points = exclusion_points)
      
    }else if(chartType == "C'"){
      limits_list <- get_cp_limits(y = calculation_period$y, exclusion_points = exclusion_points, mr_screen_max_loops = mr_screen_max_loops)
      
    }else if(chartType == "P"){
      limits_list <- get_p_limits(y = calculation_period$y_numerator, n = calculation_period$n, exclusion_points = exclusion_points, multiply = 100)
      
    }else if(chartType == "P'"){
      limits_list <- get_pp_limits(y = calculation_period$y_numerator, n = calculation_period$n, exclusion_points = exclusion_points, multiply = 100, mr_screen_max_loops = mr_screen_max_loops)
    
    }else if(chartType == "XMR"){
      limits_list <- get_i_limits(y = calculation_period$y, exclusion_points = exclusion_points, mr_screen_max_loops = mr_screen_max_loops)
      
    }else if(chartType == "MR"){
      limits_list <- get_mr_limits(mr = calculation_period$y, exclusion_points = exclusion_points, mr_screen_max_loops = 0L)  
      calculation_period$y <- get_mr_limits(mr = calculation_period$y,
                                            exclusion_points = NULL,
                                            mr_screen_max_loops = 0L)$mr
    }
    
    calculation_period$cl <- limits_list$cl
    calculation_period$ucl <- limits_list$ucl
    calculation_period$lcl <- limits_list$lcl
    
    calculation_period <- calculation_period %>%
      dplyr::select(x,y,ucl,lcl, cl)
    
    calculation_period <- add_rule_breaks(calculation_period,
                                          rule2Tolerance = rule2Tolerance,
                                          runRuleLength = runRuleLength)
    calculation_period <- calculation_period %>% 
      dplyr::mutate(aboveCl = ifelse(y > cl, TRUE,ifelse(y < cl, FALSE, NA))) %>%
      dplyr::mutate(rule1Distance = ifelse(rule1 & aboveCl, y - ucl, 
                                    ifelse(rule1 & !aboveCl, lcl - y, NA))) %>%
      #set already established extremes as NA
      dplyr::mutate(rule1Distance = ifelse(dplyr::row_number() %in% 
                                             exclusion_points, NA, rule1Distance))
    
    if(sum(!is.na(calculation_period$rule1Distance)) == 0) {
      # If no extremes, set furthest_extreme to -Inf
      furthest_extreme <- -Inf
    } else {
      # Otherwise, set furthest extreme to the greatest distance from limit
      furthest_extreme <- max(calculation_period$rule1Distance, na.rm = T)
    }
    exclusion_point <- which(calculation_period$rule1Distance == furthest_extreme)
    
    #add next exclusion point and furthest extreme to the vectors
    furthest_extremes <- c(furthest_extremes, furthest_extreme)
    exclusion_points <- c(exclusion_points, exclusion_point)
    i = i + 1
  }

  #check whether there are more than 3 exclusion points (due to points with the same values)
  if(length(exclusion_points) > maxNoOfExclusions){
    exclusion_points <- exclusion_points[1:maxNoOfExclusions]
  }
  
  if(length(exclusion_points) == 0){
    NULL
  }else{
    exclusion_points
  }
  
}


#function to form display limits (period extension)
form_display_limits <- function(limits_table, counter, chartType = "C'"){
  
  if(counter > nrow(limits_table)) {
    # No display limits needed - no data beyond calculation period
    return(limits_table)
  }
  
  if(chartType == "C" | chartType == "C'" | chartType == "XMR" | chartType == "MR"){
    limits_table[counter:nrow(limits_table), "ucl"] <- limits_table[(counter - 1), "ucl"]
    limits_table[counter:nrow(limits_table), "lcl"] <- limits_table[(counter - 1), "lcl"]
    limits_table[counter:nrow(limits_table), "cl"] <- limits_table[(counter - 1), "cl"]
    limits_table[counter:nrow(limits_table), "periodType"] <- "display"
    
  }else{
    #constant from P' chart calc = (UCL - CL)sqrt(n)
    constant <- (limits_table[(counter - 1), "ucl"] - limits_table[(counter - 1), "cl"]) * sqrt(limits_table[(counter - 1), "n"])
    pbar <- limits_table[(counter - 1), "cl"]
    
    limits_table[counter:nrow(limits_table), "cl"] <- limits_table[(counter - 1), "cl"]
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

  limits_table
  
}


#function to scan to see where start of each rule 2 break is - returns list of these points 
rule2_break_start_positions <- function(limits_table, counter){
  #add a column for start of rule 2 breaks
  #Flags if there is a rule 2 highlight and that is not preceded by a rule 2 highlight 
  limits_table <- limits_table %>% 
    dplyr::mutate(startOfRule2Break = ifelse(rule2 & rule2 != dplyr::lag(rule2), T, F))
  next_rule_break_positions <- (which(limits_table$startOfRule2Break[counter:nrow(limits_table)] == T)) + counter - 1
  
  next_rule_break_positions
  
}


#function to identify whether there has been a rule break in the opposite direction in calc period
#returns TRUE for rule break in opposite direction within candidate calc period including hang over into display
#set counter to beginning of candidate limits 
identify_opposite_break <- function(limits_table,
                                    counter,
                                    periodMin, 
                                    triggering_rule_break_direction,
                                    rule2Tolerance,
                                    runRuleLength,
                                    overhangingReversions = TRUE){
  
  # start rule breaks from candidate period as not to include "hang over" rule
  # breaks from prev period
  # overhangingReversions controls whether to include "hang over" into following
  # display period
  
  candidate_start <- counter
  if(overhangingReversions) {
    candidate_end <- nrow(limits_table)
  } else {
    candidate_end <- counter + periodMin - 1L
  }
  
  limits_table_candidate <- limits_table[candidate_start:candidate_end,]
  limits_table_candidate <- add_rule_breaks(limits_table_candidate,
                                            rule2Tolerance = rule2Tolerance,
                                            runRuleLength = runRuleLength)

  limits_table_candidate <- limits_table_candidate %>%
    dplyr::mutate(laggedAOBC = dplyr::lag(aboveOrBelowCl),
                  newRun = dplyr::if_else((is.na(laggedAOBC) | (aboveOrBelowCl != 0 &
                                            aboveOrBelowCl != laggedAOBC)),
                                          TRUE,
                                          FALSE),
                  runCount = cumsum(newRun))

  #looks for a rule break in the opposite direction within the candidate period
  # Don't consider the first run as a potential opposite rule break. If it is
  # in the same direction as the triggering run, it can't be an opposite break,
  # and if it is in the opposite direction, it just represents a transition on
  # the way to the new level
  limits_table_candidate <- limits_table_candidate %>% 
    dplyr::mutate(oppositeBreak = dplyr::if_else(rule2 & (aboveOrBelowCl != triggering_rule_break_direction) & runCount > 1, 
                                                 TRUE, 
                                                 FALSE))
  
  if(!overhangingReversions & nrow(limits_table) > candidate_end) {
    
    limits_table_tail <- limits_table[(candidate_end + 1L):nrow(limits_table),]
    limits_table_tail <- limits_table_tail %>% 
      dplyr::mutate(oppositeBreak = FALSE)
    
    limits_table_candidate <- limits_table_candidate %>%
      dplyr::bind_rows(limits_table_tail)
  }
  
  #return list containing: boolean of whether there is an opposite break, 
  #the next rule break position if applicable,
  #the candidate table
  if(all(limits_table_candidate$oppositeBreak == FALSE)){
    #if there are no further rule breaks
    output <- list(FALSE, NA, limits_table_candidate)
    
  }else{
    
    next_rule_break_position <- min(which(limits_table_candidate$oppositeBreak == TRUE )) + counter - 1
    
    last_point_in_calc_period <- tail(
      which(limits_table_candidate$periodType == "calculation"),
      n = 1L) + counter - 1
    
    if(next_rule_break_position > last_point_in_calc_period){
      #No rule break in opposite direction
      output <- list(FALSE, NA, limits_table_candidate)
    }else{
      output <- list(TRUE, next_rule_break_position, limits_table_candidate)
    }
  }

  output
}


# Function to establish whether the final run in the candidate calculation
# period prevents the recalculation (for no regrets)
final_run_of_calc_period_prevents_recalc <- function(
  candidate_limits_table,
  triggering_rule_break_direction) {
  
  # Filter data to exclude everything prior to the last calculation period 
  data <- candidate_limits_table
  data <- data %>%
    dplyr::mutate(laggedPeriodType = dplyr::lag(periodType),
      newPeriod = dplyr::if_else((is.na(laggedPeriodType) |
                            laggedPeriodType != periodType), TRUE, FALSE),
      periodCount = cumsum(newPeriod)
      )
  period_table <- data %>%
    dplyr::distinct(periodType, periodCount)
  
  last_calc_period <- period_table %>%
    dplyr::filter(periodType == "calculation") %>%
    dplyr::pull(periodCount) %>%
    max()
  
  data <- data %>%
    dplyr::filter(periodCount >= last_calc_period)

  #handles NA value that appears sometimes at the end of the data 
  if(is.na(data$y[nrow(data)])){
    data <- data[1:(nrow(data) - 1),]
  }
  
  # identify the row number of the last point, in the last calculation period,
  # that is not on the centre line
  last_point_in_last_calc_period <- tail(
    which(data$periodType == "calculation" &
            data$aboveOrBelowCl != 0),
    n = 1L)
  
  if(length(last_point_in_last_calc_period) != 1L) {
    # all the points in the last calculation period are on the centre line
    return(FALSE)
  }
  
  final_direction <- data[last_point_in_last_calc_period,
                          "aboveOrBelowCl"]
  
  if(final_direction == triggering_rule_break_direction) {
    # the last point in the final calculation period is in the same direction
    # as the triggering run, and therefore there is no potential for a rule-
    # breaking run in the opposite direction spanning the end of the last
    # calculation period
    return(FALSE)
  } else {
    # the last point in the final calculation period is in the opposite
    # direction to the triggering rule break
    
    # is the final run of the final calculation period the final run overall?
    final_calc_run_is_final_run <- data %>%
      dplyr::filter(dplyr::row_number() >= last_point_in_last_calc_period,
                    aboveOrBelowCl != 0) %>%
      dplyr::pull(aboveOrBelowCl) %>%
      is_numeric_vector_constant()
    
    if(final_calc_run_is_final_run) {
      # The final run in the final calculation period is also the final run
      # in the data. There are two cases: either a) it is a rule breaking
      # run, or b) it is not. In either case, there is *at least* potential for
      # a rbr in the opposite direction.
      return(TRUE)
    } else {
      # The final run in the final calculation period is not the final run in
      # the data. There are two cases: either a) it is a rbr, b) it is not.
      # (a) in this case, identify_opposite_break will identify it and prevent
      # the recalculation at the triggering rule break.
      # (b) in this case, there is no reason to prevent the recalculation
      return(FALSE)
    }
    
  }
  
}


#function to create limits for new calculation and display period with rule breaks
form_calculation_and_display_limits <- function(data, 
                                                periodMin, 
                                                counter_at_period_start, 
                                                chartType,
                                                maxNoOfExclusions,
                                                rule2Tolerance,
                                                runRuleLength,
                                                mr_screen_max_loops){
  
  #form calculation limits for first period
  limits_table <- form_calculation_limits(data = data, periodMin = periodMin,
                                          counter = counter_at_period_start, chartType = chartType,
                                          maxNoOfExclusions  = maxNoOfExclusions,
                                          rule2Tolerance = rule2Tolerance,
                                          runRuleLength = runRuleLength,
                                          mr_screen_max_loops = mr_screen_max_loops)
  
  
  #extend display limits to end 
  limits_table <- form_display_limits(limits_table = limits_table, 
                                      counter = counter_at_period_start + periodMin,
                                      chartType = chartType)
  
  #add rule breaks considering where periods are
  limits_table <- add_rule_breaks_respecting_periods(limits_table = limits_table, 
                                               counter = counter_at_period_start,
                                               rule2Tolerance = rule2Tolerance,
                                               runRuleLength = runRuleLength)
  
  
  limits_table
}


#function to add rule breaks to data with many periods
#Avoids issues faces with highlighting across periods
add_rule_breaks_respecting_periods <- function(limits_table,
                                         counter,
                                         rule2Tolerance,
                                         runRuleLength){
  
  #add a column to show where the breakpoints are
  limits_table <- limits_table %>% 
    dplyr::mutate(breakPoint = ifelse(cl == dplyr::lag(cl), FALSE, TRUE))
  
  #get breakpoint positions
  breakpoints <- which(limits_table$breakPoint)
  
  
  if(counter == 1 | length(breakpoints) == 0L) {
    #for first period, or cases where there is only one period
    
    #add rule breaks to all of data
    limits_table <- add_rule_breaks(x = limits_table,
                                    rule2Tolerance = rule2Tolerance,
                                    runRuleLength = runRuleLength)
    
  }else if(length(breakpoints) == 1){
    #for data with 2 periods
    
    #split data into sections
    limits_table_top <- limits_table[1:(counter-1),]
    limits_table_bottom <- limits_table[counter:nrow(limits_table),]
    
    #add rule breaks to the old and new periods separately 
    limits_table_top <- add_rule_breaks(x = limits_table_top,
                                        rule2Tolerance = rule2Tolerance,
                                        runRuleLength = runRuleLength)
    limits_table_bottom <- add_rule_breaks(x = limits_table_bottom,
                                           rule2Tolerance = rule2Tolerance,
                                           runRuleLength = runRuleLength)
    
    #put data back together
    limits_table <- dplyr::bind_rows(limits_table_top, 
                                     limits_table_bottom)
    
  }else if(length(breakpoints) >= 2){
    #for data with 3 or more periods, 
    #only re-run rule breaks on most recent 2 periods
    
    #find start of previous period
    no_of_breakpoints <- length(breakpoints)
    penultimate_breakpoint <- breakpoints[no_of_breakpoints - 1L]
    
    #split data into sections
    limits_table_top <- limits_table[1:(penultimate_breakpoint - 1L),]
    limits_table_mid <- limits_table[penultimate_breakpoint:(counter - 1L),]
    limits_table_bottom <- limits_table[counter:nrow(limits_table),]
    
    #add rule breaks to the penultimate and new periods only
    limits_table_mid <- add_rule_breaks(x = limits_table_mid,
                                        rule2Tolerance = rule2Tolerance,
                                        runRuleLength = runRuleLength)
    limits_table_bottom <- add_rule_breaks(x = limits_table_bottom,
                                           rule2Tolerance = rule2Tolerance,
                                           runRuleLength = runRuleLength)
    
    #put data back together
    limits_table <- dplyr::bind_rows(limits_table_top, 
                                     limits_table_mid,
                                     limits_table_bottom)
  }
  
  limits_table
}




#function to rename columns
rename_columns <- function(df, x, y, n, b) {
  
  data_colnames <- colnames(df)
  
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  n <- rlang::enquo(n)
  b <- rlang::enquo(b)
  
  # Rename columns to standard names
  if(!rlang::quo_is_missing(x)) {
    if("x" %in% data_colnames) {
      warning("x is present in the data and specified as an argument.
The column specified in the argument x will be used.")
    }
    df <- df %>% dplyr::rename(x = !!x)
  }
  
  if(!rlang::quo_is_missing(y)) {
    if("y" %in% data_colnames) {
      warning("y is present in the data and specified as an argument.
The column specified in the argument y will be used.")
    }
    df <- df %>% dplyr::rename(y = !!y)
  }
  
  if(!rlang::quo_is_missing(n)) {
    if("n" %in% data_colnames) {
      warning("n is present in the data and specified as an argument.
The column specified in the argument n will be used.")
    }
    df <- df %>% dplyr::rename(n = !!n)
  }
  
  if(!rlang::quo_is_missing(b)) {
    if("b" %in% data_colnames) {
      warning("b is present in the data and specified as an argument.
The column specified in the argument b will be used.")
    }
    df <- df %>% dplyr::rename(b = !!b)
  }
  
  return(df)
  
}


# helper function to establish whether all elements of a numeric vector are
# equal
is_numeric_vector_constant <- function(x) {
  diff(range(x)) < .Machine$double.eps ^ 0.5
}


#helper function to fill in NA values with previous non-NA value
#used for line aestetics
fill_NA <- function(x) {
  which.na <- c(which(!is.na(x)), length(x) + 1)
  values <- na.omit(x)
  
  if (which.na[1] != 1) {
    which.na <- c(1, which.na)
    values <- c(values[1], values)
  }
  
  diffs <- diff(which.na)
  return(rep(values, times = diffs))
}