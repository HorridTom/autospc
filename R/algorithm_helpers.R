# Function to determine whether there are enough data points left to form a new
# period
enough_data_for_new_period <- function(data,
                                       period_min,
                                       baseline_length,
                                       counter,
                                       chart_type){
  
  num_remaining_non_missing_data_points <- data %>%
    dplyr::filter(dplyr::row_number() >= counter) %>%
    dplyr::filter(!is.na(y)) %>%
    nrow()
  
  if(chart_type == "MR") {
    num_remaining_non_missing_data_points <-
      num_remaining_non_missing_data_points + 1L
  }
  
  if(counter == 1L & !is.null(baseline_length)) {
    
    enough_data <- num_remaining_non_missing_data_points >= baseline_length 
    
  } else {
    
    enough_data <- num_remaining_non_missing_data_points >= period_min
    
  }
  
  return(enough_data)
}


# Function to find most extreme points outside of control limits and return
# their positions
find_extremes <- function(data,
                          chart_type,
                          counter,
                          period_min,
                          max_exclusions,
                          centre_line_tolerance,
                          shift_rule_threshold,
                          mr_screen_max_loops){
  
  #initialise variables
  i <- 1
  exclusion_points <- NULL
  furthest_extremes <- NULL
  
  while(i <= max_exclusions){
    
    calculation_period <- data[counter:(counter + period_min - 1),]
    
    if(chart_type == "C"){
      limits_list <- get_c_limits(y = calculation_period$y,
                                  exclusion_points = exclusion_points)
      
    }else if(chart_type == "C'"){
      limits_list <- get_cp_limits(y = calculation_period$y,
                                   exclusion_points = exclusion_points,
                                   mr_screen_max_loops = mr_screen_max_loops)
      
    }else if(chart_type == "P"){
      limits_list <- get_p_limits(y = calculation_period$y_numerator,
                                  n = calculation_period$n,
                                  exclusion_points = exclusion_points,
                                  multiply = 100)
      
    }else if(chart_type == "P'"){
      limits_list <- get_pp_limits(y = calculation_period$y_numerator,
                                   n = calculation_period$n,
                                   exclusion_points = exclusion_points,
                                   multiply = 100,
                                   mr_screen_max_loops = mr_screen_max_loops)
      
    }else if(chart_type == "XMR"){
      limits_list <- get_i_limits(y = calculation_period$y,
                                  exclusion_points = exclusion_points,
                                  mr_screen_max_loops = mr_screen_max_loops)
      
    }else if(chart_type == "MR"){
      limits_list <- get_mr_limits(mr = calculation_period$y,
                                   exclusion_points = exclusion_points,
                                   mr_screen_max_loops = 0L)  
      calculation_period$y <- get_mr_limits(mr = calculation_period$y,
                                            exclusion_points = NULL,
                                            mr_screen_max_loops = 0L)$mr
    }
    
    calculation_period$cl <- limits_list$cl
    calculation_period$ucl <- limits_list$ucl
    calculation_period$lcl <- limits_list$lcl
    
    calculation_period <- calculation_period %>%
      dplyr::select(x,y,ucl,lcl, cl)
    
    calculation_period <- add_rule_breaks(
      calculation_period,
      centre_line_tolerance = centre_line_tolerance,
      shift_rule_threshold = shift_rule_threshold)
    calculation_period <- calculation_period %>% 
      dplyr::mutate(aboveCl = ifelse(y > cl,
                                     TRUE,
                                     ifelse(y < cl,
                                            FALSE,
                                            NA))) %>%
      dplyr::mutate(rule1Distance = ifelse(rule1 & aboveCl,
                                           y - ucl, 
                                           ifelse(rule1 & !aboveCl,
                                                  lcl - y,
                                                  NA))) %>%
      # Set already established extremes as NA
      dplyr::mutate(rule1Distance = ifelse(dplyr::row_number() %in% 
                                             exclusion_points,
                                           NA,
                                           rule1Distance))
    
    if(sum(!is.na(calculation_period$rule1Distance)) == 0) {
      # If no extremes, set furthest_extreme to -Inf
      furthest_extreme <- -Inf
    } else {
      # Otherwise, set furthest extreme to the greatest distance from limit
      furthest_extreme <- max(calculation_period$rule1Distance, na.rm = T)
    }
    exclusion_point <- which(
      calculation_period$rule1Distance == furthest_extreme)
    
    # Add next exclusion point and furthest extreme to the vectors
    furthest_extremes <- c(furthest_extremes, furthest_extreme)
    exclusion_points <- c(exclusion_points, exclusion_point)
    i = i + 1
  }
  
  # Check whether there are more than 3 exclusion points (due to points with the
  # same values)
  if(length(exclusion_points) > max_exclusions){
    exclusion_points <- exclusion_points[1:max_exclusions]
  }
  
  if(length(exclusion_points) == 0){
    NULL
  } else {
    exclusion_points
  }
  
}


# Function to scan to see where start of each rule 2 break is -
# returns list of these points 
rule2_break_start_positions <- function(limits_table, counter){
  # Add a column for start of rule 2 breaks - i.e. if there is a rule 2
  # highlight and that is not preceded by a rule 2 highlight
  limits_table <- limits_table %>% 
    dplyr::mutate(startOfRule2Break = rule2 & 
                    (rule2 != dplyr::lag(rule2) |
                       different_cl_side(aboveOrBelowCl,
                                         dplyr::lag(aboveOrBelowCl))))
  
  next_rule_break_positions <-
    (which(limits_table$startOfRule2Break[
      counter:nrow(limits_table)
    ] == T)) + counter - 1
  
  next_rule_break_positions
  
}

different_cl_side <- function(x, y) {
  return(x*y == -1)
}


# Function to identify whether there has been a rule break in the opposite
# direction in calc period returns TRUE for rule break in opposite direction
# within candidate calc period including hang over into display set counter to
# beginning of candidate limits
identify_opposite_break <- function(limits_table,
                                    counter,
                                    period_min, 
                                    triggering_rule_break_direction,
                                    centre_line_tolerance,
                                    shift_rule_threshold,
                                    overhanging_reversions = TRUE){
  
  # start rule breaks from candidate period so as not to include "overhanging"
  # rule breaks from prev period.
  # overhanging_reversions controls "overhanging" reversions into following
  # display period prevent re-establishment
  candidate_start <- counter
  if(overhanging_reversions) {
    candidate_end <- nrow(limits_table)
  } else {
    candidate_end <- counter + period_min - 1L
  }
  
  limits_table_candidate <- limits_table[candidate_start:candidate_end,]
  limits_table_candidate <- add_rule_breaks(
    limits_table_candidate,
    centre_line_tolerance = centre_line_tolerance,
    shift_rule_threshold = shift_rule_threshold)
  
  limits_table_candidate <- limits_table_candidate %>%
    dplyr::mutate(
      laggedAOBC = dplyr::lag(aboveOrBelowCl),
      newRun = dplyr::if_else((is.na(laggedAOBC) |
                                 (aboveOrBelowCl != 0 &
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
    dplyr::mutate(oppositeBreak = dplyr::if_else(
      rule2 & (aboveOrBelowCl != triggering_rule_break_direction) &
        runCount > 1, 
      TRUE, 
      FALSE))
  
  if(!overhanging_reversions & nrow(limits_table) > candidate_end) {
    
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
    
    next_rule_break_position <- min(
      which(limits_table_candidate$oppositeBreak == TRUE )) + counter - 1
    
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
                  newPeriod = dplyr::if_else(
                    (is.na(laggedPeriodType) |
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


# Function to add rule breaks to data with many periods. Avoids issues with
# highlighting across periods. NB this function counts actual break points, not
# period starts, hence it relies on the breakPoint column not being TRUE on the
# first row.
add_rule_breaks_respecting_periods <- function(limits_table,
                                               counter,
                                               centre_line_tolerance,
                                               shift_rule_threshold){
  
  #get breakpoint positions
  breakpoints <- which(limits_table$breakPoint)
  
  
  if(counter == 1 | length(breakpoints) == 0L) {
    #for first period, or cases where there is only one period
    
    #add rule breaks to all of data
    limits_table <- add_rule_breaks(
      x = limits_table,
      centre_line_tolerance = centre_line_tolerance,
      shift_rule_threshold = shift_rule_threshold)
    
  }else if(length(breakpoints) == 1){
    #for data with 2 periods
    
    #split data into sections
    limits_table_top <- limits_table[1:(counter-1),]
    limits_table_bottom <- limits_table[counter:nrow(limits_table),]
    
    #add rule breaks to the old and new periods separately 
    limits_table_top <- add_rule_breaks(
      x = limits_table_top,
      centre_line_tolerance = centre_line_tolerance,
      shift_rule_threshold = shift_rule_threshold)
    limits_table_bottom <- add_rule_breaks(
      x = limits_table_bottom,
      centre_line_tolerance = centre_line_tolerance,
      shift_rule_threshold = shift_rule_threshold)
    
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
    limits_table_mid <- add_rule_breaks(
      x = limits_table_mid,
      centre_line_tolerance = centre_line_tolerance,
      shift_rule_threshold = shift_rule_threshold)
    limits_table_bottom <- add_rule_breaks(
      x = limits_table_bottom,
      centre_line_tolerance = centre_line_tolerance,
      shift_rule_threshold = shift_rule_threshold)
    
    #put data back together
    limits_table <- dplyr::bind_rows(limits_table_top, 
                                     limits_table_mid,
                                     limits_table_bottom)
  }
  
  limits_table
}


# Helper function to establish whether all elements of a numeric vector are
# equal
is_numeric_vector_constant <- function(x) {
  diff(range(x)) < .Machine$double.eps ^ 0.5
}


# Helper function to fill in NA values with previous non-NA value
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


# Check whether a floating median is required, and add a column to df providing
# its values if so
floating_median_column <- function(df,
                                   floating_median,
                                   floating_median_n) {
  
  median_from_x <- df %>%
    dplyr::mutate(non_missing_y = !is.na(y)) %>%
    dplyr::arrange(dplyr::desc(x)) %>%
    dplyr::mutate(cumulative_num_non_missing = cumsum(non_missing_y)) %>%
    dplyr::filter(cumulative_num_non_missing == floating_median_n) %>%
    dplyr::pull(x) %>%
    max()
  
  addfloating_median <- switch(
    EXPR = floating_median,
    yes = TRUE,
    auto = any(df %>%
                 dplyr::filter(x >= median_from_x) %>% 
                 dplyr::pull(rule2)),
    FALSE)
  
  if(addfloating_median) {
    
    df <- df %>%
      dplyr::mutate(
        median =
          dplyr::if_else(x >= median_from_x,
                         median(df %>%
                                  dplyr::filter(x >= median_from_x) %>%
                                  dplyr::pull(y),
                                na.rm = TRUE),
                         NA))
    
  }
  
  return(df)
  
}


# Add floating median line to the plot p
add_floating_median <- function(df,
                                p,
                                floating_median_n) {
  
  p <- p +
    ggplot2::geom_line(data = df, 
                       ggplot2::aes(x, median),
                       linetype = "75551555",
                       colour = "gray50",
                       linewidth = 0.5,
                       show.legend = TRUE,
                       na.rm = TRUE) +
    ggplot2::annotate(
      "text",
      x = df %>%
        dplyr::filter(dplyr::row_number() == nrow(df) -
                        floating_median_n + 1L) %>%
        dplyr::pull(x),
      y = df %>%
        dplyr::filter(dplyr::row_number() == nrow(df) -
                        floating_median_n + 1L) %>%
        dplyr::pull(median)*0.95,
      label = "Median",
      size = 3,
      colour = "gray50",
      na.rm = TRUE)
  
  return(p)
  
}


sign_chr <- function(x) {
  y <- dplyr::case_when(
    x < 0 ~ "01",
    x == 0 ~ "00",
    x > 0 ~ "10"
  )
  
  return(y)
}


counter_at_rule_break <- function(df,
                                  counter,
                                  shift_rule_threshold) {
  
  if(!(df %>%
       dplyr::filter(dplyr::row_number() == counter) %>%
       dplyr::pull(rule2))) {
    
    return(FALSE)
    
  }
  
  start_of_next_run <- df %>%
    dplyr::mutate(rowNumber = dplyr::row_number()) %>%
    dplyr::filter(rowNumber >= counter,
                  runStart) %>%
    dplyr::slice_head(n = 1L) %>%
    dplyr::pull(rowNumber)
  
  if(length(start_of_next_run) == 0L) {
    start_of_next_run <- nrow(df) + 1L
  }
  
  result <- start_of_next_run - counter >= shift_rule_threshold
  
  return(result)
  
}

