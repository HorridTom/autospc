library(tidyverse)
library(Rfast)

#function to determine whether there are enough data points left to form a new period
enough_data_for_new_period <- function(data, periodMin, counter){
   
  if((nrow(data) - counter) >= periodMin){
    TRUE
  }else{
    FALSE
  }
}

#function to form calculation limits for a period
#data has columns x and y
form_calculation_limits <- function(data, counter, periodMin, cht_type = "C", maxNoOfExclusions = 3){
  
  exclusion_points <- find_extremes(data, cht_type, counter, periodMin, maxNoOfExclusions)
  
  #run the calculation of limits excluding extremes for selected section of data
  if(cht_type == "C"){
    calculation_period <- qicharts2::qic(x, y, data = data[counter:(counter + periodMin),]
                                         , chart = 'c', exclude = exclusion_points)
  }else if(cht_type == "C'"){
    calculation_period <- qicharts2::qic(x, y, n = rep(1, nrow(data[counter:(counter + periodMin),])), 
                                         data = data[counter:(counter + periodMin),]
                                         , chart = 'up', exclude = exclusion_points)
  }else if(cht_type == "P"){
    calculation_period <- qicharts2::qic(x, y = b, n = n, data = data[counter:(counter + periodMin),], 
                                         chart = 'p', multiply = 100, exclude = exclusion_points)
  }else if(cht_type == "P'"){
    calculation_period <- qicharts2::qic(x, y = b, n = n, data = data[counter:(counter + periodMin),], 
                                         chart = 'pp', multiply = 100, exclude = exclusion_points)
  }
  

  calculation_period <- calculation_period$data %>%
    select(x, y, ucl,lcl, cl) %>%
    mutate(periodType = "calculation") %>%
    mutate(excluded = ifelse(row_number() %in% exclusion_points, T, F))


  #first period does not already have the additional columns
  if(counter == 0){
    
    #joins limits to the existing data
    limits_table <- data %>%
      left_join(calculation_period, by = "x") %>%
      mutate(y = if_else(is.na(y.y), y.x, y.y)) 
    
    #only selects n if P chart
    if(cht_type == "P" | cht_type == "P'"){
      limits_table <- limits_table %>%
        select(x, y, n, b, ucl, lcl, cl, periodType, excluded)
    }else{
      limits_table <- limits_table %>%
      select(x, y, ucl, lcl, cl, periodType, excluded)
    }
    
  }else{
    
    #joins limits to the existing data, overwriting display limits 
    limits_table <- data %>%
      left_join(calculation_period, by = "x") %>%
      mutate(y = if_else(is.na(y.y), y.x, y.y)) %>%
      mutate(ucl = if_else(is.na(ucl.y), ucl.x, ucl.y)) %>%
      mutate(lcl = if_else(is.na(lcl.y), lcl.x, lcl.y)) %>%
      mutate(cl = if_else(is.na(cl.y), cl.x, cl.y)) %>%
      mutate(periodType = if_else(is.na(periodType.y), periodType.x, periodType.y)) %>%
      mutate(excluded = if_else(is.na(excluded.y), excluded.x, excluded.y)) 
    
    #only selects n if P chart
    if(cht_type == "P" | cht_type == "P'"){
      limits_table <- limits_table %>%
        select(x, y, n, b, ucl, lcl, cl, periodType, excluded)
    }else{
      limits_table <- limits_table %>%
        select(x, y, ucl, lcl, cl, periodType, excluded)
    }
  }
  
}



#function to find most extreme points outside of control limits and return their positions
find_extremes <- function(data, cht_type, counter, periodMin, maxNoOfExclusions){
  
  #initialise variables
  i <- 1
  exclusion_points <- NULL
  furthest_extremes <- NULL
  
  while(i <= maxNoOfExclusions){
    if(cht_type == "C"){
      calculation_period <- qicharts2::qic(x, y, data = data[counter:(counter + periodMin),],
                                           chart = 'c', exclude = exclusion_points)
    }else if(cht_type == "C'"){
      calculation_period <- qicharts2::qic(x, y, n = rep(1, nrow(data[counter:(counter + periodMin),])), 
                                           data = data[counter:(counter + periodMin),],
                                           chart = 'up', exclude = exclusion_points)
    }else if(cht_type == "P"){
      calculation_period <- qicharts2::qic(x, y = b, n, data = data[counter:(counter + periodMin),], 
                                           chart = 'p', multiply = 100, exclude = exclusion_points)
    }else if(cht_type == "P'"){
      calculation_period <- qicharts2::qic(x, y = b, n, data = data[counter:(counter + periodMin),], 
                                           chart = 'pp', multiply = 100, exclude = exclusion_points)
    }
    
    calculation_period <- calculation_period$data %>%
      select(x,y,ucl,lcl, cl)
    
    calculation_period <- add_rule_breaks(calculation_period)
    calculation_period <- calculation_period %>% 
      mutate(aboveCl = ifelse(y > cl, T,ifelse(y < cl, F, NA))) %>%
      mutate(rule1Distance = ifelse(rule1 & aboveCl, y - ucl, 
                                    ifelse(rule1 & !aboveCl, lcl - y, NA))) %>%
      #set already established extremes as NA
      mutate(rule1Distance = ifelse(row_number() %in% exclusion_points, NA, rule1Distance))
    
    furthest_extreme <- max(calculation_period$rule1Distance, na.rm = T)
    exclusion_point <- which(calculation_period$rule1Distance == furthest_extreme)
    
    #add next exclusion point and furthest extreme to the vectors
    furthest_extremes <- c(furthest_extremes, furthest_extreme)
    exclusion_points <- c(exclusion_points, exclusion_point)
    i = i + 1
  }

  #check whether there are more than 3 exclusion points (due to points with the same values)
  if(length(exclusion_points) > 3){
    exclusion_points <- exclusion_points[1:3]
  }
  
  exclusion_points
}


#function to form display limits (period extension)
form_display_limits <- function(limits_table, counter){
  
  limits_table[counter:nrow(limits_table), "ucl"] <- limits_table[(counter - 1), "ucl"]
  limits_table[counter:nrow(limits_table), "lcl"] <- limits_table[(counter - 1), "lcl"]
  limits_table[counter:nrow(limits_table), "cl"] <- limits_table[(counter - 1), "cl"]
  limits_table[counter:nrow(limits_table), "periodType"] <- "display"
  
  limits_table
  
}


#function to scan to see where start of each rule 2 break is - returns list of these points 
rule2_break_start_positions <- function(limits_table, counter){
  #add a column for start of rule 2 breaks
  #Flags if there is a rule 2 highlight and that is not preceded by a rule 2 highlight 
  limits_table <- limits_table %>% mutate(startOfRule2Break = ifelse(rule2 & rule2 != lag(rule2), T, F))
  next_rule_break_positions <- (which(limits_table$startOfRule2Break[counter:nrow(limits_table)] == T)) + counter - 1
  
  next_rule_break_positions
  
}


#function to identify whether there has been a rule break in the opposite direction in calc period
#returns TRUE for rule break in opposite direction within candidate calc period 
#set counter to beginning of candidate limits 
identify_opposite_break <- function(limits_table, counter, periodMin, noRegrets = T){
  
  if(noRegrets){
    #only looks at calculation period for no regrets 
    limits_table <- limits_table[counter:(counter + periodMin - 1),]
    limits_table <- add_rule_breaks(limits_table)
  }else{
    #start rule breaks from candidate period as not to include "hang over" rule breaks from prev period
    #but do include "hang over" into following display period
    limits_table_top <- limits_table[1:(counter-1),]
    limits_table_bottom <- add_rule_breaks(limits_table[counter:nrow(limits_table),])
    limits_table <- bind_rows(limits_table_top, limits_table_bottom)
  }

  #state whether each point is above or below the centre line
  limits_table <- limits_table %>% mutate(aboveOrBelowCl = ifelse(y > cl, 1,
                                                                  ifelse(y < cl, -1, 0)))
  
  #Numerical change in centre line
  cl_change <- limits_table$cl[counter - 1] - limits_table$cl[counter]
  #state whether rule break is up (1), down (-1) or no change (0)
  rule_break_direction <- ifelse(cl_change > 0, -1, 1)
    
  #looks for a rule break in the opposite direction within the candidate period
  limits_table <- limits_table %>% mutate(oppositeBreak  = ifelse(rule2 & (aboveOrBelowCl != rule_break_direction), T, F))
  
  if(noRegrets){
    next_rule_break_position <- min(which(limits_table$oppositeBreak == T )) + counter - 1
  }else{
    next_rule_break_position <- min(which(limits_table$oppositeBreak[counter:(counter + periodMin - 1)] == T )) + counter - 1
  }
  
  if(next_rule_break_position == Inf){
    #No rule break in opposite direction
    list(FALSE, NA, limits_table)
  }else{
    # next_y_change <- limits_table$y[next_rule_break_position - 1] - limits_table$y[next_rule_break_position]
    # next_rule_break_direction <- ifelse(next_y_change > 0, "DOWN", "UP")
    list(TRUE, next_rule_break_position, limits_table)

  }

}


#function to create initial limits with rule breaks
initialise_limits <- function(data, periodMin, 
                              counter, cht_type,
                              maxNoOfExclusions){
  
  #form calculation limits for first period
  limits_table <- form_calculation_limits(data = data, periodMin = periodMin,
                                          counter = counter, cht_type = cht_type,
                                          maxNoOfExclusions  = maxNoOfExclusions)
  
  #extend display limits to end 
  limits_table <- form_display_limits(limits_table = limits_table, 
                                      counter = counter + periodMin + 1)
  
  #add rule breaks
  limits_table <- add_rule_breaks(x = limits_table)
}

