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

#function to form limits for a period
#data has columns x and y
form_calculation_limits <- function(data, counter, periodMin, cht_type = "C"){
  
  if(cht_type == "C"){
    calculation_period <- qicharts2::qic(x, y, data = data[counter:(counter + periodMin),], chart = 'c')
  }else if(cht_type == "C'"){
    calculation_period <- qicharts2::qic(x, y, n = rep(1, nrow(data[counter:(counter + periodMin),])), 
                                         data = data[counter:(counter + periodMin),], chart = 'up')
  }else if(cht_type == "P"){
    calculation_period <- qicharts2::qic(x, y, n = n, data = data[counter:(counter + periodMin),], 
                                         chart = 'p', multiply = 100)
  }else if(cht_type == "P'"){
    calculation_period <- qicharts2::qic(x, y, n = n, data = data[counter:(counter + periodMin),], 
                                         chart = 'pp', multiply = 100)
  }

  
  calculation_period <- calculation_period$data %>%
    select(x,y,ucl,lcl, cl) %>%
    mutate(periodType = "calculation")
  
  #code to exclude most extreme 3 points from calculation period
  calculation_period <- add_rule_breaks(calculation_period)
  calculation_period <- calculation_period %>% mutate(aboveCl = ifelse(y > cl, T,ifelse(y < cl, F, NA))) %>%
    mutate(rule1Distance = ifelse(rule1 & aboveCl, y - ucl, 
                                  ifelse(rule1 & !aboveCl, lcl - y, NA)))
  
  #values of 3 furthest extremes
  furthest_extremes <- sort(calculation_period$rule1Distance, decreasing = T)[1:3]
  
  calculation_period <- calculation_period %>% mutate(exclude = ifelse(rule1Distance %in% furthest_extremes, T, F))
  exclusion_points <- which(calculation_period$exclude)
  
  #re-run the calculation of limits now excluding extremes
  if(cht_type == "C"){
    calculation_period <- qicharts2::qic(x, y, data = data[counter:(counter + periodMin),]
                                         , chart = 'c', exclude = exclusion_points)
  }else if(cht_type == "C'"){
    calculation_period <- qicharts2::qic(x, y, n = rep(1, nrow(data[counter:(counter + periodMin),])), 
                                         data = data[counter:(counter + periodMin),]
                                         , chart = 'up', exclude = exclusion_points)
  }else if(cht_type == "P"){
    calculation_period <- qicharts2::qic(x, y, n = n, data = data[counter:(counter + periodMin),], 
                                         chart = 'p', multiply = 100, exclude = exclusion_points)
  }else if(cht_type == "P'"){
    calculation_period <- qicharts2::qic(x, y, n = n, data = data[counter:(counter + periodMin),], 
                                         chart = 'pp', multiply = 100, exclude = exclusion_points)
  }
  
  calculation_period <- calculation_period$data %>%
    select(x,ucl,lcl, cl) %>%
    mutate(periodType = "calculation") %>%
    mutate(excluded = ifelse(row_number() %in% exclusion_points, T, F))


  #left joins if this is the first period
  if(counter == 0){
    limits_table <- data %>%
      left_join(calculation_period, by = "x")
  }else{
    limits_table <- data %>%
      left_join(calculation_period, by = "x") %>%
      mutate(ucl = if_else(is.na(ucl.y), ucl.x, ucl.y)) %>%
      mutate(lcl = if_else(is.na(lcl.y), lcl.x, lcl.y)) %>%
      mutate(cl = if_else(is.na(cl.y), cl.x, cl.y)) %>%
      mutate(periodType = if_else(is.na(periodType.y), periodType.x, periodType.y)) %>%
      mutate(excluded = if_else(is.na(excluded.y), excluded.x, excluded.y)) %>%
      select(x, y, ucl, lcl, cl, periodType, excluded)
  }
  
}



#function to form display limits (period extension)
form_display_limits <- function(limits_table, counter){
  
  limits_table[counter:nrow(limits_table), "ucl"] <- limits_table[(counter - 1), "ucl"]
  limits_table[counter:nrow(limits_table), "lcl"] <- limits_table[(counter - 1), "lcl"]
  limits_table[counter:nrow(limits_table), "cl"] <- limits_table[(counter - 1), "cl"]
  limits_table[counter:nrow(limits_table), "periodType"] <- "display"
  
  limits_table
  
}

#function to scan to see where the next rule 2 break is and returns point position
rule2_break_scan <- function(limits_table, counter){
  next_rule_break_position <- min(which(limits_table$rule2[counter:nrow(limits_table)] == T)) + counter - 1
  next_rule_break_position
}

#function to identify whether there has been a rule break in the opposite direction in calc period
#returns TRUE for rule break in opposite direction within candidate calc period 
#set counter to beginning of candidate limits 
identify_opposite_break <- function(limits_table, counter, periodMin){
  
  #state whether each point is above or below the centre line
  limits_table <- limits_table %>% mutate(aboveOrBelowCl = ifelse(y > cl, "UP",
                                                                  ifelse(y < cl, "DOWN", "NO CHANGE")))
  
  cl_change <- limits_table$cl[counter - 1] - limits_table$cl[counter]
  rule_break_direction <- ifelse(cl_change > 0, "DOWN", "UP")
    
  #looks for a rule break in the opposite direction within the candidate period
  limits_table <- limits_table %>% mutate(oppositeBreak  = ifelse(rule2 & (aboveOrBelowCl != rule_break_direction), T, F))
  next_rule_break_position <- min(which(limits_table$oppositeBreak[counter:(counter + periodMin - 1)] == T )) + counter - 1
  
  if(next_rule_break_position == Inf){
    #No rule break in opposite direction
    list(FALSE, NA, limits_table)
  }else{
    # next_y_change <- limits_table$y[next_rule_break_position - 1] - limits_table$y[next_rule_break_position]
    # next_rule_break_direction <- ifelse(next_y_change > 0, "DOWN", "UP")
    list(TRUE, next_rule_break_position, limits_table)

  }

}