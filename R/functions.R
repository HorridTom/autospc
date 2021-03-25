library(tidyverse)

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
form_calculation_limits <- function(data, counter, periodMin){
  
  calculation_period <- qicharts2::qic(x, y, data = data[counter:(counter + periodMin),], chart = 'c')
  
  calculation_period <- calculation_period$data %>%
    select(x,ucl,lcl, cl) %>%
    mutate(periodType = "calculation")
  
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
      select(x, y, ucl, lcl, cl, periodType)
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

#funtion to scan to see where the next rule 2 break is and returns point position
rule2_break_scan <- function(){
  
}

#function to identify whether there has been a rule break in the opposite direction in calc period
indentify_opposite_break <- function(){
  
}