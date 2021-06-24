library(tidyverse)

source("R/spc_rules.R")
source("R/functions.R")

#function interface for automated SPC function
#' create_SPC_auto_limits_table
#'
#' @param data For a C or C' chart: a data frame with columns x, y, title (optional) 
#' and subtitle (optional)
#' For a P or P' chart: a data frame with columns x, n (total), b (number of breaches), 
#' title (optional), subtitle (optional) 
#' @param chart_typ the type of chart you wish to plot (e.g. "C", "C'", "P", "P'")
#' @param periodMin the minimum number of points per period.
#' @param runRuleLength the number of points above or below the centre line needed
#' for a rule 2 break
#' @param maxNoOfExclusions the maximum number of extreme points to exclude from 
#' limit calculations 
#'
#'
#' @return data frame with limits, rule breaks and additional info needed for plotting
#'
#'
#' @examples
create_SPC_auto_limits_table <- function(data, 
                          cht_type = "C",
                          periodMin = 21,
                          runRuleLength = 8,
                          maxNoOfExclusions = 3,
                          noRegrets = T,
                          ...
) {
  
  data <- mutate(data, x = as.Date(x))
  
  #add y column of percentages for P and P' charts
  #this is to avoid issues with joins later 
  if(cht_type == "P" | cht_type == "P'"){
    data <- data %>% mutate(y = b * 100 / n)
  }
  
  #set counter to zero
  counter <- 0
  
  #see whether there are enough data points to form one period
  if(!enough_data_for_new_period(data, periodMin, counter)){
    
    #add a column to show where the breakpoints are
    limits_table <- limits_table %>% 
      mutate(breakPoint = ifelse(cl == dplyr::lag(cl), F, T))
    
    return(limits_table)
    
  }else{

    #form calculation limits for first period
    limits_table <- form_calculation_limits(data = data, periodMin = periodMin, 
                                            counter = counter, cht_type = cht_type,
                                            maxNoOfExclusions  = maxNoOfExclusions)
    
    #set counter to end of first period
    counter <- counter + periodMin + 1
    
    #extend display limits to end 
    limits_table <- form_display_limits(limits_table = limits_table, 
                                        counter = counter)
    
    #add rule breaks
    limits_table <- add_rule_breaks(x = limits_table)
    
    
    ###loop starts
    while(counter < nrow(data)){
      #see whether there are enough points after the counter to form new period
      if(!enough_data_for_new_period(limits_table, periodMin, counter)){
        print("There are not enough data points to form another period. Calculation complete.")
        break
      }else{
        
        #check if counter is part way through a rule 2 break already
        #provided there are at least 8 rule 2 break points following
        if(all(limits_table$rule2[counter:(counter + runRuleLength)])){
          rule2_break_positions <- NA
          rule2_break_position <- counter
        }else{
          #scan for start of next rule 2 break
          rule2_break_positions <- rule2_break_start_positions(limits_table = limits_table, 
                                                               counter = counter)
          rule2_break_position <- rule2_break_positions[1]
        }
        #print(paste("rule 2 break position", rule2_break_position))
        
        #see if there are any further rule 2 breaks
        if(is.na(rule2_break_position) | rule2_break_position > nrow(data)){
          #print("There are no further rule breaks. Calculation complete.")
          break
          
        }else{
          
          counter <- rule2_break_position
          
          #see whether there are enough points after the counter to form new period
          if(!enough_data_for_new_period(limits_table, periodMin, counter)){
            #print("There are not enough data points to form another period. Calculation complete.")
            break
            
          }else{
            
            #add new candidate calculation period 
            candidate_limits_table <- form_calculation_limits(data = limits_table, 
                                                              periodMin = periodMin, 
                                                              counter = counter, 
                                                              cht_type = cht_type,
                                                              maxNoOfExclusions  = maxNoOfExclusions)
            
            #add new candidate display period
            candidate_limits_table <- form_display_limits(limits_table = candidate_limits_table, 
                                                          counter = counter + periodMin + 1)
            
            #add rule breaks
            candidate_limits_table <- add_rule_breaks(candidate_limits_table)
            
            #check whether there is a rule break in the opposite direction within calc period
            if(!identify_opposite_break(candidate_limits_table, counter = counter, 
                                        periodMin = periodMin, noRegrets = noRegrets)[[1]]){
              
              #No opposite rule break in candidate calculation period
              #candidate limits become real limits
              limits_table <- candidate_limits_table
              
              #Set counter to end of calculation period
              counter <- counter + periodMin + 1

            }else{
              #print("Opposite rule break in candidate period.")
              
              #check if counter is part way through a rule 2 break already
              #provided there are at least 8 rule 2 breaks following
              #or no further rule breaks have been identified 
              if(is.na(rule2_break_positions[2]) | all(limits_table$rule2[counter:(counter + runRuleLength)])){
                #print("Counter is part way through a rule break.")
                counter = counter + 1
                
              }else{
                
                #set counter to the start of the next rule 2 break 
                counter <- rule2_break_positions[2]
                #print(paste("counter",counter))
              }
              
            }
            
          }
          
        }
        
      }
      
    }#####loop ends
    
  }

  #add a column to show where the breakpoints are
  limits_table <- limits_table %>% 
    mutate(breakPoint = ifelse(cl == dplyr::lag(cl), F, T))
  
  limits_table
  
}