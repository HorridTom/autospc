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
#' @param noRegrets Boolean signifying which version of the algorithm should be used. 
#' Defines whether limits can change as more data is added or not.
#' @param verbosity integer specifying how talkative the algorithm is; the
#' higher the number the more information is provided, none if 0.
#'
#' @return data frame with limits, rule breaks and additional info needed for plotting
#'
#' @importFrom magrittr %>%
#'
#' @examples
create_SPC_auto_limits_table <- function(data, 
                          chartType = "C",
                          periodMin = 21,
                          runRuleLength = 8,
                          maxNoOfExclusions = 3,
                          noRegrets = TRUE,
                          verbosity = 1L,
                          ...
) {

  #add y column of percentages for P and P' charts. This is to avoid issues with joins later 
  if(chartType == "P" | chartType == "P'"){
    data <- data %>% dplyr::mutate(y = b * 100 / n)
  }
  
  #set counter to zero
  counter <- 0
  
  #[1] see whether there are enough data points to form one period
  if(enough_data_for_new_period(data, periodMin, counter)){
    
    #[2]
    limits_table <- form_calculation_and_display_limits(data, periodMin, counter, chartType, maxNoOfExclusions)
    
    #set counter to end of first period
    counter <- counter + periodMin + 1

    #[3]loop starts
    while(counter < nrow(data)){
      
      #[4]see whether there are enough points after the counter to form new period
      if(enough_data_for_new_period(limits_table, periodMin, counter)){
        
        #check if counter is part way through a rule 2 break already, provided there are at least 8 rule 2 break points following
        #if so, set next rule break position to next point 
        if(all(limits_table$rule2[counter:(counter + runRuleLength)])){
          
          rule2_break_positions <- NA
          rule2_break_position <- counter
          
        }else{
          
          #scan for start of next rule 2 break
          rule2_break_positions <- rule2_break_start_positions(limits_table = limits_table, counter = counter)
          rule2_break_position <- rule2_break_positions[1]
          
        }
        
        #[5]see if there are any further rule 2 breaks
        if(!is.na(rule2_break_position) & rule2_break_position < nrow(data)){
          
          
          #[6]set counter to the next rule break position
          counter <- rule2_break_position
          triggering_rule_break_direction <- limits_table$aboveOrBelowCl[counter]

          #[7]see whether there are enough points after the counter to form new period
          if(enough_data_for_new_period(limits_table, periodMin, counter)){
            
            #[8]
            candidate_limits_table <- form_calculation_and_display_limits(data = limits_table, periodMin, counter,
                                                        chartType, maxNoOfExclusions)
            
            #[9]check whether there is a rule break in the opposite direction within calc period
            opposite_rule_break <- identify_opposite_break(candidate_limits_table, counter, periodMin,
                                                           triggering_rule_break_direction)[[1]]
            
            #establish whether (for no regrets) the final run in the candidate
            #calculation period prevents a recalculation
            final_run_prevents <- final_run_of_calc_period_prevents_recalc(
              candidate_limits_table,
              triggering_rule_break_direction)
            
            #recalc if...
            if(!opposite_rule_break & ((noRegrets == TRUE & !final_run_prevents) | noRegrets == FALSE)){
              
              #[10]No opposite rule break in candidate calculation period - candidate limits become real limits
              limits_table <- candidate_limits_table
              
              #Set counter to end of calculation period
              counter <- counter + periodMin + 1

            }else{
              #[11]
              #check if counter is part way through a rule 2 break already
              #provided there are at least 8 rule 2 breaks following or no further rule breaks have been identified 
              if(is.na(rule2_break_positions[2]) | all(limits_table$rule2[counter:(counter + runRuleLength)])){
                
                counter <- counter + 1

              }else{
                
                #set counter to the start of the next rule 2 break 
                counter <- rule2_break_positions[2]

              }
              
            }

          }else{
            if(verbosity > 0) {
              #print("There are not enough data points to form another period. Calculation complete.")
            }
            break
            }
  
        }else{
          if(verbosity > 0) {
            #print("There are no further rule breaks. Calculation complete.")
          }
          break
          }

      }else{        
        if(verbosity > 0) {
          #print("There are not enough data points to form another period. Calculation complete.")
        }
        break
        }
    }#loop ends

    #add a column to show where the breakpoints are
    limits_table <- limits_table %>%
      dplyr::mutate(breakPoint = ifelse(cl == dplyr::lag(cl), FALSE, TRUE))

    limits_table
    
  }else{
    if(verbosity > 0) {
      #print("There are not enough points to form one period.")
    }
  }

}