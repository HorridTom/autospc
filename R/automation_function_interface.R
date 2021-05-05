#function interface for automated SPC function
#' plot_SPC_auto
#'
#' @param data a data frame or CSV file in the standard format
#' @param chart_typ the type of chart you wish to plot(e.g. "C", "C'", "P", "P'")
#' @param periodMin the minimum number of points per period. (This will then be fed
#' into the first freeze argument)
#' @param runRuleLength the number of points above or below the centre line needed
#' for a rule 2 break
#' @param baselineWait ??
#' @param part a vector of part point returned from the SPC algorithm (could 
#' happen within the function)
#' @param excule a vector of points to exclude from any limit calculations
#' @param r1_col the colour for points causing a rule 1 break 
#' @param r2_col the colour for points causing a rule 2 break 
#' @param cht_title the main title of the chart (could be extracted from the csv?)
#' @param sub_tile can be used for the place that the data has come from (e.g. 
#' which board)(could be extracted from the csv?)
#' @param plot_chart if true, chart is plotted, if false, table of plot data is returned
#' @param write_table if true, the plot data is written to a folder location 
#' 
#'
#'
#' @return the input df with an extra column containing the corresponding occupancy on arrival for each patient
#'
#'
#' @examples
create_SPC_auto_limits_table <- function(data, 
                          cht_type = "C",
                          periodMin = 21,
                          runRuleLength = 8,
                          baselineWait = 0,
                          part,
                          freeze = c(20, 80),
                          exclude,
                          
                          r1_col = "orange",
                          r2_col = "steelblue3",
                          cht_title = "title",
                          sub_title = "Ayrshire and Arran",
                          plot_chart = T,
                          write_table = F,
                          ...
) {
  
  data <- mutate(data, x = as.Date(x))
  
  #set counter to zero
  counter <- 0
  
  #see whether there are enough data points to form one period
  if(!enough_data_for_new_period(data, periodMin, counter)){
    print("There are not enough data points to form the minimum period.")
  }else{

    #form calculation limits for first period
    limits_table <- form_calculation_limits(data = data, periodMin = periodMin, counter = counter, cht_type = cht_type)
    
    #set counter to end of first period
    counter <- counter + periodMin + 1
    
    #extend display limits to end 
    limits_table <- form_display_limits(limits_table = limits_table, counter = counter)
    
    #add rule breaks
    limits_table <- add_rule_breaks(x = limits_table)
    
    
    ###loop starts
    while(counter < nrow(data)){
      #see whether there are enough points after the counter to form new period
      if(!enough_data_for_new_period(limits_table, periodMin, counter)){
        print("There are not enough data points to form another period. Calculation complete.")
        break
      }else{
        
        #scan for next rule 2 break
        rule2_break_positions <- rule2_break_scan(limits_table = limits_table, counter = counter)
        rule2_break_position <- rule2_break_positions[1]
        print(paste("rule 2 break position", rule2_break_position))
        
        #see if there are any further rule 2 breaks
        if(is.na(rule2_break_position)){
          print("There are no further rule breaks. Calculation complete.")
          break
          
        }else{
          
          #increase counter to where first rule break is
          counter <- rule2_break_position
          
          #see whether there are enough points after the counter to form new period
          if(!enough_data_for_new_period(limits_table, periodMin, counter)){
            print("There are not enough data points to form another period. Calculation complete.")
            break
            
          }else{
            
            #add new candidate calculation period 
            candidate_limits_table <- form_calculation_limits(data = limits_table, periodMin = periodMin, counter = counter, cht_type = cht_type)
            
            #add new candidate display period
            candidate_limits_table <- form_display_limits(limits_table = candidate_limits_table, counter = counter + periodMin + 1)
            
            #add rule breaks
            candidate_limits_table <- add_rule_breaks(candidate_limits_table)
            
            #check whether there is a rule break in the opposite direction within calc period
            if(!identify_opposite_break(candidate_limits_table, counter = counter, periodMin = periodMin)[[1]]){
              
              #No opposite rule break in candidate calculation period
              #candidate limits become real limits
              limits_table <- candidate_limits_table
              
              #Set counter to end of calculation period
              counter <- counter + periodMin + 1
              
              #Extend display limits
              #limits_table <- form_display_limits(limits_table = limits_table, counter = counter)
              #limits_table <- add_rule_breaks(limits_table)
              
            }else{
              print("Opposite rule break in calc period.")
              
              if(is.na(rule2_break_positions[2])){
                print("There are no further rule breaks.")
                counter = counter + 1
                
              }else{
                counter <- rule2_break_positions[2]
                print(paste("counter",counter))
              }
              
            }
            
            
            
            
          }
          
        }
        
      }
    }#####loop ends
    
  }

  
limits_table
  
}