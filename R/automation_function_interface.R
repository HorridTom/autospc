#function interface for automated SPC function
#' plot_SPC_auto
#'
#' @param data a dataframe or CSV file in the standard format
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
plot_SPC_auto <- function(data, 
                          chart_typ = "C",
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
  
  
  #set counter to zero
  counter <- 0
  
  #see whether there are enough data points to form one period
  if(!enough_data_for_new_period(data, periodMin, counter)){
    print("There are not enough data points to form the minimum period.")
    break
  }
  
  #form calculation limits for first period
  
  
  #set counter to end of first period
  counter <- counter + periodMin + 1
  
  #extend display limits to end 
  
  #add rule breaks
  
  #see whetehr there are enough pints after the counter to form new period
  
  #increase counter to where first rule break is
  
  #add new calculation period 
  
  #check whether there is a rule break in the opposite direction within calc period
  #if so remove this calculation period and revert to previous state
  
  #extend display limits to the end
  
  #see whether there are enough data points to form a new period

  
  
  
}