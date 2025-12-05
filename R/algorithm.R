#' Automatically recalculate SPC control limits
#'
#' `create_SPC_auto_limits_table` applies the Stable Shift Algorithm to automate
#' recalculation of control limits.
#' 
#' @inheritParams autospc
#'
#' @return data frame with limits, rule breaks and additional info needed for
#'   plotting
#'
#' @examples
#' # Calculate limts for a C' chart for count of monthly attendances
#'
#' df <- ed_attendances_monthly %>%
#'         dplyr::rename(x = Month_Start,
#'                       y = Att_All)
#'
#' limits_table <- create_SPC_auto_limits_table(
#'   df,
#'   chart_type = "C'",
#'   period_min = 21,
#'   baseline_length = NULL,
#'   shift_rule_threshold = 8,
#'   maxNoOfExclusions = 3,
#'   noRegrets = TRUE,
#'   verbosity = 1L,
#'   baseline_only = FALSE,
#'   establish_every_shift = FALSE,
#'   rule2Tolerance = 0,
#'   showLimits = TRUE,
#'   overhangingReversions = TRUE,
#'   mr_screen_max_loops = 1L
#' )
#'
#' head(limits_table)
#'
#' @export
create_SPC_auto_limits_table <- function(data, 
                                         chart_type,
                                         period_min,
                                         baseline_length,
                                         shift_rule_threshold,
                                         maxNoOfExclusions,
                                         noRegrets,
                                         verbosity,
                                         baseline_only,
                                         establish_every_shift,
                                         rule2Tolerance,
                                         showLimits,
                                         overhangingReversions,
                                         mr_screen_max_loops
) {
  
  if(noRegrets & !overhangingReversions) {
    warning(paste0("Setting noRegrets = TRUE and overhangingReversions = ",
                   "FALSE does not make sense, since noRegrets requires ",
                   "consideration of overhanging reversions. Changing ",
                   "overhangingReversions to TRUE."))
    overhangingReversions <- TRUE
  }
  
  # add y column of percentages for P and P' charts. This is to avoid issues
  # with joins later 
  if(chart_type == "P" | chart_type == "P'"){
    data <- data %>% 
      dplyr::mutate(y_numerator = y) %>%
      dplyr::mutate(y = y * 100 / n) %>%
      dplyr::mutate(y = dplyr::if_else(is.nan(y) | is.infinite(y),
                                       as.numeric(NA),
                                       y))
  }
  
  #set counter to one
  counter <- 1
  
  # [1] Counter initialised
  data <- record_log_entry(df = data,
                           counter = counter,
                           entry = "0100")
  # Check whether there are enough data points to form one period
  if(!enough_data_for_new_period(data = data,
                                 period_min = period_min,
                                 baseline_length = baseline_length,
                                 counter = counter,
                                 chart_type = chart_type)){
    
    data <- record_log_entry(df = data,
                             counter = counter,
                             entry = "0210")
    
    if(showLimits == TRUE){
      warning(paste("The input data has fewer than the minimum number of",
                    "points needed to calculate one period. Timeseries data",
                    "without limits has been displayed."))
    }
    
    return(data)
    
  } else {
    
    # [2] There are enough data points to form one period
    limits_table <- form_calculation_and_display_limits(
      data = data, 
      period_min = period_min,
      baseline_length = baseline_length,
      counter_at_period_start = counter, 
      chart_type = chart_type, 
      maxNoOfExclusions  = maxNoOfExclusions, 
      rule2Tolerance = rule2Tolerance,
      shift_rule_threshold = shift_rule_threshold,
      mr_screen_max_loops = mr_screen_max_loops)
    
    limits_table <- record_log_entry(df = limits_table,
                                     counter = counter,
                                     entry = "0200")
    
    # Set counter to first point after end of first period
    if(counter == 1L & !is.null(baseline_length)) {
      counter <- counter + baseline_length
    } else {
      counter <- counter + period_min
    }
    
    if(!baseline_only){
      # [3] Algorithm loop starts - unless user specified no recalculations
      limits_table <- record_log_entry(df = limits_table,
                                       counter = counter,
                                       entry = "0300")
      
      while(counter < nrow(data)){
        
        # [4] Check whether enough points after the counter to form new period
        if(!enough_data_for_new_period(data = limits_table,
                                       period_min = period_min,
                                       baseline_length = baseline_length,
                                       counter = counter,
                                       chart_type = chart_type)) {        
          
          limits_table <- record_log_entry(df = limits_table,
                                           counter = counter,
                                           entry = "0410")
          
          break
          
        } else {
          
          # There are sufficient data points remaining after the counter to form
          # a new period if indicated.
          
          # Identify the next rule break to consider as a triggering rule break:
          # Check whether counter is part way through a rule 2 break already,
          # with at least [shift_rule_threshold] rule 2 break points following.
          if(counter_at_rule_break(df = limits_table,
                                   counter = counter,
                                   shift_rule_threshold = shift_rule_threshold)){
            # If so, set next rule break position to the counter. 
            rule2_break_positions <- NA
            rule2_break_position <- counter
            
            log_entry <- paste0("0400",
                                rule2_break_position)
            
            limits_table <- record_log_entry(df = limits_table,
                                             counter = counter,
                                             entry = log_entry)
            
          } else {
            # If not, i.e. if either the counter is not within a rule 2 break,
            # or it is but there are fewer than [shift_rule_threshold] points of the
            # run following, then scan for start of next rule 2 break.
            rule2_break_positions <- rule2_break_start_positions(
              limits_table = limits_table,
              counter = counter)
            
            rule2_break_position <- rule2_break_positions[1]
            
            log_entry <- paste0("0401",
                                rule2_break_position)
            
            limits_table <- record_log_entry(df = limits_table,
                                             counter = counter,
                                             entry = log_entry)
            
          }
          
          # [5] Check whether there are any further rule 2 breaks
          if(is.na(rule2_break_position) | rule2_break_position >= nrow(data)){
            # [5b] If not, then there can be no more additional periods
            limits_table <- record_log_entry(df = limits_table,
                                             counter = counter,
                                             entry = "0510")
            
            break
            
          } else {
            # If so, then consider the next rule break as the start of a
            # potential new period
            
            # [5a] Set counter to the next rule break position and record the
            # direction of the rule break
            counter <- rule2_break_position
            triggering_rule_break_direction <-
              limits_table$aboveOrBelowCl[counter]
            
            log_entry <- paste0("0500",
                                sign_chr(triggering_rule_break_direction))
            
            limits_table <- record_log_entry(df = limits_table,
                                             counter = counter,
                                             entry = log_entry)
            
            
            # [6] Check whether there are enough points after the counter to
            # form a new period
            
            if(!enough_data_for_new_period(data = limits_table,
                                           period_min = period_min,
                                           baseline_length = baseline_length,
                                           counter = counter,
                                           chart_type = chart_type)){
              
              limits_table <- record_log_entry(df = limits_table,
                                               counter = counter,
                                               entry = "0610")
              
              break
              
            } else {
              
              # [6a] There are sufficient points. Establish candidate limits
              # using the first period_min points from the counter as calculation
              # period
              
              candidate_limits_table <- form_calculation_and_display_limits(
                data = limits_table,
                period_min = period_min,
                baseline_length = baseline_length,
                counter_at_period_start = counter,
                chart_type = chart_type,
                maxNoOfExclusions = maxNoOfExclusions,
                rule2Tolerance = rule2Tolerance,
                shift_rule_threshold = shift_rule_threshold,
                mr_screen_max_loops = mr_screen_max_loops)
              
              # Establish whether there is a rule break in the opposite
              # direction within this calculation period
              
              opposite_rule_break <- identify_opposite_break(
                candidate_limits_table,
                counter,
                period_min,
                triggering_rule_break_direction,
                rule2Tolerance = rule2Tolerance,
                shift_rule_threshold = shift_rule_threshold,
                overhangingReversions = overhangingReversions)[[1]]
              
              # Establish whether (for no regrets) the final run in the
              # candidate calculation period prevents re-establishment of limits
              final_run_prevents <- final_run_of_calc_period_prevents_recalc(
                candidate_limits_table,
                triggering_rule_break_direction)
              
              log_entry <- paste0("0600",
                                  as.integer(opposite_rule_break),
                                  as.integer(final_run_prevents))
              
              limits_table <- record_log_entry(df = limits_table,
                                               counter = counter,
                                               entry = log_entry)
              candidate_limits_table <- record_log_entry(
                df = candidate_limits_table,
                counter = counter,
                entry = log_entry)
              
              # Check whether either we recalculate at every shift OR:
              # 1) There is no opposing rule break AND
              # 2) Either:
              #     a) noRegrets is FALSE OR
              #     b) the final run does not prevent re-establishment of limits
              if(establish_every_shift |
                 (!opposite_rule_break &
                  ((noRegrets == TRUE & !final_run_prevents) |
                   noRegrets == FALSE))){
                # [7a] If so, re-establish limits at the counter, confirming the
                # candidate limits
                
                limits_table <- candidate_limits_table
                
                limits_table <- record_log_entry(df = limits_table,
                                                 counter = counter,
                                                 entry = "0700")
                
                # and set the counter to the first point after the end of the
                # new calculation period
                counter <- counter + period_min
                
              } else {
                # [7b] If not (i.e. there is an opposing rule break, or the
                # final run prevents re-establishment of limits), limits are not
                # re-established, the candidate limits are rejected, and the
                # algorithm proceeds to the next point that could potentially
                # be the start of a new period.
                
                limits_table <- record_log_entry(df = limits_table,
                                                 counter = counter,
                                                 entry = "0710")
                
                # Check whether:
                # 1) no further rule breaks have been identified OR
                # 2) counter is part way through a rule 2 break with at least
                # [shift_rule_threshold] points of the run following
                if(is.na(rule2_break_positions[2]) | 
                   all(
                     limits_table$rule2[counter:(counter + shift_rule_threshold - 1)]
                   )){
                  
                  # If so, advance the counter by 1
                  counter <- counter + 1
                  
                } else {
                  # If not, move counter to the start of the next rule 2 break 
                  counter <- rule2_break_positions[2]
                }
              } # end of: [7b] candidate limits rejected
            } # end of: [6a] establish candidate limits
          } # end of [5a], [6] there are rule breaks to consider 
        } # end of: [4a] enough points remaining after the counter
      } # end of: algorithm loop
    } # end of: [3] !baseline_only
    
    
    #update NAs in limit columns
    limits_table <- limits_table %>%
      dplyr::mutate(ucl = dplyr::if_else(is.na(y), as.numeric(NA), ucl)) %>%
      dplyr::mutate(lcl = dplyr::if_else(is.na(y), as.numeric(NA), lcl)) 
    
    return(limits_table)
  } # end of: [2] enough data points to form one period
}
