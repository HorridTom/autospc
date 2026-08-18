#' Set control limits over a prepared series
#'
#' Reads `chart$data`. When there are too few points to form a period the table
#' has no limits columns.
#'
#' @return autospc_chart object, with `chart$result$table` set
#' @noRd
run_limit_algorithm <- function(chart) {

  data <- chart$data

  #set counter to one
  counter <- 1
  
  # [1] Counter initialised
  # Check whether there are enough data points to form one period
  if(!enough_data_for_new_period(data = data,
                                 counter = counter,
                                 chart = chart)){
    
    chart$result$table <- data
    chart$result$table$log <- render_log(chart)

    return(chart)
    
  } else {
    
    # [2] There are enough data points to form one period
    limits_table <- form_calculation_and_display_limits(
      data = data, 
      counter_at_period_start = counter, 
      chart = chart)
    
    # Set counter to first point after end of first period
    if(counter == 1L & !is.null(chart$baseline_length)) {
      chart$history$baseline <- list(length = chart$baseline_length,
                                     rows = 1:chart$baseline_length)
      counter <- counter + chart$baseline_length
    } else {
      counter <- counter + chart$period_min
    }
    chart <- record_counter_move(chart, 1L, counter, "first period established")
    
    if(!chart$baseline_only){
      # [3] Algorithm loop starts - unless user specified no recalculations
      while(counter < nrow(data)){
        
        # [4] Check whether enough points after the counter to form new period
        if(!enough_data_for_new_period(data = limits_table,
                                       counter = counter,
                                       chart = chart)) {        
          
          chart <- record_stop(chart, counter,
                               "not enough data for a further period")
          
          break
          
        } else {
          
          # There are sufficient data points remaining after the counter to form
          # a new period if indicated.
          
          # Identify the next rule break to consider as a triggering rule break:
          # Check whether counter is part way through a rule 2 break already,
          # with at least [shift_rule_threshold] rule 2 break points following.
          if(counter_at_rule_break(df = limits_table,
                                   counter = counter,
                                   shift_rule_threshold =
                                     chart$shift_rule_threshold)
          ) {
            # If so, set next rule break position to the counter. 
            rule2_break_positions <- NA
            rule2_break_position <- counter
            
            chart <- record_break(chart, counter, rule2_break_position,
                                  already_at_break = TRUE,
                                  limits_table = limits_table)

          } else {
            # If not, i.e. if either the counter is not within a rule 2 break,
            # or it is but there are fewer than [shift_rule_threshold] points of
            # the run following, then scan for start of next rule 2 break.
            rule2_break_positions <- rule2_break_start_positions(
              limits_table = limits_table,
              counter = counter)
            
            rule2_break_position <- rule2_break_positions[1]
            
            chart <- record_break(chart, counter, rule2_break_position,
                                  already_at_break = FALSE,
                                  limits_table = limits_table)

          }
          
          # [5] Check whether there are any further rule 2 breaks
          if(is.na(rule2_break_position) | rule2_break_position >= nrow(data)){
            # [5b] If not, then there can be no more additional periods
            chart <- record_stop(chart, counter,
                                 "no further shift rule breaks")
            
            break
            
          } else {
            # If so, then consider the next rule break as the start of a
            # potential new period
            
            # [5a] Set counter to the next rule break position and record the
            # direction of the rule break
            chart <- record_counter_move(chart, counter, rule2_break_position,
                                         "moved to shift rule break")
            counter <- rule2_break_position
            triggering_rule_break_direction <-
              limits_table$aboveOrBelowCl[counter]
            
            
            # [6] Check whether there are enough points after the counter to
            # form a new period
            
            if(!enough_data_for_new_period(data = limits_table,
                                           counter = counter,
                                           chart = chart)){
              
              chart <- record_stop(chart, counter,
                                   "too few points after the shift rule break")
              
              break
              
            } else {
              
              # [6a] There are sufficient points. Establish candidate limits
              # using the first period_min points from the counter as
              # calculation period
              
              candidate_limits_table <- form_calculation_and_display_limits(
                data = limits_table,
                counter_at_period_start = counter,
                chart = chart)
              
              # Establish whether there is a rule break in the opposite
              # direction within this calculation period
              
              opposite_rule_break <- identify_opposite_break(
                candidate_limits_table,
                counter,
                chart$period_min,
                triggering_rule_break_direction,
                centre_line_tolerance = chart$centre_line_tolerance,
                shift_rule_threshold = chart$shift_rule_threshold,
                overhanging_reversions = chart$overhanging_reversions)[[1]]
              
              # Establish whether (for no regrets) the final run in the
              # candidate calculation period prevents re-establishment of limits
              final_run_prevents <- final_run_of_calc_period_prevents_recalc(
                candidate_limits_table,
                triggering_rule_break_direction)
              
              # Check whether either we re-establish at every shift OR:
              # 1) There is no opposing rule break AND
              # 2) Either:
              #     a) no_regrets is FALSE OR
              #     b) the final run does not prevent re-establishment of limits
              re_establish <- chart$establish_every_shift |
                (!opposite_rule_break &
                 ((chart$no_regrets == TRUE & !final_run_prevents) |
                  chart$no_regrets == FALSE))

              # Record the candidate. Rejected candidates are not retained
              # anywhere else.
              period_end <- min(counter + chart$period_min - 1L,
                                nrow(limits_table))
              prevailing_row <- if(counter > 1L) as.integer(counter) - 1L else
                NA_integer_

              chart$history$candidates <- c(
                chart$history$candidates,
                list(list(
                  counter            = as.integer(counter),
                  period_rows        = counter:period_end,
                  trigger_direction  = triggering_rule_break_direction,
                  table              = candidate_limits_table,
                  prevailing         = list(
                    last_row = prevailing_row,
                    cl       = limits_table$cl[prevailing_row],
                    ucl      = limits_table$ucl[prevailing_row],
                    lcl      = limits_table$lcl[prevailing_row]),
                  opposite_break     = opposite_rule_break,
                  final_run_prevents = final_run_prevents,
                  accepted           = re_establish
                )))

              if(re_establish){
                # [7a] If so, re-establish limits at the counter, confirming the
                # candidate limits
                
                limits_table <- candidate_limits_table
                
                # and set the counter to the first point after the end of the
                # new calculation period
                chart <- record_counter_move(chart, counter,
                                             counter + chart$period_min,
                                             "limits re-established")
                counter <- counter + chart$period_min
                
              } else {
                # [7b] If not (i.e. there is an opposing rule break, or the
                # final run prevents re-establishment of limits), limits are not
                # re-established, the candidate limits are rejected, and the
                # algorithm proceeds to the next point that could potentially
                # be the start of a new period.
                
                # Check whether:
                # 1) no further rule breaks have been identified OR
                # 2) counter is part way through a rule 2 break with at least
                # [shift_rule_threshold] points of the run following
                if(is.na(rule2_break_positions[2]) | 
                   all(
                     limits_table$rule2[counter:(counter + chart$shift_rule_threshold 
                                                 - 1)]
                   )){
                  
                  # If so, advance the counter by 1
                  chart <- record_counter_move(chart, counter, counter + 1,
                                               "candidate rejected")
                  counter <- counter + 1
                  
                } else {
                  # If not, move counter to the start of the next rule 2 break 
                  chart <- record_counter_move(chart, counter,
                                               rule2_break_positions[2],
                                               "candidate rejected")
                  counter <- rule2_break_positions[2]
                }
              } # end of: [7b] candidate limits rejected
            } # end of: [6a] establish candidate limits
          } # end of [5a], [6] there are rule breaks to consider 
        } # end of: [4a] enough points remaining after the counter
      } # end of: algorithm loop

      # the loop can also end by its own condition, having reached the series
      if(is.null(chart$history$stopped)) {
        chart <- record_stop(chart, counter, "reached the end of the series")
      }

    } else {

      chart <- record_stop(chart, counter, "baseline only")

    } # end of: [3] !baseline_only
    
    
    #update NAs in limit columns
    limits_table <- limits_table %>%
      dplyr::mutate(ucl = dplyr::if_else(is.na(y), as.numeric(NA), ucl)) %>%
      dplyr::mutate(lcl = dplyr::if_else(is.na(y), as.numeric(NA), lcl)) 
    
    chart$result$table <- limits_table
    chart$result$re_establish_rows <- which(limits_table$breakPoint)
    chart$result$exclusions <- which(limits_table$excluded)
    chart$result$table$log <- render_log(chart)

    return(chart)
  } # end of: [2] enough data points to form one period
}
