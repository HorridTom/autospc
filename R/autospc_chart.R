# autospc_chart class

new_autospc_chart <- function(x = list(),
                              class = character()) {
  
  stopifnot(is.list(x))
  
  return(
    structure(x,
              class = c(class,
                        "spc_chart"))
  )
  
}


validate_autospc_chart <- function(x) {
  
  base_list <- unclass(x)
  element_names <- names(x)
  
  
  element_check <- autospc_chart_elements() %in% element_names
  if(!all(element_check)) {
    stop(paste("Malformed autospc_chart object - element(s) not present:",
               paste(autospc_chart_elements()[!element_check],
                     collapse = ", ")),
         call. = FALSE)
  }
  
  return(x)
  
}


autospc_chart_elements <- function() {
  
  chart_elements <- c(
    "data",
    "x",
    "y",
    "period_min",
    "baseline_length",
    "shift_rule_threshold",
    "baseline_only",
    "establish_every_shift",
    "no_regrets",
    "overhanging_reversions",
    "max_exclusions",
    "mr_screen_max_loops",
    "centre_line_tolerance"
  )
  
  return(chart_elements)
  
}


autospc_chart <- function(data,
                          x,
                          y,
                          period_min = 21L,
                          baseline_length = NULL,
                          shift_rule_threshold = 8L,
                          baseline_only = FALSE,
                          establish_every_shift = FALSE,
                          no_regrets = TRUE,
                          overhanging_reversions = TRUE,
                          max_exclusions = 3L,
                          mr_screen_max_loops = 1L,
                          centre_line_tolerance = 0) {
  
  autospc_chart_list <- list(
    data = data,
    x = x,
    y = y,
    period_min = period_min,
    baseline_length = baseline_length,
    shift_rule_threshold = shift_rule_threshold,
    baseline_only = baseline_only,
    establish_every_shift = establish_every_shift,
    no_regrets = no_regrets,
    overhanging_reversions = overhanging_reversions,
    max_exclusions = max_exclusions,
    mr_screen_max_loops = mr_screen_max_loops,
    centre_line_tolerance = centre_line_tolerance
  )
  
  autospc_chart_object <- new_autospc_chart(autospc_chart_list)
  
  autospc_chart_object <- validate_autospc_chart(autospc_chart_object)
  
  return(autospc_chart_object)
  
}

