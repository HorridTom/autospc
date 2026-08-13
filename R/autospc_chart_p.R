# autospc_chart_p class

#' Construct an autospc_chart_p object
#'
#' @return An object of class `c("autospc_chart_p", "autospc_chart")`.
#' @noRd
new_autospc_chart_p <- function(x) {
  
  return(
    new_autospc_chart(x,
                      class = "autospc_chart_p")
  )
  
}


#' Validate an autospc_chart_p object
#'
#' See `validate_autospc_chart()` for the class contract.
#'
#' @return `x`, unchanged, if valid; otherwise an error.
#' @noRd
validate_autospc_chart_p <- function(x) {
  
  if(!inherits(x, "autospc_chart_p")) {
    stop("Not an autospc_chart_p object.", call. = FALSE)
  }
  
  x <- validate_autospc_chart(x)
  
  element_names <- names(x)
  
  element_check <- autospc_chart_p_elements() %in% element_names
  
  if(!all(element_check)) {
    stop(paste("Malformed autospc_chart_p object - element(s) not present:",
               paste(autospc_chart_p_elements()[!element_check],
                     collapse = ", ")),
         call. = FALSE)
  }
  
  return(x)
  
}


#' Elements specific to autospc_chart_p objects
#'
#' Additional to those given by `autospc_chart_elements()`, which every chart
#' carries. `n` holds the name of the denominator column.
#'
#' @return A character vector of element names.
#' @noRd
autospc_chart_p_elements <- function() {
  
  chart_elements <- c(
    "n"
  )
  
  return(chart_elements)
  
}


#' Create an autospc_chart_p object
#'
#' Helper for P charts: assemble, construct, validate, return.
#'
#' @return An object of class `c("autospc_chart_p", "autospc_chart")`.
#' @noRd
autospc_chart_p <- function(data,
                            x,
                            y,
                            n,
                            ...) {
  
  autospc_chart_p_l <- autospc_chart_list(data = data,
                                          x = x,
                                          y = y,
                                          ...)
  autospc_chart_p_l <- c(autospc_chart_p_l,
                         list(n = n))
  
  autospc_chart_p_l <- normalise_columns(autospc_chart_p_l,
                                         fields = c("x", "y", "n"))

  autospc_chart_p_object <- new_autospc_chart_p(autospc_chart_p_l)
  
  autospc_chart_p_object <- validate_autospc_chart_p(autospc_chart_p_object)
  
  return(autospc_chart_p_object)
  
}


# Analysis methods

#' Aggregate data for analysis
#' 
#' Sums y and n (counts) over x (subgroup) as needed for P-chart analysis.
#' Data may be provided as either pre-aggregated counts for y and n or
#' individual binary observations
#'
#' @return autospc_chart_p object
#' @noRd
aggregate_data.autospc_chart_p <- function(chart) {

  return(
    aggregate_ratios(chart,
                     allow_individual_observations = TRUE)
  )

}


#' Calculate control limits for a subset of P-chart data
#' 
#' Centre line and limits are established using standard formulae based on the
#' Binomial distribution (see e.g. Provost and Murray) for non-excluded data
#' points
#'
#' @return list of three vectors (cl, ucl, lcl), each the same length as period
#' @noRd
calculate_limits.autospc_chart_p <- function(chart,
                                             period,
                                             exclusion_points) {
  
  limits <- get_p_limits(y = period$y_numerator,
                         n = period$n,
                         exclusion_points = exclusion_points,
                         multiply = 100)
  
  return(limits)
  
}



#' Columns the limits table carries in addition to the common ones
#'
#' `y` holds percentages for this class, so the counts and denominators the
#' limits were calculated from have to be kept as well.
#'
#' @return character vector
#' @noRd
limits_table_columns.autospc_chart_p <- function(chart) {

  return(c("n", "y_numerator"))

}

# Presentation methods

#' Chart name
#'
#' @return string, name of chart for labels
#' @noRd
chart_type_label.autospc_chart_p <- function(chart) {
  return("P")
}


#' Retrieve default y axis label
#'
#' @return string
#' @noRd
y_axis_title.autospc_chart_p <- function(chart) {
  return("Percentage")
}
