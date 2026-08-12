# autospc_chart_pp class

#' Construct an autospc_chart_pp object
#'
#' @return An object of class `c("autospc_chart_pp", "autospc_chart")`.
#' @noRd
new_autospc_chart_pp <- function(x) {

  return(
    new_autospc_chart(x,
                      class = "autospc_chart_pp")
  )

}


#' Validate an autospc_chart_pp object
#'
#' See `validate_autospc_chart()` for the class contract.
#'
#' @return `x`, unchanged, if valid; otherwise an error.
#' @noRd
validate_autospc_chart_pp <- function(x) {

  if(!inherits(x, "autospc_chart_pp")) {
    stop("Not an autospc_chart_pp object.", call. = FALSE)
  }

  x <- validate_autospc_chart(x)

  element_names <- names(x)

  element_check <- autospc_chart_pp_elements() %in% element_names

  if(!all(element_check)) {
    stop(paste("Malformed autospc_chart_pp object - element(s) not present:",
               paste(autospc_chart_pp_elements()[!element_check],
                     collapse = ", ")),
         call. = FALSE)
  }

  return(x)

}


#' Elements specific to autospc_chart_pp objects
#'
#' Additional to those given by `autospc_chart_elements()`, which every chart
#' carries. `n` holds the name of the denominator column.
#'
#' @return A character vector of element names.
#' @noRd
autospc_chart_pp_elements <- function() {

  chart_elements <- c(
    "n"
  )

  return(chart_elements)

}


#' Create an autospc_chart_pp object
#'
#' Helper for P' charts: assemble, construct, validate, return.
#'
#' @return An object of class `c("autospc_chart_pp", "autospc_chart")`.
#' @noRd
autospc_chart_pp <- function(data,
                             x,
                             y,
                             n,
                             ...) {

  autospc_chart_pp_l <- autospc_chart_list(data = data,
                                           x = x,
                                           y = y,
                                           ...)
  autospc_chart_pp_l <- c(autospc_chart_pp_l,
                          list(n = n))

  autospc_chart_pp_object <- new_autospc_chart_pp(autospc_chart_pp_l)

  autospc_chart_pp_object <- validate_autospc_chart_pp(autospc_chart_pp_object)

  return(autospc_chart_pp_object)

}


# Analysis methods

#' Aggregate data for analysis
#'
#' Sums y and n (counts) over x (subgroup) as needed for P'-chart analysis.
#' Data may be provided as either pre-aggregated counts for y and n or
#' individual binary observations
#'
#' @return autospc_chart_pp object
#' @noRd
aggregate_data.autospc_chart_pp <- function(chart) {

  any_multiple_x <- chart$data %>%
    dplyr::group_by(x) %>%
    dplyr::summarise(num_rows = dplyr::n()) %>%
    dplyr::mutate(multiple_rows = num_rows > 1L) %>%
    dplyr::pull(multiple_rows) %>%
    any()

  # Check if data fully pre-aggregated, return with the same column signature
  # as the aggregated route if so
  if(("n" %in% colnames(chart$data)) &&
     is.numeric(chart$data$y) &&
     !any_multiple_x) {
    chart$data <- chart$data %>%
      dplyr::select(x, y, n)

    return(chart)
  }

  # Set up n for aggregation if data provided as individual binary observations
  if(!("n" %in% colnames(chart$data)) && is.logical(chart$data$y)) {
    chart$data <- chart$data %>%
      dplyr::mutate(n = 1L)
  }

  chart$data <- chart$data %>%
    dplyr::group_by(x) %>%
    dplyr::summarise(y = sum(y),
                     n = sum(n))

  return(chart)

}


#' Calculate control limits for a subset of P'-chart data
#'
#' As for the P chart, but with the standard deviation inflated by the mean
#' moving range of the data, screened for outliers. The number of screening
#' passes is taken from the chart's `mr_screen_max_loops`.
#'
#' @return list of three vectors (cl, ucl, lcl), each the same length as period
#' @noRd
calculate_limits.autospc_chart_pp <- function(chart,
                                              period,
                                              exclusion_points) {

  limits <- get_pp_limits(y = period$y_numerator,
                          n = period$n,
                          exclusion_points = exclusion_points,
                          multiply = 100,
                          mr_screen_max_loops = chart$mr_screen_max_loops)

  return(limits)

}


# Presentation methods

#' Chart name
#'
#' @return string, name of chart for labels
#' @noRd
chart_type_label.autospc_chart_pp <- function(chart) {
  return("P'")
}


#' Retrieve default y axis label
#'
#' @return string
#' @noRd
y_axis_title.autospc_chart_pp <- function(chart) {
  return("Percentage")
}
