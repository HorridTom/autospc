# autospc_chart_up class

#' Construct an autospc_chart_up object
#'
#' @return An object of class `c("autospc_chart_up", "autospc_chart")`.
#' @noRd
new_autospc_chart_up <- function(x) {
  return(
    new_autospc_chart(x,
      class = "autospc_chart_up"
    )
  )
}


#' Validate an autospc_chart_up object
#'
#' See `validate_autospc_chart()` for the class contract.
#'
#' @return `x`, unchanged, if valid; otherwise an error.
#' @noRd
validate_autospc_chart_up <- function(x) {
  if (!inherits(x, "autospc_chart_up")) {
    stop("Not an autospc_chart_up object.", call. = FALSE)
  }

  x <- validate_autospc_chart(x)

  validate_subgroup_elements(x, class_elements = autospc_chart_up_elements())

  validate_rate_columns(x$data)

  return(x)
}


#' Elements specific to autospc_chart_up objects
#'
#' Additional to those given by `autospc_chart_elements()`, which every chart
#' carries. `n` holds the name of the column giving the area of opportunity
#' each count was made over.
#'
#' @return A character vector of element names.
#' @noRd
autospc_chart_up_elements <- function() {
  chart_elements <- c(
    "n"
  )

  return(chart_elements)
}


#' Create an autospc_chart_up object
#'
#' Helper for U' charts: assemble, construct, validate, round, check, return.
#'
#' @return An object of class `c("autospc_chart_up", "autospc_chart")`.
#' @noRd
autospc_chart_up <- function(data,
                             x,
                             y,
                             n,
                             ...) {
  autospc_chart_up_l <- assemble_chart_list(
    data = data,
    x = x,
    y = y,
    ...
  )
  autospc_chart_up_l <- c(
    autospc_chart_up_l,
    list(n = n)
  )

  autospc_chart_up_l <- normalise_columns(autospc_chart_up_l,
    fields = c("x", "y", "n")
  )

  autospc_chart_up_object <- new_autospc_chart_up(autospc_chart_up_l)

  autospc_chart_up_object <- validate_autospc_chart_up(autospc_chart_up_object)

  autospc_chart_up_object <- round_counts(autospc_chart_up_object)

  check_rate_counts(autospc_chart_up_object$data)

  return(autospc_chart_up_object)
}


# Analysis methods


#' Round the count column to whole numbers
#'
#' @return autospc_chart_up object
#' @noRd
round_counts.autospc_chart_up <- function(chart) {
  chart$data <- round_rate_counts(chart$data)

  return(chart)
}


#' Aggregate data for analysis
#'
#' Sums y and n over x, so that rows sharing an x become one subgroup whose
#' rate is its total count over its total area of opportunity.
#'
#' @return autospc_chart_up object
#' @noRd
aggregate_data.autospc_chart_up <- function(chart) {
  return(
    aggregate_ratios(chart,
      allow_individual_observations = FALSE
    )
  )
}


#' Turn the aggregated data into the series the algorithm analyses
#'
#' A U' chart plots rates, so the series is the count over the area of
#' opportunity and `y` keeps the count. Division by a zero or missing area
#' gives `NA` rather than `NaN`.
#'
#' @return autospc_chart object of the same class as chart
#' @noRd
prepare_data.autospc_chart_up <- function(chart) {
  chart$data <- chart$data %>%
    dplyr::mutate(series = y / n) %>%
    dplyr::mutate(series = dplyr::if_else(
      is.nan(series) | is.infinite(series),
      as.numeric(NA),
      series
    ))

  return(chart)
}


#' Calculate control limits for a subset of U'-chart data
#'
#' As for the U chart, but with the standard deviation estimate multiplied by
#' Laney's sigma_z, from the moving ranges of the z-scores screened for
#' outliers. The number of screening passes is taken from the chart's
#' `mr_screen_max_loops`.
#'
#' @return list of `cl` and `sd_estimate`, each the length of the period
#' @noRd
calculate_limits.autospc_chart_up <- function(chart,
                                              period,
                                              exclusion_points) {
  limits <- get_up_statistics(
    y = period$y,
    n = period$n,
    exclusion_points = exclusion_points,
    mr_screen_max_loops = chart$mr_screen_max_loops
  )

  return(limits)
}


#' The standard error at each of a set of rows
#'
#' The estimate is per unit of opportunity, so each row's standard error is
#' the estimate over the square root of that row's area of opportunity.
#'
#' @return numeric, one value per row of `rows`
#' @noRd
standard_error_at.autospc_chart_up <- function(chart, sd_estimate, rows) {
  return(rep_len(sd_estimate, nrow(rows)) / sqrt(rows$n))
}


#' The range a rate can take
#'
#' A rate cannot be negative, and has no upper bound.
#'
#' @return list of two numbers, low and high
#' @noRd
limit_bounds.autospc_chart_up <- function(chart) {
  return(list(
    low = 0,
    high = Inf
  ))
}


#' Columns the limits table carries beside the series under analysis
#'
#' The count and the area of opportunity, because the limits of this class are
#' calculated from both rather than from the rates it plots.
#'
#' @return character vector
#' @noRd
limits_table_columns.autospc_chart_up <- function(chart) {
  return(c("y", "n"))
}


# Presentation methods


#' Chart name
#'
#' @return string, name of chart for labels
#' @noRd
chart_type_label.autospc_chart_up <- function(chart) {
  return("U'")
}


#' Rounding accuracy for centre line labels
#'
#' Four significant figures at the scale of the axis, because a rate's scale
#' depends on the unit the area of opportunity is in.
#'
#' @return number, passed to scales::number(accuracy =)
#' @noRd
label_accuracy.autospc_chart_up <- function(chart,
                                            ylimhigh) {
  accuracy <- 10^(ceiling(log10(ylimhigh)) - 4)

  return(accuracy)
}


#' Lower and upper ends of the y axis
#'
#' From zero, or from the lowest limit or point where one is below zero, to a
#' tenth above the highest limit or point.
#'
#' @return list of two numbers, low and high
#' @noRd
y_axis_range.autospc_chart_up <- function(chart,
                                          data) {
  low <- min(0, data$lcl, data$series, na.rm = TRUE)

  high <- max(data$ucl,
    data$series,
    na.rm = TRUE
  ) * 1.1

  return(list(
    low = low,
    high = high
  ))
}


#' Retrieve default y axis label
#'
#' @return string
#' @noRd
y_axis_title.autospc_chart_up <- function(chart) {
  return("Rate")
}
