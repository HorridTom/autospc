# autospc_chart_cp class

#' Construct an autospc_chart_cp object
#'
#' @return An object of class `c("autospc_chart_cp", "autospc_chart")`.
#' @noRd
new_autospc_chart_cp <- function(x) {
  return(
    new_autospc_chart(x,
      class = "autospc_chart_cp"
    )
  )
}


#' Validate an autospc_chart_cp object
#'
#' See `validate_autospc_chart()` for the class contract.
#'
#' @return `x`, unchanged, if valid; otherwise an error.
#' @noRd
validate_autospc_chart_cp <- function(x) {
  if (!inherits(x, "autospc_chart_cp")) {
    stop("Not an autospc_chart_cp object.", call. = FALSE)
  }

  x <- validate_autospc_chart(x)

  require_column(
    data = x$data,
    column = "y",
    message = paste(
      "y not specified. For C and C' charts, y must",
      "be specified."
    )
  )

  require_column_type(
    data = x$data,
    column = "y",
    types = c("integer", "double"),
    message = paste(
      "For a C or C' chart, y must be of type",
      "integer or double."
    )
  )

  return(x)
}


#' Create an autospc_chart_cp object
#'
#' Helper for C' charts: assemble, construct, validate, round, return.
#'
#' @return An object of class `c("autospc_chart_cp", "autospc_chart")`.
#' @noRd
autospc_chart_cp <- function(data,
                             x,
                             y,
                             ...) {
  autospc_chart_cp_l <- assemble_chart_list(
    data = data,
    x = x,
    y = y,
    ...
  )

  autospc_chart_cp_l <- normalise_columns(autospc_chart_cp_l,
    fields = c("x", "y")
  )

  autospc_chart_cp_object <- new_autospc_chart_cp(autospc_chart_cp_l)

  autospc_chart_cp_object <- validate_autospc_chart_cp(autospc_chart_cp_object)

  autospc_chart_cp_object <- round_counts(autospc_chart_cp_object)

  return(autospc_chart_cp_object)
}


# Analysis methods

#' Round the count columns to whole numbers
#'
#' @return autospc_chart_cp object
#' @noRd
round_counts.autospc_chart_cp <- function(chart) {
  chart$data <- round_count_column(
    data = chart$data,
    column = "y",
    message = paste(
      "At least one element of y has non-zero fractional",
      "part. Rounding to the nearest whole number.\n",
      "C and C' charts are for count data, i.e. whole",
      "numbers only."
    )
  )

  return(chart)
}


#' Aggregate data for analysis
#'
#' Sums y (count) over x (subgroup)
#'
#' @return autospc_chart object of the same class as chart
#' @noRd
aggregate_data.autospc_chart_cp <- function(chart) {
  chart$data <- sum_over_subgroups(chart$data,
    columns = "y",
    aggregation_na_rm = chart$aggregation_na_rm
  )

  return(chart)
}


#' Calculate control limits for a subset of C'-chart data
#'
#' As for the C chart, but with the standard deviation inflated by the mean
#' moving range of the data, screened for outliers. The number of screening
#' passes is taken from the chart's `mr_screen_max_loops`.
#'
#' @return list of three vectors (cl, ucl, lcl), each the same length as period
#' @noRd
calculate_limits.autospc_chart_cp <- function(chart,
                                              period,
                                              exclusion_points) {
  limits <- get_cp_limits(
    y = period$y,
    exclusion_points = exclusion_points,
    mr_screen_max_loops = chart$mr_screen_max_loops
  )

  return(limits)
}


# Presentation methods

#' Chart name
#'
#' @return string, name of chart for labels
#' @noRd
chart_type_label.autospc_chart_cp <- function(chart) {
  return("C'")
}


#' Lower and upper ends of the y axis
#'
#' Headroom above the highest point or limit, so the annotations have room.
#'
#' @return list of two numbers, low and high
#' @noRd
y_axis_range.autospc_chart_cp <- function(chart,
                                          data) {
  high <- max(data$ucl,
    data$y,
    na.rm = TRUE
  ) +
    max(data$ucl,
      na.rm = TRUE
    ) / 10 +
    10

  return(list(
    low = 0,
    high = high
  ))
}


#' Retrieve default y axis label
#'
#' @return string
#' @noRd
y_axis_title.autospc_chart_cp <- function(chart) {
  return("Number")
}
