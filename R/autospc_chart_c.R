# autospc_chart_c class

#' Construct an autospc_chart_c object
#'
#' @return An object of class `c("autospc_chart_c", "autospc_chart")`.
#' @noRd
new_autospc_chart_c <- function(x) {
  return(
    new_autospc_chart(x,
      class = "autospc_chart_c"
    )
  )
}


#' Validate an autospc_chart_c object
#'
#' See `validate_autospc_chart()` for the class contract.
#'
#' @return `x`, unchanged, if valid; otherwise an error.
#' @noRd
validate_autospc_chart_c <- function(x) {
  if (!inherits(x, "autospc_chart_c")) {
    stop("Not an autospc_chart_c object.", call. = FALSE)
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


#' Create an autospc_chart_c object
#'
#' Helper for C charts: assemble, construct, validate, round, return.
#'
#' @return An object of class `c("autospc_chart_c", "autospc_chart")`.
#' @noRd
autospc_chart_c <- function(data,
                            x,
                            y,
                            ...) {
  autospc_chart_c_l <- assemble_chart_list(
    data = data,
    x = x,
    y = y,
    ...
  )

  autospc_chart_c_l <- normalise_columns(autospc_chart_c_l,
    fields = c("x", "y")
  )

  autospc_chart_c_object <- new_autospc_chart_c(autospc_chart_c_l)

  autospc_chart_c_object <- validate_autospc_chart_c(autospc_chart_c_object)

  autospc_chart_c_object <- round_counts(autospc_chart_c_object)

  return(autospc_chart_c_object)
}


# Analysis methods

#' Round the count columns to whole numbers
#'
#' @return autospc_chart_c object
#' @noRd
round_counts.autospc_chart_c <- function(chart) {
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
aggregate_data.autospc_chart_c <- function(chart) {
  df_agg <- chart$data %>%
    dplyr::group_by(x) %>%
    dplyr::summarise(y = sum(y))

  chart$data <- df_agg

  return(chart)
}


#' Calculate control limits for a subset of C-chart data
#'
#' Centre line and limits are established using standard formulae based on the
#' Poisson distribution (see e.g. Provost and Murray) for non-excluded data
#' points
#'
#' @return list of three vectors (cl, ucl, lcl), each the same length as period
#' @noRd
calculate_limits.autospc_chart_c <- function(chart,
                                             period,
                                             exclusion_points) {
  limits <- get_c_limits(
    y = period$y,
    exclusion_points = exclusion_points
  )

  return(limits)
}


# Presentation methods

#' Chart name
#'
#' @return string, name of chart for labels
#' @noRd
chart_type_label.autospc_chart_c <- function(chart) {
  return("C")
}


#' Lower and upper ends of the y axis
#'
#' Headroom above the highest point or limit, so the annotations have room.
#'
#' @return list of two numbers, low and high
#' @noRd
y_axis_range.autospc_chart_c <- function(chart,
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
y_axis_title.autospc_chart_c <- function(chart) {
  return("Number")
}
