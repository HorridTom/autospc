# autospc_chart_u class

#' Construct an autospc_chart_u object
#'
#' @return An object of class `c("autospc_chart_u", "autospc_chart")`.
#' @noRd
new_autospc_chart_u <- function(x) {
  return(
    new_autospc_chart(x,
      class = "autospc_chart_u"
    )
  )
}


#' Validate an autospc_chart_u object
#'
#' See `validate_autospc_chart()` for the class contract.
#'
#' @return `x`, unchanged, if valid; otherwise an error.
#' @noRd
validate_autospc_chart_u <- function(x) {
  if (!inherits(x, "autospc_chart_u")) {
    stop("Not an autospc_chart_u object.", call. = FALSE)
  }

  x <- validate_autospc_chart(x)

  validate_subgroup_elements(x, class_elements = autospc_chart_u_elements())

  validate_rate_columns(x$data)

  return(x)
}


#' Elements specific to autospc_chart_u objects
#'
#' Additional to those given by `autospc_chart_elements()`, which every chart
#' carries. `n` holds the name of the column giving the area of opportunity
#' each count was made over.
#'
#' @return A character vector of element names.
#' @noRd
autospc_chart_u_elements <- function() {
  chart_elements <- c(
    "n"
  )

  return(chart_elements)
}


#' Create an autospc_chart_u object
#'
#' Helper for U charts: assemble, construct, validate, round, check, return.
#'
#' @return An object of class `c("autospc_chart_u", "autospc_chart")`.
#' @noRd
autospc_chart_u <- function(data,
                            x,
                            y,
                            n,
                            ...) {
  autospc_chart_u_l <- assemble_chart_list(
    data = data,
    x = x,
    y = y,
    ...
  )
  autospc_chart_u_l <- c(
    autospc_chart_u_l,
    list(n = n)
  )

  autospc_chart_u_l <- normalise_columns(autospc_chart_u_l,
    fields = c("x", "y", "n")
  )

  autospc_chart_u_object <- new_autospc_chart_u(autospc_chart_u_l)

  autospc_chart_u_object <- validate_autospc_chart_u(autospc_chart_u_object)

  autospc_chart_u_object <- round_counts(autospc_chart_u_object)

  check_rate_counts(autospc_chart_u_object$data)

  return(autospc_chart_u_object)
}


# Helpers shared with autospc_chart_up


#' Stop unless the data is usable for a U or U' chart
#'
#' Both columns are required: `y` the counts, and `n` the area of opportunity
#' each count was made over, in whatever unit the rate is to be expressed per.
#'
#' @param data The chart's data.
#'
#' @return invisible TRUE, or an error
#' @noRd
validate_rate_columns <- function(data) {
  require_column(
    data = data,
    column = "y",
    message = paste(
      "y not specified. For U and U' charts, y must",
      "be specified."
    )
  )

  require_column(
    data = data,
    column = "n",
    message = paste(
      "n not specified. For U and U' charts, n must be specified: it is",
      "the area of opportunity each count in y was made over."
    )
  )

  for (column in c("y", "n")) {
    require_column_type(
      data = data,
      column = column,
      types = c("integer", "double"),
      message = paste0(
        "For U and U' charts, ", column, " must be of type integer or ",
        "double."
      )
    )
  }

  return(invisible(TRUE))
}


#' Round the counts of a U or U' chart to whole numbers
#'
#' Only `y`: the area of opportunity need not be a whole number.
#'
#' @param data The chart's data.
#'
#' @return `data`, with `y` rounded
#' @noRd
round_rate_counts <- function(data) {
  return(round_count_column(
    data = data,
    column = "y",
    message = paste(
      "At least one element of y has non-zero",
      "fractional part. Rounding to the nearest whole",
      "number.\nU and U' charts require y to be a count,",
      "i.e. whole numbers only."
    )
  ))
}


#' Stop unless the counts and areas of opportunity are usable
#'
#' Runs after the counts are rounded. A count and an area of opportunity
#' cannot be negative, and a count over an area of zero has no rate.
#'
#' @param data The chart's data.
#'
#' @return invisible TRUE, or an error naming the rows at fault
#' @noRd
check_rate_counts <- function(data) {
  require_count_not_negative(
    data = data,
    message = "For U and U' charts, y cannot be negative."
  )

  require_denominator_not_negative(
    data = data,
    message = "For U and U' charts, n cannot be negative."
  )

  require_no_count_without_denominator(
    data = data,
    message = "For U and U' charts, y must be 0 where n is 0."
  )

  return(invisible(TRUE))
}


# Analysis methods


#' Round the count column to whole numbers
#'
#' @return autospc_chart_u object
#' @noRd
round_counts.autospc_chart_u <- function(chart) {
  chart$data <- round_rate_counts(chart$data)

  return(chart)
}


#' Aggregate data for analysis
#'
#' Sums y and n over x, so that rows sharing an x become one subgroup whose
#' rate is its total count over its total area of opportunity.
#'
#' @return autospc_chart_u object
#' @noRd
aggregate_data.autospc_chart_u <- function(chart) {
  return(
    aggregate_ratios(chart,
      allow_individual_observations = FALSE
    )
  )
}


#' Turn the aggregated data into the series the algorithm analyses
#'
#' A U chart plots rates, so the series is the count over the area of
#' opportunity and `y` keeps the count. Division by a zero or missing area
#' gives `NA` rather than `NaN`.
#'
#' @return autospc_chart object of the same class as chart
#' @noRd
prepare_data.autospc_chart_u <- function(chart) {
  chart$data <- chart$data %>%
    dplyr::mutate(series = y / n) %>%
    dplyr::mutate(series = dplyr::if_else(
      is.nan(series) | is.infinite(series),
      as.numeric(NA),
      series
    ))

  return(chart)
}


#' Calculate control limits for a subset of U-chart data
#'
#' Centre line and standard deviation estimate from the counts and areas of
#' opportunity of the non-excluded points, as `get_u_statistics()` sets out.
#'
#' @return list of `cl` and `sd_estimate`, each the length of the period
#' @noRd
calculate_limits.autospc_chart_u <- function(chart,
                                             period,
                                             exclusion_points) {
  limits <- get_u_statistics(
    y = period$y,
    n = period$n,
    exclusion_points = exclusion_points
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
standard_error_at.autospc_chart_u <- function(chart, sd_estimate, rows) {
  return(rep_len(sd_estimate, nrow(rows)) / sqrt(rows$n))
}


#' The range a rate can take
#'
#' A rate cannot be negative, and has no upper bound.
#'
#' @return list of two numbers, low and high
#' @noRd
limit_bounds.autospc_chart_u <- function(chart) {
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
limits_table_columns.autospc_chart_u <- function(chart) {
  return(c("y", "n"))
}


# Presentation methods


#' Chart name
#'
#' @return string, name of chart for labels
#' @noRd
chart_type_label.autospc_chart_u <- function(chart) {
  return("U")
}


#' Rounding accuracy for centre line labels
#'
#' Four significant figures at the scale of the axis, because a rate's scale
#' depends on the unit the area of opportunity is in.
#'
#' @return number, passed to scales::number(accuracy =)
#' @noRd
label_accuracy.autospc_chart_u <- function(chart,
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
y_axis_range.autospc_chart_u <- function(chart,
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
y_axis_title.autospc_chart_u <- function(chart) {
  return("Rate")
}
