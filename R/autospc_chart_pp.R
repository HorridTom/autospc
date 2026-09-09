# autospc_chart_pp class

#' Construct an autospc_chart_pp object
#'
#' @return An object of class `c("autospc_chart_pp", "autospc_chart")`.
#' @noRd
new_autospc_chart_pp <- function(x) {
  return(
    new_autospc_chart(x,
      class = "autospc_chart_pp"
    )
  )
}


#' Validate an autospc_chart_pp object
#'
#' See `validate_autospc_chart()` for the class contract.
#'
#' @return `x`, unchanged, if valid; otherwise an error.
#' @noRd
validate_autospc_chart_pp <- function(x) {
  if (!inherits(x, "autospc_chart_pp")) {
    stop("Not an autospc_chart_pp object.", call. = FALSE)
  }

  x <- validate_autospc_chart(x)

  element_names <- names(x)

  element_check <- autospc_chart_pp_elements() %in% element_names

  if (!all(element_check)) {
    stop(
      paste(
        "Malformed autospc_chart_pp object - element(s) not present:",
        paste(autospc_chart_pp_elements()[!element_check],
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  require_column(
    data = x$data,
    column = "y",
    message = paste(
      "y not specified. For P and P' charts, y must",
      "be specified."
    )
  )

  if (!"n" %in% colnames(x$data)) {
    # No denominator column: the numerator has to be individual binary
    # observations, one row per observation.
    require_column_type(
      data = x$data,
      column = "y",
      types = "logical",
      message = paste(
        "n is not specified and y is not of",
        "type logical. For P and P' charts, if",
        "n is not specified, y must be of type",
        "logical."
      )
    )
  } else {
    require_column_type(
      data = x$data,
      column = "y",
      types = c("integer", "double"),
      message = paste(
        "For a P or P' chart with n specified,",
        "y must be of type integer or double."
      )
    )

    require_column_type(
      data = x$data,
      column = "n",
      types = c("integer", "double"),
      message = paste(
        "For a P or P' chart with n specified,",
        "n must be of type integer or double."
      )
    )
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
#' Helper for P' charts: assemble, construct, validate, round, return.
#'
#' @return An object of class `c("autospc_chart_pp", "autospc_chart")`.
#' @noRd
autospc_chart_pp <- function(data,
                             x,
                             y,
                             n,
                             ...) {
  autospc_chart_pp_l <- assemble_chart_list(
    data = data,
    x = x,
    y = y,
    ...
  )
  autospc_chart_pp_l <- c(
    autospc_chart_pp_l,
    list(n = n)
  )

  autospc_chart_pp_l <- normalise_columns(autospc_chart_pp_l,
    fields = c("x", "y", "n")
  )

  autospc_chart_pp_object <- new_autospc_chart_pp(autospc_chart_pp_l)

  autospc_chart_pp_object <- validate_autospc_chart_pp(autospc_chart_pp_object)

  autospc_chart_pp_object <- round_counts(autospc_chart_pp_object)

  return(autospc_chart_pp_object)
}


# Analysis methods


#' Round the count columns to whole numbers
#'
#' Where the data has no n column there is nothing to round: y is then
#' individual binary observations, which the validator has already required to
#' be of type logical.
#'
#' @return autospc_chart_pp object
#' @noRd
round_counts.autospc_chart_pp <- function(chart) {
  if (!"n" %in% colnames(chart$data)) {
    return(chart)
  }

  chart$data <- round_count_column(
    data = chart$data,
    column = "y",
    message = paste(
      "At least one element of y has non-zero",
      "fractional part. Rounding to the nearest whole",
      "number.\nP and P' charts with n specified",
      "require y to be a count, i.e. whole numbers only."
    )
  )

  chart$data <- round_count_column(
    data = chart$data,
    column = "n",
    message = paste(
      "At least one element of n has non-zero",
      "fractional part. Rounding to the nearest whole",
      "number.\nP and P' charts with n specified",
      "require n to be a count, i.e. whole numbers only."
    )
  )

  return(chart)
}


#' Aggregate data for analysis
#'
#' Sums y and n (counts) over x (subgroup) as needed for P'-chart analysis.
#' Data may be provided as either pre-aggregated counts for y and n or
#' individual binary observations
#'
#' @return autospc_chart_pp object
#' @noRd
aggregate_data.autospc_chart_pp <- function(chart) {
  return(
    aggregate_ratios(chart,
      allow_individual_observations = TRUE
    )
  )
}


#' Turn the aggregated data into the series the algorithm analyses
#'
#' A P' chart plots percentages, so the series is the count as a percentage of
#' the denominator and `y` keeps the count. Division by a zero or missing
#' denominator gives `NA` rather than `NaN` or `Inf`.
#'
#' @return autospc_chart object of the same class as chart
#' @noRd
prepare_data.autospc_chart_pp <- function(chart) {
  chart$data <- chart$data %>%
    dplyr::mutate(series = y * 100 / n) %>%
    dplyr::mutate(series = dplyr::if_else(
      is.nan(series) | is.infinite(series),
      as.numeric(NA),
      series
    ))

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
  limits <- get_pp_limits(
    y = period$y,
    n = period$n,
    exclusion_points = exclusion_points,
    multiply = 100,
    mr_screen_max_loops = chart$mr_screen_max_loops
  )

  return(limits)
}


#' Columns the limits table carries beside the series under analysis
#'
#' The count and the denominator, because the limits of this class are
#' calculated from both rather than from the percentages it plots.
#'
#' @return character vector
#' @noRd
limits_table_columns.autospc_chart_pp <- function(chart) {
  return(c("y", "n"))
}


#' Extend the limits of the preceding calculation period over the display period
#'
#' The limits of a P' chart depend on the denominator, so they cannot simply be
#' carried forward. The width of the last calculated period is expressed as a
#' constant, and the display limits are recomputed from it at each point's own
#' denominator. The centre line is carried forward unchanged.
#'
#' @return `limits_table`, with the display rows filled in
#' @noRd
extend_display_limits.autospc_chart_pp <- function(chart,
                                                   limits_table,
                                                   counter) {
  return(extend_display_limits_at_denominators(
    limits_table = limits_table,
    counter = counter
  ))
}


#' Limits for the rows that hold no observation
#'
#' The limits of a P' chart vary with the denominator, so the values the
#' default method gives are recalculated at each row's own denominator.
#'
#' @return list of three vectors, named cl, ucl and lcl
#' @noRd
limits_for_missing_rows.autospc_chart_pp <- function(chart,
                                                     period,
                                                     rows) {
  return(proportion_limits_for_missing_rows(
    limits = NextMethod(),
    period = period,
    rows = rows
  ))
}


#' Limits to use beyond the end of the data
#'
#' As for P, except that the denominators are left as they are and
#' `use_nbar_for_stdev` handles the averaging inside `get_pp_limits()`.
#'
#' @return list of single values, named cl, lcl and ucl
#' @noRd
extrapolate_limits.autospc_chart_pp <- function(chart,
                                                period) {
  exclusion_points <- period %>%
    dplyr::pull(excluded) %>%
    which()

  limits <- get_pp_limits(
    y = period$y,
    n = period$n,
    exclusion_points = exclusion_points,
    multiply = 100,
    use_nbar_for_stdev = TRUE
  )

  return(lapply(limits[c("cl", "ucl", "lcl")], "[[", 1L))
}

# Presentation methods


#' The centre line label, formatted
#'
#' A per cent sign, and no thousands separator: the values are percentages.
#'
#' @return character
#' @noRd
centre_line_label.autospc_chart_pp <- function(chart,
                                               cl,
                                               ylimhigh) {
  return(scales::number(cl,
    accuracy = label_accuracy(
      chart = chart,
      ylimhigh = ylimhigh
    ),
    suffix = "%"
  ))
}


#' Chart name
#'
#' @return string, name of chart for labels
#' @noRd
chart_type_label.autospc_chart_pp <- function(chart) {
  return("P'")
}


#' Rounding accuracy for centre line labels
#'
#' @return number, passed to scales::number(accuracy =)
#' @noRd
label_accuracy.autospc_chart_pp <- function(chart,
                                            ylimhigh) {
  return(0.1)
}


#' Default vertical position of centre line labels
#'
#' As for the P chart: the axis is a percentage scale, so a tenth above the
#' upper control limit is a wide gap in the units of the axis.
#'
#' @return number, the scale factor applied to the upper control limit
#' @noRd
upper_annotation_sf_default.autospc_chart_pp <- function(chart) {
  return(1.04)
}


#' Retrieve default y axis label
#'
#' @return string
#' @noRd
y_axis_title.autospc_chart_pp <- function(chart) {
  return("Percentage")
}
