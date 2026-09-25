# autospc_chart_s class

#' Construct an autospc_chart_s object
#'
#' @return An object of class `c("autospc_chart_s", "autospc_chart")`.
#' @noRd
new_autospc_chart_s <- function(x) {
  return(
    new_autospc_chart(x,
      class = "autospc_chart_s"
    )
  )
}


#' Validate an autospc_chart_s object
#'
#' See `validate_autospc_chart()` for the class contract.
#'
#' @return `x`, unchanged, if valid; otherwise an error.
#' @noRd
validate_autospc_chart_s <- function(x) {
  if (!inherits(x, "autospc_chart_s")) {
    stop("Not an autospc_chart_s object.", call. = FALSE)
  }

  x <- validate_autospc_chart(x)

  validate_subgroup_elements(x, class_elements = autospc_chart_s_elements())

  validate_subgroup_columns(x$data)

  return(x)
}


#' Elements specific to autospc_chart_s objects
#'
#' Additional to those given by `autospc_chart_elements()`, which every chart
#' carries. `n` and `s` hold the names of the subgroup size and standard
#' deviation columns, used where the data is given one row per subgroup.
#'
#' @return A character vector of element names.
#' @noRd
autospc_chart_s_elements <- function() {
  chart_elements <- c(
    "n",
    "s"
  )

  return(chart_elements)
}


#' Create an autospc_chart_s object
#'
#' Helper for S charts: assemble, construct, validate, round, return.
#'
#' @return An object of class `c("autospc_chart_s", "autospc_chart")`.
#' @noRd
autospc_chart_s <- function(data,
                            x,
                            y,
                            n = "n",
                            s = "s",
                            ...) {
  autospc_chart_s_l <- assemble_chart_list(
    data = data,
    x = x,
    y = y,
    ...
  )
  autospc_chart_s_l <- c(
    autospc_chart_s_l,
    list(n = n, s = s)
  )

  autospc_chart_s_l <- normalise_columns(autospc_chart_s_l,
    fields = c("x", "y", "n", "s")
  )

  autospc_chart_s_object <- new_autospc_chart_s(autospc_chart_s_l)

  autospc_chart_s_object <- validate_autospc_chart_s(autospc_chart_s_object)

  autospc_chart_s_object <- round_counts(autospc_chart_s_object)

  return(autospc_chart_s_object)
}


# Analysis methods


#' Round the subgroup sizes to whole numbers
#'
#' Where the data has no n column each row is one observation, and there is
#' nothing to round.
#'
#' @return autospc_chart_s object
#' @noRd
round_counts.autospc_chart_s <- function(chart) {
  chart$data <- round_subgroup_sizes(chart$data)

  return(chart)
}


#' Aggregate data for analysis
#'
#' One row per subgroup, holding its mean `y`, size `n` and sample standard
#' deviation `s`, combined from the rows that share an `x`.
#'
#' @return autospc_chart_s object
#' @noRd
aggregate_data.autospc_chart_s <- function(chart) {
  return(aggregate_xbars_statistics(chart))
}


#' Turn the aggregated data into the series the algorithm analyses
#'
#' An S chart analyses the subgroup standard deviations, so the series is `s`
#' and `y` keeps the subgroup means. A subgroup of one has no standard
#' deviation, so its row holds no observation.
#'
#' @return autospc_chart_s object
#' @noRd
prepare_data.autospc_chart_s <- function(chart) {
  chart$data$series <- chart$data$s

  return(chart)
}


#' Calculate control limits for a subset of S-chart data
#'
#' The centre line is sbar, and the standard deviation estimate at each
#' subgroup is that of its sample standard deviation at its size, following
#' Provost and Murray.
#'
#' @return list of two vectors (cl, sd_estimate), each the same length as period
#' @noRd
calculate_limits.autospc_chart_s <- function(chart,
                                             period,
                                             exclusion_points) {
  return(get_s_statistics(
    s = period$series,
    n = period$n,
    exclusion_points = exclusion_points
  ))
}


#' Columns the limits table carries beside the series under analysis
#'
#' The subgroup mean as supplied, and the subgroup size the limits are
#' calculated at.
#'
#' @return character vector
#' @noRd
limits_table_columns.autospc_chart_s <- function(chart) {
  return(c("y", "n"))
}


#' The range a standard deviation can take
#'
#' A standard deviation is not negative.
#'
#' @return list of two numbers, low and high
#' @noRd
limit_bounds.autospc_chart_s <- function(chart) {
  return(list(
    low = 0,
    high = Inf
  ))
}


#' A period's standard deviation estimate at each of a set of rows
#'
#' The standard deviation of a subgroup's sample standard deviation, at each
#' row's subgroup size, formed from the centre line, which is sbar.
#'
#' @return numeric, one value per row of `rows`
#' @noRd
sd_estimate_at.autospc_chart_s <- function(chart, statistics, rows) {
  return(s_sd_estimate(sbar = statistics$cl, n = rows$n))
}


#' Control limits from a period's statistics
#'
#' Three standard errors either side of the centre line, with the lower limit no
#' lower than zero, whether or not limits are being constrained: B3 is zero for
#' a subgroup of fewer than six.
#'
#' @return list of two numeric vectors named ucl and lcl
#' @noRd
limits_from_statistics.autospc_chart_s <- function(chart, statistics, rows) {
  limits <- NextMethod()

  limits$lcl <- pmax(limits$lcl, 0)

  return(limits)
}


#' Columns the S chart adds when its pair is joined wide
#'
#' The subgroup standard deviations and their centre line and limits, as
#' `subgroup_s`, `scl`, `s_ucl` and `s_lcl`.
#'
#' @return named character vector
#' @noRd
paired_columns.autospc_chart_s <- function(chart) {
  return(c(subgroup_s = "series", scl = "cl", s_ucl = "ucl", s_lcl = "lcl"))
}


# Presentation methods

#' Chart name
#'
#' @return string, name of chart for labels
#' @noRd
chart_type_label.autospc_chart_s <- function(chart) {
  return("S")
}


#' Rounding accuracy for centre line labels
#'
#' Four significant figures at the scale of the axis, because the values are in
#' the units of the measure rather than percentages.
#'
#' @return number, passed to scales::number(accuracy =)
#' @noRd
label_accuracy.autospc_chart_s <- function(chart,
                                           ylimhigh) {
  accuracy <- 10^(ceiling(log10(ylimhigh)) - 4)

  return(accuracy)
}


#' Do this chart's labels always sit above the centre line?
#'
#' Yes, as for the moving range chart: the lower limit is often zero, which
#' leaves no room below the centre line.
#'
#' @return TRUE or FALSE
#' @noRd
labels_stay_above.autospc_chart_s <- function(chart) {
  return(TRUE)
}


#' Lower and upper ends of the y axis
#'
#' @return list of two numbers, low and high
#' @noRd
y_axis_range.autospc_chart_s <- function(chart,
                                         data) {
  high <- max(data$ucl,
    data$series,
    na.rm = TRUE
  ) * 1.1

  return(list(
    low = 0,
    high = high
  ))
}


#' Retrieve default y axis label
#'
#' @return string
#' @noRd
y_axis_title.autospc_chart_s <- function(chart) {
  return("S")
}
