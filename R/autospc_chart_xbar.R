# autospc_chart_xbar class

#' Construct an autospc_chart_xbar object
#'
#' @return An object of class `c("autospc_chart_xbar", "autospc_chart")`.
#' @noRd
new_autospc_chart_xbar <- function(x) {
  return(
    new_autospc_chart(x,
      class = "autospc_chart_xbar"
    )
  )
}


#' Validate an autospc_chart_xbar object
#'
#' See `validate_autospc_chart()` for the class contract.
#'
#' @return `x`, unchanged, if valid; otherwise an error.
#' @noRd
validate_autospc_chart_xbar <- function(x) {
  if (!inherits(x, "autospc_chart_xbar")) {
    stop("Not an autospc_chart_xbar object.", call. = FALSE)
  }

  x <- validate_autospc_chart(x)

  validate_subgroup_elements(x, class_elements = autospc_chart_xbar_elements())

  validate_subgroup_columns(x$data)

  return(x)
}


#' Elements specific to autospc_chart_xbar objects
#'
#' Additional to those given by `autospc_chart_elements()`, which every chart
#' carries. `n` and `s` hold the names of the subgroup size and standard
#' deviation columns, used where the data is given one row per subgroup.
#'
#' @return A character vector of element names.
#' @noRd
autospc_chart_xbar_elements <- function() {
  chart_elements <- c(
    "n",
    "s"
  )

  return(chart_elements)
}


#' Create an autospc_chart_xbar object
#'
#' Helper for Xbar charts: assemble, construct, validate, round, return.
#'
#' @return An object of class `c("autospc_chart_xbar", "autospc_chart")`.
#' @noRd
autospc_chart_xbar <- function(data,
                               x,
                               y,
                               n = "n",
                               s = "s",
                               ...) {
  autospc_chart_xbar_l <- assemble_chart_list(
    data = data,
    x = x,
    y = y,
    ...
  )
  autospc_chart_xbar_l <- c(
    autospc_chart_xbar_l,
    list(n = n, s = s)
  )

  autospc_chart_xbar_l <- normalise_columns(autospc_chart_xbar_l,
    fields = c("x", "y", "n", "s")
  )

  autospc_chart_xbar_object <- new_autospc_chart_xbar(autospc_chart_xbar_l)

  autospc_chart_xbar_object <- validate_autospc_chart_xbar(
    autospc_chart_xbar_object
  )

  autospc_chart_xbar_object <- round_counts(autospc_chart_xbar_object)

  return(autospc_chart_xbar_object)
}


# Analysis methods


#' Round the subgroup sizes to whole numbers
#'
#' Where the data has no n column each row is one observation, and there is
#' nothing to round.
#'
#' @return autospc_chart_xbar object
#' @noRd
round_counts.autospc_chart_xbar <- function(chart) {
  chart$data <- round_subgroup_sizes(chart$data)

  return(chart)
}


#' Aggregate data for analysis
#'
#' One row per subgroup, holding its mean `y`, size `n` and sample standard
#' deviation `s`, combined from the rows that share an `x`.
#'
#' @return autospc_chart_xbar object
#' @noRd
aggregate_data.autospc_chart_xbar <- function(chart) {
  return(aggregate_xbars_statistics(chart))
}


#' Calculate control limits for a subset of Xbar-chart data
#'
#' The centre line is the mean of the subgroup means weighted by subgroup size,
#' and the standard deviation estimate at each subgroup is sbar over c4 at its
#' size, following Provost and Murray.
#'
#' @return list of three vectors (cl, sd_estimate, sbar), each the same length
#'   as period
#' @noRd
calculate_limits.autospc_chart_xbar <- function(chart,
                                                period,
                                                exclusion_points) {
  return(get_xbar_statistics(
    y = period$series,
    n = period$n,
    s = period$s,
    exclusion_points = exclusion_points
  ))
}


#' Columns the limits table carries beside the series under analysis
#'
#' The subgroup mean as supplied, and the subgroup size and standard deviation
#' the limits are calculated from.
#'
#' @return character vector
#' @noRd
limits_table_columns.autospc_chart_xbar <- function(chart) {
  return(c("y", "n", "s"))
}


#' The period statistics the limits table carries
#'
#' `sbar` as well as the standard deviation estimate, because the estimate
#' varies with the subgroup size and is formed from `sbar` at each row.
#'
#' @return character vector
#' @noRd
period_statistics_columns.autospc_chart_xbar <- function(chart) {
  return(c("sd_estimate", "sbar"))
}


#' A period's standard deviation estimate at each of a set of rows
#'
#' `sbar` over c4 at each row's subgroup size.
#'
#' @return numeric, one value per row of `rows`
#' @noRd
sd_estimate_at.autospc_chart_xbar <- function(chart, statistics, rows) {
  return(xbar_sd_estimate(sbar = statistics$sbar, n = rows$n))
}


#' The standard error at each of a set of rows
#'
#' The standard error of a subgroup mean: the sd estimate over the square root
#' of the subgroup size.
#'
#' @return numeric, one value per row of `rows`
#' @noRd
standard_error_at.autospc_chart_xbar <- function(chart, sd_estimate, rows) {
  return(rep_len(sd_estimate, nrow(rows)) / sqrt(rows$n))
}


# Presentation methods

#' Chart name
#'
#' @return string, name of chart for labels
#' @noRd
chart_type_label.autospc_chart_xbar <- function(chart) {
  return("Xbar")
}


#' Rounding accuracy for centre line labels
#'
#' Four significant figures at the scale of the axis, because the values are in
#' the units of the measure rather than percentages.
#'
#' @return number, passed to scales::number(accuracy =)
#' @noRd
label_accuracy.autospc_chart_xbar <- function(chart,
                                              ylimhigh) {
  accuracy <- 10^(ceiling(log10(ylimhigh)) - 4)

  return(accuracy)
}


#' Lower and upper ends of the y axis
#'
#' @return list of two numbers, low and high
#' @noRd
y_axis_range.autospc_chart_xbar <- function(chart,
                                            data) {
  low <- min(data$lcl,
    data$series,
    na.rm = TRUE
  )

  if (sign(low) != -1) {
    low <- low * 0.9
  } else {
    low <- low * 1.1
  }

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
y_axis_title.autospc_chart_xbar <- function(chart) {
  return("Xbar")
}


# Helpers shared with autospc_chart_s


#' Stop unless the chart carries the elements of its class
#'
#' @param class_elements The elements the class adds, as its `_elements()`
#'   function gives them.
#'
#' @return invisible TRUE, or an error naming the elements missing
#' @noRd
validate_subgroup_elements <- function(x,
                                       class_elements) {
  element_check <- class_elements %in% names(x)

  if (!all(element_check)) {
    stop(
      paste(
        "Malformed", class(x)[[1L]], "object - element(s) not present:",
        paste(class_elements[!element_check], collapse = ", ")
      ),
      call. = FALSE
    )
  }

  return(invisible(TRUE))
}


#' Stop unless the data is usable for an Xbar or S chart
#'
#' Either one row per observation - `x` and a numeric `y` - or rows that each
#' summarise a subgroup as its mean `y`, size `n` and sample standard deviation
#' `s`, in which case all three are needed.
#'
#' @param data The chart's data.
#'
#' @return invisible TRUE, or an error
#' @noRd
validate_subgroup_columns <- function(data) {
  require_column(
    data = data,
    column = "y",
    message = paste(
      "y not specified. For Xbar, S and XbarS charts, y",
      "must be specified."
    )
  )

  require_column_type(
    data = data,
    column = "y",
    types = c("integer", "double"),
    message = paste(
      "For Xbar, S and XbarS charts, y must be of",
      "type integer or double."
    )
  )

  summarised <- c("n", "s") %in% colnames(data)

  if (any(summarised) && !all(summarised)) {
    stop(
      paste(
        "For Xbar, S and XbarS charts given one row per subgroup, n and s",
        "must both be specified: y is then the subgroup mean, n its size and",
        "s its sample standard deviation."
      ),
      call. = FALSE
    )
  }

  if (all(summarised)) {
    for (column in c("n", "s")) {
      require_column_type(
        data = data,
        column = column,
        types = c("integer", "double"),
        message = paste0(
          "For Xbar, S and XbarS charts, ", column, " must be of type ",
          "integer or double."
        )
      )
    }

    if (any(data$n < 0, na.rm = TRUE) || any(data$s < 0, na.rm = TRUE)) {
      stop(
        paste(
          "For Xbar, S and XbarS charts, n and s cannot be negative."
        ),
        call. = FALSE
      )
    }
  }

  return(invisible(TRUE))
}


#' Round the subgroup sizes to whole numbers
#'
#' Only where the data carries a subgroup size column.
#'
#' @return `data`, with `n` rounded
#' @noRd
round_subgroup_sizes <- function(data) {
  if (!"n" %in% colnames(data)) {
    return(data)
  }

  return(round_count_column(
    data = data,
    column = "n",
    message = paste(
      "At least one element of n has non-zero fractional part. Rounding to",
      "the nearest whole number.\nXbar, S and XbarS charts require n to be a",
      "subgroup size, i.e. whole numbers only."
    )
  ))
}
