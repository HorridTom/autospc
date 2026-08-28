#' Validate chart_type argument
#'
#' Checks that `chart_type` is a single, non-NULL character string corresponding
#' to a supported SPC chart type. Intended for internal use only.
#'
#' @param chart_type Character scalar specifying chart type.
#'
#' @return Invisibly returns TRUE if valid; otherwise errors.
#' @noRd
validate_chart_type <- function(chart_type) {
  allowed_chart_types <- autospc_chart_types()

  # NULL check
  if (is.null(chart_type)) {
    lifecycle::deprecate_stop(
      when = "0.0.0.9008",
      what = I("chart_type  = NULL"),
      details = I(paste(
        "Please explicitly pass the desired chart type.",
        "Available chart types are: ",
        paste(allowed_chart_types, collapse = ", "),
        "."
      ))
    )
  }

  # Length check
  if (length(chart_type) != 1) {
    stop(
      "chart_type must have length one. ",
      "Available chart types are: ",
      paste(allowed_chart_types, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  # Type check (defensive)
  if (!is.character(chart_type)) {
    stop(
      "chart_type must be a character string.",
      call. = FALSE
    )
  }

  # Value check
  if (!chart_type %in% allowed_chart_types) {
    stop(
      sprintf(
        "Invalid chart_type: '%s'. Available chart types are: %s.",
        chart_type,
        paste(allowed_chart_types, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Warn where x is not a type the axis can be built from
#'
#' Date, POSIXct, numeric and integer are what the scales in `visualisation.R`
#' know how to draw. Anything else is a warning rather than an error, because
#' the analysis itself only needs x to order the rows.
#'
#' Called once per call to `autospc()` or `facet_stages()`, on the series they
#' were given. Checking each chart of an XmR pair, or each facet, would repeat
#' the same warning for the same series.
#'
#' `x` is NULL where the caller named a column the data does not hold. Nothing
#' is checked in that case: the class validator reports the missing column.
#'
#' @param x The column the caller named as x.
#'
#' @return invisible TRUE
#' @noRd
check_x_type <- function(x) {
  if (is.null(x)) {
    return(invisible(TRUE))
  }

  x_class <- class(x)

  if (all(x_class != "Date") &
    all(x_class != c("POSIXct", "POSIXt")) &
    all(x_class != "numeric") &
    all(x_class != "integer")) {
    warning(paste(
      "Please make sure that your x column is a",
      "'Date', 'POSIXct', 'numeric' or 'integer' type."
    ))
  }

  invisible(TRUE)
}


#' Check the algorithm parameters against each other
#'
#' The checks on parameters that are not independent of one another. Where a
#' pair is inconsistent, one of the two is changed and the caller is warned.
#'
#' `no_regrets = TRUE` with `overhanging_reversions = FALSE` sets
#' `overhanging_reversions` to TRUE.
#'
#' Called once per call to `autospc()` or `facet_stages()`.
#'
#' @param arguments A named list of the argument values for one call.
#'
#' @return `arguments`, with any value the checks changed.
#' @noRd
validate_algorithm_parameters <- function(arguments) {
  if (arguments$no_regrets & !arguments$overhanging_reversions) {
    warning(paste0(
      "Setting no_regrets = TRUE and overhanging_reversions = ",
      "FALSE does not make sense, since no_regrets requires ",
      "consideration of overhanging reversions. Changing ",
      "overhanging_reversions to TRUE."
    ))

    arguments$overhanging_reversions <- TRUE
  }

  return(arguments)
}


#' Stop unless a column is present in the data
#'
#' The error text is passed in as `message` so that each class can give the
#' wording for the columns it requires.
#'
#' @return invisible TRUE, or an error with `message` as its text
#' @noRd
require_column <- function(data,
                           column,
                           message) {
  if (!column %in% colnames(data)) {
    stop(message, call. = FALSE)
  }

  invisible(TRUE)
}


#' Stop unless a column has one of the given types
#'
#' `types` are the values `typeof()` returns, so a count column is "integer" or
#' "double" and a column of individual binary observations is "logical".
#'
#' @return invisible TRUE, or an error with `message` as its text
#' @noRd
require_column_type <- function(data,
                                column,
                                types,
                                message) {
  if (!typeof(data[[column]]) %in% types) {
    stop(message, call. = FALSE)
  }

  invisible(TRUE)
}


is_whole_number <- function(x,
                            tol = .Machine$double.eps^0.5) {
  return(abs(x - round(x)) < tol)
}


#' Round a count column, with a warning, if any value is not a whole number
#'
#' A column whose values are all whole numbers is returned unchanged, whether it
#' is of type integer or double, and no warning is given.
#'
#' @return `data`, with `column` rounded if it needed rounding
#' @noRd
round_count_column <- function(data,
                               column,
                               message) {
  values <- data[[column]]

  if (typeof(values) != "double" || !any(!is_whole_number(values), na.rm = TRUE)) {
    return(data)
  }

  data[[column]] <- round(values)

  warning(message, call. = FALSE)

  return(data)
}
