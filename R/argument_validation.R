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

  if (typeof(values) != "double" ||
    !any(!is_whole_number(values), na.rm = TRUE)) {
    return(data)
  }

  data[[column]] <- round(values)

  warning(message, call. = FALSE)

  return(data)
}


#' Describe a value in an error message
#'
#' @return A character scalar.
#' @noRd
describe_value <- function(value) {
  return(paste(deparse(value), collapse = " "))
}


#' Match one argument against the values it accepts
#'
#' The accepted values are the argument's default in `autospc()`, written there
#' as a vector. An argument the caller did not give arrives as that whole
#' vector, and `rlang::arg_match0()` takes the first of them.
#'
#' @return The matched value, or an error naming the argument.
#' @noRd
match_choice <- function(value,
                         name,
                         call = rlang::caller_env()) {
  return(rlang::arg_match0(
    value,
    values = autospc_default(name),
    arg_nm = name,
    error_call = call
  ))
}


#' Check that one argument is TRUE or FALSE
#'
#' 1 and 0 are accepted, as they are anywhere R expects a condition. NA is not,
#' being neither TRUE nor FALSE.
#'
#' @return TRUE or FALSE, or an error naming the argument.
#' @noRd
match_flag <- function(value,
                       name,
                       call = rlang::caller_env()) {
  is_flag <- length(value) == 1L &&
    !is.na(value) &&
    (is.logical(value) ||
      (is.numeric(value) && value %in% c(0, 1)))

  if (!is_flag) {
    rlang::abort(
      sprintf(
        "`%s` must be TRUE or FALSE, not %s.",
        name,
        describe_value(value)
      ),
      call = call
    )
  }

  return(as.logical(value))
}


#' Bring one argument into the range it is documented to take
#'
#' A value outside the range is taken as the nearest end of it, and a value
#' between two whole numbers as the ceiling.
#'
#' @return An integer in `range`, or an error naming the argument.
#' @noRd
match_range <- function(value,
                        name,
                        range,
                        call = rlang::caller_env()) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value)) {
    rlang::abort(
      sprintf(
        "`%s` must be a number from %d to %d, not %s.",
        name,
        range[1],
        range[2],
        describe_value(value)
      ),
      call = call
    )
  }

  return(as.integer(min(max(ceiling(value), range[1]), range[2])))
}


#' Check that one numeric argument is of the kind it accepts
#'
#' The kinds are listed in `autospc_numeric_arguments()`. Inf is a number but
#' not a finite one, and is accepted only where the kind says so.
#'
#' @return `value`, or an error naming the argument.
#' @noRd
match_number <- function(value,
                         name,
                         kind,
                         call = rlang::caller_env()) {
  accepts <- switch(kind,
    count = "a whole number of one or more",
    count_from_zero = "a whole number of zero or more",
    loops = "a whole number of zero or more, or Inf",
    non_negative = "a number of zero or more",
    positive = "a number above zero",
    number = "a number"
  )

  is_kind <- is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    switch(kind,
      count = is.finite(value) && is_whole_number(value) && value >= 1,
      count_from_zero = is.finite(value) &&
        is_whole_number(value) &&
        value >= 0,
      loops = identical(value, Inf) ||
        (is.finite(value) && is_whole_number(value) && value >= 0),
      non_negative = is.finite(value) && value >= 0,
      positive = is.finite(value) && value > 0,
      number = is.finite(value)
    )

  if (!is_kind) {
    rlang::abort(
      sprintf(
        "`%s` must be %s, not %s.",
        name,
        accepts,
        describe_value(value)
      ),
      call = call
    )
  }

  return(value)
}


#' Check that one argument is a single point on the horizontal axis
#'
#' The types the axis holds are not listed here. It holds dates and times as
#' well as numbers, and is not meant to be limited to the types it holds today,
#' so what cannot be a point on it is named instead of what can. Text is
#' excluded because a string reaches `seq()` in `format_x_axis()` and fails
#' there.
#'
#' @return `value`, or an error naming the argument.
#' @noRd
match_axis_value <- function(value,
                             name,
                             call = rlang::caller_env()) {
  is_value <- length(value) == 1L &&
    !is.list(value) &&
    !is.character(value) &&
    !is.factor(value) &&
    !is.na(value)

  if (!is_value) {
    rlang::abort(
      sprintf(
        "`%s` must be a single value on the horizontal axis, not %s.",
        name,
        describe_value(value)
      ),
      call = call
    )
  }

  return(value)
}


#' Check the arguments that accept a fixed set of values
#'
#' Called once per call to `autospc()` or `facet_stages()`, before
#' `validate_algorithm_parameters()`, so that a value that is not one of those
#' allowed is reported as such rather than failing the consistency check.
#'
#' @param arguments A named list of the argument values for one call.
#'
#' @return `arguments`, with each checked value as it was matched.
#' @noRd
validate_argument_values <- function(arguments,
                                     call = rlang::caller_env()) {
  arguments$floating_median <- match_choice(
    arguments$floating_median,
    "floating_median",
    call = call
  )

  for (name in autospc_flag_arguments()) {
    arguments[[name]] <- match_flag(arguments[[name]], name, call = call)
  }

  arguments$verbosity <- match_range(
    arguments$verbosity,
    "verbosity",
    range = c(0L, 2L),
    call = call
  )

  kinds <- autospc_numeric_arguments()

  for (name in names(kinds)) {
    value <- arguments[[name]]

    # an argument autospc() declares as NULL accepts NULL
    if (is.null(value) && is.null(autospc_default(name))) {
      next
    }

    arguments[[name]] <- if (identical(kinds[[name]], "axis_value")) {
      match_axis_value(value, name, call = call)
    } else {
      match_number(value, name, kinds[[name]], call = call)
    }
  }

  return(arguments)
}
