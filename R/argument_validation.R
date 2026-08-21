
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
      details = I(paste("Please explicitly pass the desired chart type.",
                        "Available chart types are: ",
                        paste(allowed_chart_types, collapse = ", "),
                        ".")))
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


#' Resolve overhanging_reversions against no_regrets
#'
#' The two arguments are not independent: the no-regrets rule asks whether a
#' candidate period's final run may revert, so it cannot be applied while
#' overhanging reversions are ignored. Where the pair is inconsistent,
#' `overhanging_reversions` is set to TRUE and the caller is told.
#'
#' Called once per call to `autospc()` or `facet_stages()`, before the chart or
#' charts are constructed. It is argument validation about a pair of user
#' parameters rather than anything about a chart, which is why it is here and
#' not in `autospc_chart()`: the constructor runs once per chart, so an XmR pair
#' produced the same warning twice.
#'
#' @return TRUE or FALSE, the value `overhanging_reversions` should take
#' @noRd
resolve_overhanging_reversions <- function(no_regrets,
                                           overhanging_reversions) {

  if(no_regrets & !overhanging_reversions) {

    warning(paste0("Setting no_regrets = TRUE and overhanging_reversions = ",
                   "FALSE does not make sense, since no_regrets requires ",
                   "consideration of overhanging reversions. Changing ",
                   "overhanging_reversions to TRUE."))

    return(TRUE)

  }

  return(overhanging_reversions)

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

  if(!column %in% colnames(data)) {
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

  if(!typeof(data[[column]]) %in% types) {
    stop(message, call. = FALSE)
  }

  invisible(TRUE)

}


is.wholenumber <- function(x,
                           tol = .Machine$double.eps^0.5)  {
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

  if(typeof(values) != "double" || !any(!is.wholenumber(values), na.rm = TRUE)) {
    return(data)
  }

  data[[column]] <- round(values)

  warning(message, call. = FALSE)

  return(data)

}
