
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


#' Stop unless a column is present in the data
#'
#' `message` is the error the caller wants the user to read, so each class can
#' name itself and the columns it requires.
#'
#' @return invisible TRUE, or an error carrying `message`
#' @noRd
require_column <- function(data,
                           column,
                           message) {

  if(!column %in% colnames(data)) {
    stop(message, call. = FALSE)
  }

  invisible(TRUE)

}


#' Stop unless a column is one of the given types
#'
#' Types are as `typeof()` gives them, so a count column is "integer" or
#' "double" and a column of individual binary observations is "logical".
#'
#' @return invisible TRUE, or an error carrying `message`
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


#' Round a count column, warning when it has to
#'
#' A column already holding whole numbers is left exactly as it is, integer or
#' double, so the warning fires only where a value actually changes.
#'
#' @return `data`, with `column` rounded where it was not whole
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
