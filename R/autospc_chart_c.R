# autospc_chart_c class

#' Construct an autospc_chart_c object
#'
#' @return An object of class `c("autospc_chart_c", "autospc_chart")`.
#' @noRd
new_autospc_chart_c <- function(x) {
  
  return(
    new_autospc_chart(x,
                      class = "autospc_chart_c")
  )
  
}


#' Validate an autospc_chart_c object
#'
#' See `validate_autospc_chart()` for the class contract.
#'
#' @return `x`, unchanged, if valid; otherwise an error.
#' @noRd
validate_autospc_chart_c <- function(x) {
  
  if(!inherits(x, "autospc_chart_c")) {
    stop("Not an autospc_chart_c object.", call. = FALSE)
  }
  
  return(
    validate_autospc_chart(x)
  )
  
}


#' Create an autospc_chart_c object
#'
#' Helper for C charts: assemble, construct, validate, return.
#'
#' @return An object of class `c("autospc_chart_c", "autospc_chart")`.
#' @noRd
autospc_chart_c <- function(data,
                            x,
                            y,
                            ...) {
  
  autospc_chart_c_l <- autospc_chart_list(data = data,
                                          x = x,
                                          y = y,
                                          ...)
  
  autospc_chart_c_object <- new_autospc_chart_c(autospc_chart_c_l)
  
  autospc_chart_c_object <- validate_autospc_chart_c(autospc_chart_c_object)
  
  return(autospc_chart_c_object)
  
}

