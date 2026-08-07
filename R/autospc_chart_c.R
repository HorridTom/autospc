# autospc_chart_c class

new_autospc_chart_c <- function(x) {
  
  return(
    new_autospc_chart(x,
                      class = "autospc_chart_c")
  )
  
}


validate_autospc_chart_c <- function(x) {
  
  if(!inherits(x, "autospc_chart_c")) {
    stop("Not an autospc_chart_c object.", call. = FALSE)
  }
  
  return(
    validate_autospc_chart(x)
  )
  
}


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

