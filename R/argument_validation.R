validate_chart_type <- function(chart_type) {
  
  allowed_chart_types <- c("XMR", "MR", "C", "C'", "P", "P'")
  
  # NULL check
  if (is.null(chart_type)) {
    stop(
      "chart_type must be provided. Available chart types are: ",
      paste(allowed_chart_types, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  # Length check
  if (length(chart_type) != 1) {
    stop(
      "chart_type must be a single value. ",
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
