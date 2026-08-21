# Preparing what a caller passed for the analysis and the drawing


#' The title and subtitle a plot is drawn with
#'
#' Where the data holds a `title` or `subtitle` column and the corresponding
#' argument is NULL, the value in the first row of that column is used. The
#' argument takes precedence when both are given.
#'
#' `data` here is the data frame the caller passed, not `chart$data`, which
#' holds only the columns the analysis uses.
#'
#' @return A list of `title` and `subtitle`, either of which may be NULL.
#' @noRd
titles_from_data <- function(data,
                             title = NULL,
                             subtitle = NULL) {

  if(is.null(title) & "title" %in% colnames(data)) {
    title <- data$title[1]
  }

  if(is.null(subtitle) & "subtitle" %in% colnames(data)) {
    subtitle <- data$subtitle[1]
  }

  return(list(title = title,
              subtitle = subtitle))

}


#' Warn where x is not a type the axis can be built from
#'
#' Date, POSIXct, numeric and integer are what the scales in `visualisation.R`
#' know how to draw. Anything else is a warning rather than an error, because
#' the analysis itself only needs x to order the rows.
#'
#' @return invisible TRUE
#' @noRd
check_x_type <- function(data) {

  xType <- class(data$x)

  if(all(xType != "Date") &
     all(xType!= c("POSIXct", "POSIXt")) &
     all(xType != "numeric") &
     all(xType != "integer")) {
    warning(paste("Please make sure that your x column is a",
                  "'Date', 'POSIXct', 'numeric' or 'integer' type."))
  }

  invisible(TRUE)

}


#' Order a chart's series by x, and make it a plain data frame
#'
#' The algorithm walks the data in row order, so the rows have to be in x order
#' before it runs, and before `prepare_data()` derives anything from their
#' order - an MR chart's moving ranges are differences between neighbouring
#' rows.
#'
#' `dplyr::arrange()` is stable, so rows sharing an x keep the order they
#' arrived in. Missing x values sort to the end.
#'
#' This is also where `data` becomes a plain data frame, and it is the only
#' place that does it. Every chart type passes through here, and it is after
#' `aggregate_data()`, which is what produces a tibble: `dplyr::summarise()`
#' returns one whatever it was given. Everything the algorithm derives from
#' `data` is therefore a plain data frame as well - the limits table, the
#' analysis in `chart$result$table`, and the tables recorded in
#' `chart$history`. `data_original` is left as the caller passed it.
#'
#' @return autospc_chart object of the same class as chart
#' @noRd
order_series <- function(chart) {

  chart$data <- chart$data %>%
    dplyr::arrange(x) %>%
    as.data.frame()

  return(chart)

}


