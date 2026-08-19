# autospc_plot class
#
# An autospc_plot IS a ggplot: its class vector is c("autospc_plot", "gg",
# "ggplot"), so printing, ggsave() and adding ggplot2 layers all keep working.
# What it adds is the fitted chart or charts it was drawn from, and how it was
# drawn - the presentation parameters passed, and the values derived from them.
#
# Everything in the package that depends on a ggplot being an S3 list lives in
# this file. new_autospc_plot() writes the class vector and the slots; the
# accessors read them; nothing else touches either. When the ggplot2 minimum
# version rises to 4.0 the slots will become S7 properties, and this file is
# what will change.


#' Construct an autospc_plot from a built ggplot
#'
#' `charts` is a list even when it holds one chart. An XmR plot holds two, and a
#' faceted plot holds one per facet - in both cases one ggplot, drawn from
#' several fitted charts.
#'
#' The class is prepended rather than replaced, so the object remains a ggplot
#' to everything that dispatches on `"gg"` or `"ggplot"`.
#'
#' @return An object of class `c("autospc_plot", "gg", "ggplot")`.
#' @noRd
new_autospc_plot <- function(plot,
                             charts,
                             presentation) {

  plot$charts <- charts
  plot$presentation <- presentation

  class(plot) <- c("autospc_plot", class(plot))

  return(plot)

}


#' Validate an autospc_plot object
#'
#' **Class contract.** A validated `autospc_plot` is guaranteed to be:
#'
#' - a ggplot, whose class vector starts with `"autospc_plot"` and carries it
#'   exactly once
#' - carrying every element named by `autospc_plot_elements()`
#' - `charts`: a list of at least one validated `autospc_chart`
#' - `presentation`: a list carrying exactly the elements named by
#'   `autospc_plot_presentation_elements()`, each a named list, possibly empty
#'
#' Nothing is guaranteed about *which* parameters or derived values are present.
#'
#' @return `x`, unchanged, if valid; otherwise an error.
#' @noRd
validate_autospc_plot <- function(x) {

  if(!inherits(x, "autospc_plot")) {
    stop("Not an autospc_plot object.", call. = FALSE)
  }

  if(!inherits(x, "ggplot")) {
    stop("Malformed autospc_plot object - it is not a ggplot.", call. = FALSE)
  }

  if(!identical(class(x)[1], "autospc_plot")) {
    stop(paste("Malformed autospc_plot object - autospc_plot must come first",
               "in the class vector."),
         call. = FALSE)
  }

  if(sum(class(x) == "autospc_plot") != 1L) {
    stop(paste("Malformed autospc_plot object - autospc_plot appears more than",
               "once in the class vector."),
         call. = FALSE)
  }

  element_names <- names(x)

  element_check <- autospc_plot_elements() %in% element_names
  if(!all(element_check)) {
    stop(paste("Malformed autospc_plot object - element(s) not present:",
               paste(autospc_plot_elements()[!element_check],
                     collapse = ", ")),
         call. = FALSE)
  }

  if(!is.list(x$charts) || length(x$charts) < 1L) {
    stop("Malformed autospc_plot object - charts must be a list of at least one.",
         call. = FALSE)
  }

  chart_check <- vapply(x$charts,
                        function(chart) inherits(chart, "autospc_chart"),
                        logical(1))

  if(!all(chart_check)) {
    stop(paste("Malformed autospc_plot object - every element of charts must",
               "be an autospc_chart."),
         call. = FALSE)
  }

  if(!is.list(x$presentation)) {
    stop("Malformed autospc_plot object - presentation must be a list.",
         call. = FALSE)
  }

  presentation_check <- autospc_plot_presentation_elements() %in%
    names(x$presentation)

  if(!all(presentation_check)) {
    stop(paste("Malformed autospc_plot object - presentation element(s) not",
               "present:",
               paste(autospc_plot_presentation_elements()[!presentation_check],
                     collapse = ", ")),
         call. = FALSE)
  }

  for(half in autospc_plot_presentation_elements()) {

    values <- x$presentation[[half]]

    if(!is.list(values)) {
      stop(paste0("Malformed autospc_plot object - presentation$", half,
                  " must be a list."),
           call. = FALSE)
    }

    if(length(values) > 0L &&
       (is.null(names(values)) || any(names(values) == ""))) {
      stop(paste0("Malformed autospc_plot object - presentation$", half,
                  " must be named."),
           call. = FALSE)
    }

  }

  return(x)

}


#' Elements an autospc_plot carries in addition to a ggplot's own
#'
#' @return A character vector of element names.
#' @noRd
autospc_plot_elements <- function() {

  plot_elements <- c(
    "charts",
    "presentation"
  )

  return(plot_elements)

}


#' The two halves of an autospc_plot's presentation
#'
#' `passed` is what the caller asked for; `derived` is what was worked out from
#' that and the charts. A value that is both - an axis end the caller set -
#' appears in each, as asked for and as used.
#'
#' Every parameter and every derived value goes inside one of these, rather than
#' becoming an element of its own.
#'
#' @return A character vector of element names.
#' @noRd
autospc_plot_presentation_elements <- function() {

  presentation_elements <- c(
    "passed",
    "derived"
  )

  return(presentation_elements)

}


#' Create an autospc_plot object
#'
#' Assemble, construct, validate, return - the same shape as the chart
#' construction helpers.
#'
#' @param plot A built ggplot. For an XmR pair this is the combined plot.
#' @param charts A list of validated `autospc_chart` objects.
#' @param passed A named list of the presentation parameters the plot was drawn
#'   with.
#' @param derived A named list of the values worked out for the drawing - the
#'   axis extents.
#'
#' @return An object of class `c("autospc_plot", "gg", "ggplot")`.
#' @noRd
autospc_plot <- function(plot,
                         charts,
                         passed = list(),
                         derived = list()) {

  if(inherits(charts, "autospc_chart")) {
    stop(paste("charts must be a list of autospc_chart objects, not a single",
               "chart."),
         call. = FALSE)
  }

  autospc_plot_object <- new_autospc_plot(
    plot = plot,
    charts = charts,
    presentation = list(passed = passed,
                        derived = derived)
  )

  autospc_plot_object <- validate_autospc_plot(autospc_plot_object)

  return(autospc_plot_object)

}


#' The charts an autospc_plot was drawn from
#'
#' @return A list of `autospc_chart` objects.
#' @noRd
autospc_plot_charts <- function(plot) {

  return(plot$charts)

}


#' How an autospc_plot was drawn
#'
#' @return A list of two named lists, `passed` and `derived`.
#' @noRd
autospc_plot_presentation <- function(plot) {

  return(plot$presentation)

}


#' The presentation parameters an autospc_plot was drawn with
#'
#' @param parameter Optional name of a single parameter. A parameter that was
#'   not supplied returns `NULL`.
#'
#' @return The named list, or one element of it.
#' @noRd
autospc_plot_passed <- function(plot,
                                parameter = NULL) {

  if(is.null(parameter)) {
    return(plot$presentation$passed)
  }

  return(plot$presentation$passed[[parameter]])

}


#' The values worked out for the drawing
#'
#' The axis extents, as the plot was drawn with them. They are recorded rather
#' than recomputed, so that what the object reports cannot disagree with what
#' was drawn.
#'
#' Quantities that follow from the chart alone are not here - the point count,
#' for one. The chart is on the object and can be asked.
#'
#' @param value Optional name of a single value. A value that was not worked out
#'   returns `NULL`.
#'
#' @return The named list, or one element of it.
#' @noRd
autospc_plot_derived <- function(plot,
                                 value = NULL) {

  if(is.null(value)) {
    return(plot$presentation$derived)
  }

  return(plot$presentation$derived[[value]])

}


#' The analysis results behind an autospc_plot
#'
#' The analysis results of each chart the plot holds, bound into one frame.
#' Where the plot holds more than one chart, `chart` identifies which each row
#' came from.
#'
#' @param x An `autospc_plot`.
#' @param ... Ignored, for consistency with the generic.
#'
#' @return A data frame.
#' @export
as.data.frame.autospc_plot <- function(x, ...) {

  results <- lapply(autospc_plot_charts(x),
                    function(chart) chart$result$table)

  if(length(results) == 1L) {
    return(as.data.frame(results[[1]]))
  }

  return(as.data.frame(dplyr::bind_rows(results, .id = "chart")))

}
