# autospc_plot class
#
# An autospc_plot IS a ggplot: its class vector is c("autospc_plot", "gg",
# "ggplot"), so printing, ggsave() and adding ggplot2 layers all keep working.
# What it adds is the fitted chart or charts it was drawn from, and the
# presentation parameters it was drawn with.
#
# Everything in the package that depends on a ggplot being an S3 list lives in
# this file. new_autospc_plot() writes the class vector and the slots; the two
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
#' - `presentation`: a named list, possibly empty
#'
#' Nothing is guaranteed about *which* presentation parameters are present.
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

  if(length(x$presentation) > 0L &&
     (is.null(names(x$presentation)) || any(names(x$presentation) == ""))) {
    stop("Malformed autospc_plot object - presentation must be named.",
         call. = FALSE)
  }

  return(x)

}


#' Elements an autospc_plot carries in addition to a ggplot's own
#'
#' Every presentation parameter is held inside `presentation`, rather than
#' becoming an element of its own.
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


#' Create an autospc_plot object
#'
#' Assemble, construct, validate, return - the same shape as the chart
#' construction helpers.
#'
#' @param plot A built ggplot. For an XmR pair this is the combined plot.
#' @param charts A list of validated `autospc_chart` objects.
#' @param presentation A named list of the presentation parameters the plot was
#'   drawn with.
#'
#' @return An object of class `c("autospc_plot", "gg", "ggplot")`.
#' @noRd
autospc_plot <- function(plot,
                         charts,
                         presentation = list()) {

  if(inherits(charts, "autospc_chart")) {
    stop(paste("charts must be a list of autospc_chart objects, not a single",
               "chart."),
         call. = FALSE)
  }

  autospc_plot_object <- new_autospc_plot(plot = plot,
                                          charts = charts,
                                          presentation = presentation)

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


#' The presentation parameters an autospc_plot was drawn with
#'
#' @param parameter Optional name of a single parameter. A parameter that was
#'   not supplied returns `NULL`.
#'
#' @return The named list, or one element of it.
#' @noRd
autospc_plot_presentation <- function(plot,
                                      parameter = NULL) {

  if(is.null(parameter)) {
    return(plot$presentation)
  }

  return(plot$presentation[[parameter]])

}
