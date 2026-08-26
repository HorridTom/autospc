# autospc_plot class
#
# An autospc_plot IS a ggplot: its class vector begins with "autospc_plot" and
# ggplot2's own classes follow it, so printing, ggsave() and adding ggplot2
# layers all keep working. What it adds is the analysed chart or charts it was
# drawn from, and how it was drawn - the visualisation parameters, and the axis
# extents worked out from them.
#
# The slots are read with [[ rather than by name. From ggplot2 4.0.0 a ggplot
# is an S7 object, whose names() is empty although [[ and $ still reach what
# was assigned to it; up to ggplot2 3.5 it is a list and either works.
#
# Everything in the package that depends on a ggplot carrying slots lives in
# this file. new_autospc_plot() writes the class vector and the slots; the
# accessors read them; nothing else touches either.


#' Construct an autospc_plot from a built ggplot
#'
#' `charts` is a list even when it holds one chart. An XmR plot holds two, and a
#' faceted plot holds one per facet - in both cases one ggplot, drawn from
#' several analysed charts.
#'
#' The class is prepended rather than replaced, so the object remains a ggplot
#' to everything that dispatches on `"gg"` or `"ggplot"`.
#'
#' @return An object whose class vector begins `"autospc_plot"`, followed by
#'   ggplot2's own classes.
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
#' Nothing is guaranteed about *which* parameters or axis extents are present.
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

  # Read with [[ rather than names(): from ggplot2 4.0.0 a ggplot is an S7
  # object, whose names() is empty even though [[ still reads what is there.
  element_check <- vapply(autospc_plot_elements(),
                          function(element) !is.null(x[[element]]),
                          logical(1))

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
#' `visualisation_params` is what the caller asked for; `axis_extents` is what
#' was worked out from that and the charts. A value that is both - an axis end
#' the caller set - appears in each, as asked for and as used.
#'
#' Every parameter and every axis extent goes inside one of these, rather than
#' becoming an element of its own.
#'
#' @return A character vector of element names.
#' @noRd
autospc_plot_presentation_elements <- function() {

  presentation_elements <- c(
    "visualisation_params",
    "axis_extents"
  )

  return(presentation_elements)

}


#' The visualisation parameters a plot is drawn with
#'
#' The visualisation half of the argument split, and the single definition of
#' it. `autospc()` and `facet_stages()` select their `visualisation_params` from
#' the arguments of the call by exactly these names.
#'
#' A name here that `autospc()` does not take gives a NULL rather than an error,
#' so `test-chart_arguments.R` tests that each parameter named here is recorded
#' on the plot object.
#'
#' @return A character vector of parameter names.
#' @noRd
visualisation_param_names <- function() {

  parameter_names <- c(
    "show_limits",
    "title",
    "subtitle",
    "use_caption",
    "override_x_title",
    "override_y_title",
    "override_y_lim",
    "x_break",
    "x_date_format",
    "x_pad_end",
    "extend_limits_to",
    "r1_col",
    "r2_col",
    "point_size",
    "line_width_sf",
    "highlight_exclusions",
    "include_annotations",
    "basic_annotations",
    "annotation_size",
    "align_labels",
    "flip_labels",
    "upper_annotation_sf",
    "lower_annotation_sf",
    "annotation_arrows",
    "annotation_arrow_curve"
  )

  return(parameter_names)

}


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


#' Resolve the visualisation parameters whose default depends on the chart
#'
#' The title and subtitle, which come from columns of the data where the caller
#' gave none, and the two annotation scale factors, which come from the chart
#' type. A value the caller passed wins over all four. Called once per call.
#'
#' @param visualisation_params A named list of the visualisation parameters, as
#'   the caller gave them.
#' @param chart An `autospc_chart`.
#'
#' @return `visualisation_params`, with those four resolved.
#' @noRd
resolve_default_visualisation_params <- function(visualisation_params,
                                                 chart) {

  titles <- titles_from_data(data = chart$data_original,
                             title = visualisation_params$title,
                             subtitle = visualisation_params$subtitle)

  # Assigned as single-element lists so that a NULL sets the element rather than
  # deleting it.
  visualisation_params["title"]    <- list(titles$title)
  visualisation_params["subtitle"] <- list(titles$subtitle)

  # The lower factor is the mirror image of the upper about 1.
  if(is.null(visualisation_params$upper_annotation_sf)) {
    visualisation_params$upper_annotation_sf <-
      upper_annotation_sf_default(chart)
  }

  if(is.null(visualisation_params$lower_annotation_sf)) {
    visualisation_params$lower_annotation_sf <-
      2 - visualisation_params$upper_annotation_sf
  }

  return(visualisation_params)

}


#' Draw an autospc_plot from the charts it is made of
#'
#' Draw, construct, validate, return. The analysed charts and the presentation
#' go in; the plot object comes out, carrying the charts and the presentation
#' it was drawn with.
#'
#' The charts are in drawing order, so the first is the chart the plot is drawn
#' from and the second, for an XmR pair, is its moving range panel. A faceted
#' plot is drawn from every facet at once.
#'
#' A series with limits is drawn as an SPC chart, and one without as a plain
#' time series - which draws the first chart alone, so that is the chart the
#' object carries. A faceted plot is always drawn as an SPC chart.
#'
#' `main` below is the plot data the plot is drawn from: the location chart of a
#' pair, the only chart of a single chart plot, or every facet at once of a
#' faceted one.
#'
#' @param charts A list of analysed `autospc_chart` objects.
#' @param visualisation_params A named list of the visualisation parameters.
#'   The axis titles are taken from the plot data that is drawn, so that the
#'   object records what is drawn.
#' @param split_rows Non-NULL to facet by stage.
#'
#' @return An object whose class vector begins `"autospc_plot"`, followed by
#'   ggplot2's own classes.
#' @noRd
autospc_plot <- function(charts,
                         visualisation_params,
                         split_rows = NULL) {

  if(inherits(charts, "autospc_chart")) {
    stop(paste("charts must be a list of autospc_chart objects, not a single",
               "chart."),
         call. = FALSE)
  }

  plot_data <- build_plot_data(charts = charts,
                               visualisation_params = visualisation_params)

  if(!is.null(split_rows)) {
    plot_data <- list(faceted_plot_data(
      plot_data = plot_data,
      visualisation_params = visualisation_params
    ))
  }

  main <- plot_data[[1]]

  visualisation_params["override_x_title"] <- list(main$axis_titles$x)
  visualisation_params["override_y_title"] <- list(main$axis_titles$y)

  limits_drawn <- isTRUE(visualisation_params$show_limits) &&
    centre_line_present(main$table)

  if(!limits_drawn && is.null(split_rows)) {

    charts <- charts[1]
    plot_data <- plot_data[1]

    plot <- create_timeseries_plot(table = main$table,
                                   visualisation_params = visualisation_params,
                                   axis_extents = main$axis_extents)

  } else {

    plot <- create_spc_plot(plot_data = plot_data,
                            visualisation_params = visualisation_params,
                            split_rows = split_rows)

  }

  autospc_plot_object <- new_autospc_plot(
    plot = plot,
    charts = charts,
    presentation = list(visualisation_params = visualisation_params,
                        axis_extents = main$axis_extents)
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
#' @return A list of two named lists, `visualisation_params` and
#'   `axis_extents`.
#' @noRd
autospc_plot_presentation <- function(plot) {

  return(plot$presentation)

}


#' The visualisation parameters an autospc_plot was drawn with
#'
#' @param parameter Optional name of a single parameter. A parameter that was
#'   not supplied returns `NULL`.
#'
#' @return The named list, or one element of it.
#' @noRd
autospc_plot_visualisation_params <- function(plot,
                                              parameter = NULL) {

  if(is.null(parameter)) {
    return(plot$presentation$visualisation_params)
  }

  return(plot$presentation$visualisation_params[[parameter]])

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
autospc_plot_axis_extents <- function(plot,
                                      value = NULL) {

  if(is.null(value)) {
    return(plot$presentation$axis_extents)
  }

  return(plot$presentation$axis_extents[[value]])

}


#' The analysis behind an autospc_plot
#'
#' The result of each chart the plot holds, in one table.
#'
#' An XmR pair is one analysis of one series shown as two charts, so it goes
#' out wide: the moving range and its limits join the X columns as `mr`, `amr`,
#' `url` and `lrl`. Several charts of the same type are separate analyses, so
#' they stack long, with `stage` identifying which each row came from - the same
#' column `facet_stages(plot_chart = FALSE)` returns, and the same name as the
#' facet variable, because `facet_stages()` is the only thing that produces
#' several charts of one type.
#'
#' This is the analytic result, not the table `autospc(plot_chart = FALSE)`
#' returns: it carries the columns the algorithm produced, and not the columns
#' `add_plot_columns()` adds for drawing.
#'
#' @param x An `autospc_plot`.
#' @param ... Ignored, for consistency with the generic.
#'
#' @return A data frame.
#' @export
as.data.frame.autospc_plot <- function(x, ...) {

  charts <- autospc_plot_charts(x)

  results <- lapply(charts,
                    function(chart) chart$result$table)

  if(length(results) == 1L) {
    return(as.data.frame(results[[1]]))
  }

  if(is_xmr_pair(charts)) {
    return(as.data.frame(join_mr_columns(x_table = results[[1]],
                                         mr_table = results[[2]])))
  }

  return(as.data.frame(dplyr::bind_rows(results, .id = "stage")))

}
