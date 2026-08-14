# autospc_chart class

#' Construct an autospc_chart object from an already-assembled list.
#'
#' @return An object of class `c(class, "autospc_chart")`.
#' @noRd
new_autospc_chart <- function(x = list(),
                              class = character()) {
  
  stopifnot(is.list(x))
  
  return(
    structure(x,
              class = c(class,
                        "autospc_chart"))
  )
  
}


#' Validate an autospc_chart object
#'
#' Checks internal consistency and returns the object unchanged. Called by each
#' subclass validator, which runs its own checks first and then delegates here.
#'
#' **Class contract.** A validated `autospc_chart` object is guaranteed to be:
#'
#' - a list whose class vector is `c(<subclass>, "autospc_chart")`, in that
#'   order
#' - carrying every element named by `autospc_chart_elements()`.
#'
#' Additional elements are permitted - subclasses add their own
#'
#' **`data` and `data_original`.** `data_original` is what the user supplied,
#' untouched. `data` is prepared input: its columns are named `x`, `y` and,
#' where the class has one, `n` from the moment the object exists, and after
#' `prepare_data()` its `y` is *the series under analysis*, which is not always
#' the column the user passed. For MR it holds the moving ranges; for P and P'
#' it holds percentages, with the counts kept as `y_numerator`.
#'
#' @return `x`, unchanged, if valid; otherwise an error.
#' @noRd
validate_autospc_chart <- function(x) {
  
  if(!inherits(x, "autospc_chart")) {
    stop("Not an autospc_chart object.", call. = FALSE)
  }
  
  element_names <- names(x)
  
  
  element_check <- autospc_chart_elements() %in% element_names
  if(!all(element_check)) {
    stop(paste("Malformed autospc_chart object - element(s) not present:",
               paste(autospc_chart_elements()[!element_check],
                     collapse = ", ")),
         call. = FALSE)
  }
  
  return(x)
  
}


#' Elements common to all autospc_chart objects
#'
#' These names are duplicated by `autospc_chart_list()`, which assembles
#' exactly these elements - adding one means adding it in both places.
#' 
#' data_original is a derived field retaining (by convention only) a copy of the
#' data passed by the user
#'
#' @return A character vector of element names.
#' @noRd
autospc_chart_elements <- function() {
  
  chart_elements <- c(
    "data",
    "x",
    "y",
    "period_min",
    "baseline_length",
    "shift_rule_threshold",
    "baseline_only",
    "establish_every_shift",
    "no_regrets",
    "overhanging_reversions",
    "max_exclusions",
    "mr_screen_max_loops",
    "centre_line_tolerance",
    "data_original"
  )
  
  return(chart_elements)
  
}


#' Chart types accepted by autospc()
#'
#' The single source of truth for the user-facing chart_type values.
#'
#' @return A character vector of chart types.
#' @noRd
autospc_chart_types <- function() {

  chart_types <- c("XMR", "MR", "C", "C'", "P", "P'")

  return(chart_types)

}


#' Which chart type's class serves this request?
#'
#' TEMPORARY - see CLEAN UP #16 in the worklist. Removed, along with the `if`
#' around the `autospc_chart()` call in `autospc()`, once an object can be
#' created for every chart type.
#'
#' `"XMR"` maps to `"X"`. It asks for a pair of charts, and the MR half is
#' already created by the `chart_type = "MR"` re-invocation in `autospc()`, so
#' the X half is all that is needed here. Every other chart type maps to
#' itself.
#'
#' `chart_type` has not been checked at the point `autospc()` calls this, so it
#' may be anything the user passed, including several values at once. Those
#' return NULL rather than erroring, so that `validate_chart_type()` still
#' produces the error message.
#'
#' @return A character scalar, or NULL if no object can be created.
#' @noRd
chart_type_for_object <- function(chart_type) {

  if(!is.character(chart_type) || length(chart_type) != 1) {
    return(NULL)
  }

  if(identical(chart_type, "XMR")) {
    return("X")
  }

  if(chart_type %in% autospc_chart_types()) {
    return(chart_type)
  }

  return(NULL)

}


#' Create an autospc_chart object of the class given by chart_type
#'
#' Only the P and P' branches use `n`, and R does not evaluate an argument that
#' nothing looks at, so `n` may be left out for the other chart types.
#'
#' The final `stop()` is the default branch. Without it a chart type with no
#' matching branch would return NULL without printing anything.
#'
#' @return An object of a subclass of `"autospc_chart"`.
#' @noRd
autospc_chart <- function(chart_type,
                          data,
                          x,
                          y,
                          n,
                          ...) {

  autospc_chart_object <- switch(
    chart_type,
    "C"  = autospc_chart_c(data = data, x = x, y = y, ...),
    "C'" = autospc_chart_cp(data = data, x = x, y = y, ...),
    "P"  = autospc_chart_p(data = data, x = x, y = y, n = n, ...),
    "P'" = autospc_chart_pp(data = data, x = x, y = y, n = n, ...),
    "X"  = autospc_chart_x(data = data, x = x, y = y, ...),
    "MR" = autospc_chart_mr(data = data, x = x, y = y, ...),
    stop("No autospc_chart class for chart_type: ", chart_type, call. = FALSE)
  )

  return(autospc_chart_object)

}


#' Assemble the elements common to all autospc_chart objects
#'
#' Assembles the shared elements and returns a plain, unclassed list, which each
#' subclass helper then appends to, constructs from and validates. Defaults for
#' the shared elements live here.
#'
#' The signature is deliberately **closed**
#'
#' @return A named list holding the elements given by
#'   `autospc_chart_elements()`.
#' @noRd
autospc_chart_list <- function(data,
                               x,
                               y,
                               period_min = 21L,
                               baseline_length = NULL,
                               shift_rule_threshold = 8L,
                               baseline_only = FALSE,
                               establish_every_shift = FALSE,
                               no_regrets = TRUE,
                               overhanging_reversions = TRUE,
                               max_exclusions = 3L,
                               mr_screen_max_loops = 1L,
                               centre_line_tolerance = 0) {
  
  autospc_chart_l <- list(
    data = data,
    x = x,
    y = y,
    period_min = period_min,
    baseline_length = baseline_length,
    shift_rule_threshold = shift_rule_threshold,
    baseline_only = baseline_only,
    establish_every_shift = establish_every_shift,
    no_regrets = no_regrets,
    overhanging_reversions = overhanging_reversions,
    max_exclusions = max_exclusions,
    mr_screen_max_loops = mr_screen_max_loops,
    centre_line_tolerance = centre_line_tolerance,
    # derived fields
    data_original = data
  )
  
  return(autospc_chart_l)
  
}


# Methods


#' Aggregate data for analysis
#' 
#' Returns the chart object unchanged, this reflects that the default behaviour
#' is no aggregation, unless overridden by specific subclass methods
#'
#' @return autospc_chart object of the same class as chart
#' @noRd
aggregate_data.autospc_chart <- function(chart) {

  return(chart)

}


#' Turn the aggregated data into the series the algorithm analyses
#'
#' Returns the chart unchanged. Overridden by the classes that analyse something
#' other than the column the user supplied.
#'
#' @return autospc_chart object of the same class as chart
#' @noRd
prepare_data.autospc_chart <- function(chart) {

  return(chart)

}


#' Number of points available for analysis
#'
#' The non-missing values of `y`.
#'
#' @return integer
#' @noRd
n_effective_points.autospc_chart <- function(chart,
                                             data) {

  points <- data %>%
    dplyr::filter(!is.na(y)) %>%
    nrow()

  return(points)

}


#' Columns the limits table carries in addition to the common ones
#'
#' None by default. Overridden by the classes whose limits are calculated from
#' something other than the plotted `y`.
#'
#' @return character vector, possibly empty
#' @noRd
limits_table_columns.autospc_chart <- function(chart) {

  return(character(0))

}


#' Extend the limits of the preceding calculation period over the display period
#'
#' Carries the last calculated centre line and limits forward unchanged.
#' Overridden by the classes whose limits vary with the denominator.
#'
#' @return `limits_table`, with the display rows filled in
#' @noRd
extend_display_limits.autospc_chart <- function(chart,
                                                limits_table,
                                                counter) {

  display_rows <- counter:nrow(limits_table)
  last_calculated <- counter - 1

  limits_table[display_rows, "ucl"] <- limits_table[last_calculated, "ucl"]
  limits_table[display_rows, "lcl"] <- limits_table[last_calculated, "lcl"]
  limits_table[display_rows, "cl"] <- limits_table[last_calculated, "cl"]
  limits_table[display_rows, "periodType"] <- "display"

  return(limits_table)

}


#' Limits to use beyond the end of the data
#'
#' The limits of the final calculation period, which are constant within it.
#' Overridden by the classes whose limits vary with the denominator.
#'
#' @return list of single values, named cl, lcl and ucl
#' @noRd
extrapolate_limits.autospc_chart <- function(chart,
                                             period) {

  limits <- period %>%
    dplyr::select(cl, lcl, ucl) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(),
                                   ~ mean(.x,
                                          na.rm = TRUE))) %>%
    as.list()

  return(limits)

}


# Presentation methods

#' Row that carries the first centre line label
#'
#' @return integer, row number
#' @noRd
first_label_row.autospc_chart <- function(chart) {

  return(1L)

}


#' Rounding accuracy for centre line labels
#'
#' Whole numbers by default. Overridden by the classes labelled as percentages
#' and by those whose accuracy follows the scale of the data.
#'
#' @return number, passed to scales::number(accuracy =)
#' @noRd
label_accuracy.autospc_chart <- function(chart,
                                         ylimhigh) {

  return(1)

}


#' Lower and upper ends of the y axis
#'
#' Zero to 110, the range for a chart labelled as a percentage. Overridden by
#' the classes whose axis follows the data.
#'
#' @return list of two numbers, low and high
#' @noRd
y_axis_range.autospc_chart <- function(chart,
                                       data) {

  return(list(low = 0,
              high = 110))

}
