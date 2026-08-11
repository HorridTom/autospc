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


#' Create an autospc_chart object
#'
#' Provisional
#'
#' @return An object of class `"autospc_chart"`.
#' @noRd
autospc_chart <- function(data,
                          x,
                          y,
                          ...) {
  
  autospc_chart_l <- autospc_chart_list(data = data,
                                        x = x,
                                        y = y,
                                        ...)
  
  autospc_chart_object <- new_autospc_chart(autospc_chart_l)
  
  autospc_chart_object <- validate_autospc_chart(autospc_chart_object)
  
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



