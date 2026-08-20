# autospc_chart_p class

#' Construct an autospc_chart_p object
#'
#' @return An object of class `c("autospc_chart_p", "autospc_chart")`.
#' @noRd
new_autospc_chart_p <- function(x) {
  
  return(
    new_autospc_chart(x,
                      class = "autospc_chart_p")
  )
  
}


#' Validate an autospc_chart_p object
#'
#' See `validate_autospc_chart()` for the class contract.
#'
#' @return `x`, unchanged, if valid; otherwise an error.
#' @noRd
validate_autospc_chart_p <- function(x) {
  
  if(!inherits(x, "autospc_chart_p")) {
    stop("Not an autospc_chart_p object.", call. = FALSE)
  }
  
  x <- validate_autospc_chart(x)
  
  element_names <- names(x)
  
  element_check <- autospc_chart_p_elements() %in% element_names
  
  if(!all(element_check)) {
    stop(paste("Malformed autospc_chart_p object - element(s) not present:",
               paste(autospc_chart_p_elements()[!element_check],
                     collapse = ", ")),
         call. = FALSE)
  }
  
  return(x)
  
}


#' Elements specific to autospc_chart_p objects
#'
#' Additional to those given by `autospc_chart_elements()`, which every chart
#' carries. `n` holds the name of the denominator column.
#'
#' @return A character vector of element names.
#' @noRd
autospc_chart_p_elements <- function() {
  
  chart_elements <- c(
    "n"
  )
  
  return(chart_elements)
  
}


#' Create an autospc_chart_p object
#'
#' Helper for P charts: assemble, construct, validate, return.
#'
#' @return An object of class `c("autospc_chart_p", "autospc_chart")`.
#' @noRd
autospc_chart_p <- function(data,
                            x,
                            y,
                            n,
                            ...) {
  
  autospc_chart_p_l <- autospc_chart_list(data = data,
                                          x = x,
                                          y = y,
                                          ...)
  autospc_chart_p_l <- c(autospc_chart_p_l,
                         list(n = n))
  
  autospc_chart_p_l <- normalise_columns(autospc_chart_p_l,
                                         fields = c("x", "y", "n"))

  autospc_chart_p_object <- new_autospc_chart_p(autospc_chart_p_l)
  
  autospc_chart_p_object <- validate_autospc_chart_p(autospc_chart_p_object)
  
  return(autospc_chart_p_object)
  
}


# Analysis methods


#' Aggregate data for analysis
#' 
#' Sums y and n (counts) over x (subgroup) as needed for P-chart analysis.
#' Data may be provided as either pre-aggregated counts for y and n or
#' individual binary observations
#'
#' @return autospc_chart_p object
#' @noRd
aggregate_data.autospc_chart_p <- function(chart) {

  return(
    aggregate_ratios(chart,
                     allow_individual_observations = TRUE)
  )

}


#' Turn the aggregated data into the series the algorithm analyses
#'
#' A P chart plots percentages, so `y` becomes the percentage and the count it
#' was calculated from is kept as `y_numerator`. Division by a zero or missing
#' denominator gives `NA` rather than `NaN` or `Inf`.
#'
#' @return autospc_chart object of the same class as chart
#' @noRd
prepare_data.autospc_chart_p <- function(chart) {

  chart$data <- chart$data %>%
    dplyr::mutate(y_numerator = y) %>%
    dplyr::mutate(y = y * 100 / n) %>%
    dplyr::mutate(y = dplyr::if_else(is.nan(y) | is.infinite(y),
                                     as.numeric(NA),
                                     y))

  return(chart)

}


#' Calculate control limits for a subset of P-chart data
#' 
#' Centre line and limits are established using standard formulae based on the
#' Binomial distribution (see e.g. Provost and Murray) for non-excluded data
#' points
#'
#' @return list of three vectors (cl, ucl, lcl), each the same length as period
#' @noRd
calculate_limits.autospc_chart_p <- function(chart,
                                             period,
                                             exclusion_points) {
  
  limits <- get_p_limits(y = period$y_numerator,
                         n = period$n,
                         exclusion_points = exclusion_points,
                         multiply = 100)
  
  return(limits)
  
}


#' Columns the limits table carries in addition to the common ones
#'
#' `y` holds percentages for this class, so the counts and denominators the
#' limits were calculated from have to be kept as well.
#'
#' @return character vector
#' @noRd
limits_table_columns.autospc_chart_p <- function(chart) {

  return(c("n", "y_numerator"))

}


#' Extend the limits of the preceding calculation period over the display period
#'
#' The limits of a P chart depend on the denominator, so they cannot simply be
#' carried forward. The width of the last calculated period is expressed as a
#' constant, and the display limits are recomputed from it at each point's own
#' denominator. The centre line is carried forward unchanged.
#'
#' @return `limits_table`, with the display rows filled in
#' @noRd
extend_display_limits.autospc_chart_p <- function(chart,
                                                  limits_table,
                                                  counter) {

  #constant from P' chart calc = (UCL - CL)sqrt(n)
  constant <- (limits_table[(counter - 1), "ucl"] -
                 limits_table[(counter - 1), "cl"]) *
    sqrt(limits_table[(counter - 1), "n"])
  pbar <- limits_table[(counter - 1), "cl"]

  limits_table[counter:nrow(limits_table), "cl"] <-
    limits_table[(counter - 1), "cl"]
  limits_table[counter:nrow(limits_table), "periodType"] <- "display"

  #splits limits table to just the section that we want
  limits_table_top <- limits_table[1:(counter - 1),]
  limits_table_bottom <- limits_table[counter:nrow(limits_table),]

  limits_table_bottom <- limits_table_bottom %>%
    dplyr::mutate(constant = as.numeric(constant)) %>%
    dplyr::mutate(pbar = as.numeric(pbar)) %>%
    dplyr::mutate(ucl_display = pbar + (constant/sqrt(n)) ) %>%
    dplyr::mutate(lcl_display = pbar - (constant/sqrt(n)) ) %>%
    dplyr::mutate(ucl = dplyr::if_else(periodType == "display",
                                       ucl_display,
                                       ucl)) %>%
    dplyr::mutate(lcl = dplyr::if_else(periodType == "display",
                                       lcl_display,
                                       lcl)) %>%
    dplyr::mutate(ucl = dplyr::if_else(ucl >= 100, 100, ucl)) %>%
    dplyr::mutate(lcl = dplyr::if_else(lcl <= 0, 0, lcl))

  limits_table <- dplyr::bind_rows(limits_table_top, limits_table_bottom)

  return(limits_table)

}


#' Limits to use beyond the end of the data
#'
#' The limits of a P chart vary with the denominator, so there is no single set
#' to carry forward. They are recalculated from the final calculation period
#' with every denominator replaced by the period's mean, giving one set of
#' values for the whole extension.
#'
#' @return list of single values, named cl, lcl and ucl
#' @noRd
extrapolate_limits.autospc_chart_p <- function(chart,
                                               period) {

  ext_calc_data <- period %>%
    dplyr::mutate(y = (y/100)*n,
                  n = dplyr::if_else(is.na(n),
                                     NA_real_,
                                     mean(n,
                                          na.rm = TRUE)))

  exclusion_points <- ext_calc_data %>%
    dplyr::pull(excluded) %>%
    which()

  limits <- get_p_limits(y = ext_calc_data$y,
                         n = ext_calc_data$n,
                         exclusion_points = exclusion_points,
                         multiply = 100) %>%
    lapply("[[", 1L)

  return(limits)

}

# Presentation methods


#' The centre line label, formatted
#'
#' A per cent sign, and no thousands separator: the values are percentages.
#'
#' @return character
#' @noRd
centre_line_label.autospc_chart_p <- function(chart,
                                    cl,
                                    ylimhigh) {

  return(scales::number(cl,
                        accuracy = label_accuracy(chart = chart,
                                                  ylimhigh = ylimhigh),
                        suffix = "%"))

}


#' Chart name
#'
#' @return string, name of chart for labels
#' @noRd
chart_type_label.autospc_chart_p <- function(chart) {
  return("P")
}


#' Rounding accuracy for centre line labels
#'
#' @return number, passed to scales::number(accuracy =)
#' @noRd
label_accuracy.autospc_chart_p <- function(chart,
                                           ylimhigh) {

  return(0.1)

}


#' Default vertical position of centre line labels
#'
#' A P chart plots percentages, so the axis runs on a scale of 0 to 100 whatever
#' the measure is, and the superclass's tenth would put the label far above the
#' limit it belongs to.
#'
#' @return number, the scale factor applied to the upper control limit
#' @noRd
upper_annotation_sf_default.autospc_chart_p <- function(chart) {

  return(1.04)

}


#' Retrieve default y axis label
#'
#' @return string
#' @noRd
y_axis_title.autospc_chart_p <- function(chart) {
  return("Percentage")
}
