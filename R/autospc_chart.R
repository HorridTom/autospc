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
      class = c(
        class,
        "autospc_chart"
      )
    )
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
#' untouched. `data` is prepared input. From the moment the object exists it
#' holds only the columns the analysis uses - `x`, `y` and, where the class has
#' one, `n` - renamed from the columns the user named for those arguments; those
#' columns meet the class's requirements on presence and type, and any counts
#' among them are whole numbers. After `prepare_data()` its `y` is *the series
#' under analysis*, which is not always the column the user passed. For MR it
#' holds the moving ranges; for P and P' it holds percentages, with the counts
#' kept as `y_numerator`.
#'
#' @return `x`, unchanged, if valid; otherwise an error.
#' @noRd
validate_autospc_chart <- function(x) {
  if (!inherits(x, "autospc_chart")) {
    stop("Not an autospc_chart object.", call. = FALSE)
  }

  element_names <- names(x)


  element_check <- autospc_chart_elements() %in% element_names
  if (!all(element_check)) {
    stop(
      paste(
        "Malformed autospc_chart object - element(s) not present:",
        paste(autospc_chart_elements()[!element_check],
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  return(x)
}


#' Elements common to all autospc_chart objects
#'
#' These names are duplicated by `assemble_chart_list()`, which assembles
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
    "aggregation_na_rm",
    "period_min",
    "baseline_length",
    "shift_rule_threshold",
    "baseline_only",
    "establish_every_shift",
    "no_regrets",
    "overhanging_reversions",
    "na_ends_run",
    "max_exclusions",
    "mr_screen_max_loops",
    "centre_line_tolerance",
    "floating_median",
    "floating_median_n",
    "keep_candidate_tables",
    "data_original",
    # the analysis
    "result",
    "history"
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
  chart_types <- c("XMR", "X", "MR", "C", "C'", "P", "P'")

  return(chart_types)
}


#' The chart parameters a caller can set
#'
#' The chart half of the argument split: the elements of an `autospc_chart` that
#' come from the caller, as opposed to the data, the column names, and the
#' elements the run fills in.
#'
#' @return A character vector of parameter names.
#' @noRd
autospc_chart_parameters <- function() {
  return(setdiff(
    autospc_chart_elements(),
    c(
      "data", "x", "y", "n",
      "data_original", "result", "history"
    )
  ))
}


#' The charts a chart type asks for
#'
#' `chart_type = "XMR"` asks for two charts, an X and an MR of the same series.
#' Every other chart type asks for one. This is the only place a chart type is
#' read as a string rather than dispatched on: everything after it holds chart
#' objects.
#'
#' **The charts come back in drawing order, so a pair is X then MR.**
#' `is_xmr_pair()`, the moving range panel and `as.data.frame()` all read that
#' order, and `facet_stages()` takes the first.
#'
#' Both halves of a pair are built from the same data, because neither X nor MR
#' aggregates and `prepare_data.autospc_chart_mr()` derives the moving ranges
#' from `y`.
#'
#' Callers pass a `chart_type` that `validate_chart_type()` has already
#' accepted, so anything reaching here is one of `autospc_chart_types()`.
#'
#' @return A list of one or two `autospc_chart` objects.
#' @noRd
build_charts <- function(chart_type,
                         data,
                         x,
                         y,
                         n,
                         ...) {
  if (identical(chart_type, "XMR")) {
    chart_types <- c(location = "X", dispersion = "MR")
  } else {
    chart_types <- chart_type
  }

  charts <- lapply(chart_types, function(type) {
    autospc_chart(
      chart_type = type,
      data = data,
      x = x,
      y = y,
      n = n,
      ...
    )
  })

  return(charts)
}


#' Are these two charts an XmR pair?
#'
#' An X chart and the MR chart of the same series, in that order - which is how
#' `autospc(chart_type = "XMR")` puts them on the plot object.
#'
#' @param charts A list of `autospc_chart` objects.
#'
#' @return TRUE or FALSE
#' @noRd
is_xmr_pair <- function(charts) {
  if (length(charts) != 2L) {
    return(FALSE)
  }

  return(inherits(charts[[1]], "autospc_chart_x") &&
    inherits(charts[[2]], "autospc_chart_mr"))
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
  autospc_chart_object <- switch(chart_type,
    "C" = autospc_chart_c(data = data, x = x, y = y, ...),
    "C'" = autospc_chart_cp(data = data, x = x, y = y, ...),
    "P" = autospc_chart_p(data = data, x = x, y = y, n = n, ...),
    "P'" = autospc_chart_pp(data = data, x = x, y = y, n = n, ...),
    "X" = autospc_chart_x(data = data, x = x, y = y, ...),
    "MR" = autospc_chart_mr(data = data, x = x, y = y, ...),
    stop("No autospc_chart class for chart_type: ", chart_type, call. = FALSE)
  )

  return(autospc_chart_object)
}


#' Assemble the elements common to all autospc_chart objects
#'
#' Assembles the shared elements and returns a plain, unclassed list, which each
#' subclass helper then appends to, constructs from and validates.
#'
#' Each default is read from the corresponding argument of `autospc()`, which is
#' where every default in the package is declared. Writing the values here as
#' well would mean two declarations that could disagree.
#'
#' The signature is deliberately **closed**
#'
#' @return A named list holding the elements given by
#'   `autospc_chart_elements()`.
#' @noRd
assemble_chart_list <- function(
  data,
  x,
  y,
  aggregation_na_rm = autospc_default("aggregation_na_rm"),
  period_min = autospc_default("period_min"),
  baseline_length = autospc_default("baseline_length"),
  shift_rule_threshold = autospc_default("shift_rule_threshold"),
  baseline_only = autospc_default("baseline_only"),
  establish_every_shift = autospc_default("establish_every_shift"),
  no_regrets = autospc_default("no_regrets"),
  overhanging_reversions = autospc_default("overhanging_reversions"),
  na_ends_run = autospc_default("na_ends_run"),
  max_exclusions = autospc_default("max_exclusions"),
  mr_screen_max_loops = autospc_default("mr_screen_max_loops"),
  centre_line_tolerance = autospc_default("centre_line_tolerance"),
  floating_median = autospc_default("floating_median"),
  floating_median_n = autospc_default("floating_median_n"),
  keep_candidate_tables = autospc_default("keep_candidate_tables")
) {
  # baseline_length is period_min unless the caller sets it, so that every
  # chart has a first calculation period length and the field is never NULL.
  if (is.null(baseline_length)) {
    baseline_length <- period_min
  }

  floating_median <- match_choice(floating_median, "floating_median")

  autospc_chart_l <- list(
    data = data,
    x = x,
    y = y,
    aggregation_na_rm = aggregation_na_rm,
    period_min = period_min,
    baseline_length = as.integer(baseline_length),
    shift_rule_threshold = shift_rule_threshold,
    baseline_only = baseline_only,
    establish_every_shift = establish_every_shift,
    no_regrets = no_regrets,
    overhanging_reversions = overhanging_reversions,
    na_ends_run = na_ends_run,
    max_exclusions = max_exclusions,
    mr_screen_max_loops = mr_screen_max_loops,
    centre_line_tolerance = centre_line_tolerance,
    floating_median = floating_median,
    floating_median_n = floating_median_n,
    keep_candidate_tables = keep_candidate_tables,
    # derived fields
    data_original = data,
    # the analysis, empty until the algorithm runs
    result = list(),
    history = list()
  )

  return(autospc_chart_l)
}


# Methods


#' Round the count columns to whole numbers
#'
#' Returns the chart unchanged, because for chart types whose y is a measurement
#' rather than a count there is nothing to round. The count charts override
#' this.
#'
#' @return autospc_chart object of the same class as chart
#' @noRd
round_counts.autospc_chart <- function(chart) {
  return(chart)
}


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


#' @noRd
observed_rows.autospc_chart <- function(chart,
                                        data) {
  return(!is.na(data$y))
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
  limits_table[display_rows, "period_type"] <- "display"

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
    dplyr::summarise(dplyr::across(
      dplyr::everything(),
      ~ mean(.x,
        na.rm = TRUE
      )
    )) %>%
    as.list()

  return(limits)
}


# Presentation methods


#' The centre line label, formatted
#'
#' A thousands separator and no suffix. The proportion charts override this.
#'
#' @return character
#' @noRd
centre_line_label.autospc_chart <- function(chart,
                                            cl,
                                            ylimhigh) {
  return(scales::number(cl,
    big.mark = ",",
    accuracy = label_accuracy(
      chart = chart,
      ylimhigh = ylimhigh
    )
  ))
}


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


#' Do this chart's labels always sit above the centre line?
#'
#' No by default - `flip_labels` decides. The moving range chart overrides this.
#'
#' @return TRUE or FALSE
#' @noRd
labels_stay_above.autospc_chart <- function(chart) {
  return(FALSE)
}


#' Default vertical position of centre line labels
#'
#' Labels sit a tenth above the upper control limit. The proportion charts
#' override this: their limits are percentages, so a tenth of one is a wide gap
#' in the units of the axis.
#'
#' @return number, the scale factor applied to the upper control limit
#' @noRd
upper_annotation_sf_default.autospc_chart <- function(chart) {
  return(1.1)
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
  return(list(
    low = 0,
    high = 110
  ))
}


#' Print a summary of the analysis
#'
#' An `autospc_chart` is the analysis, not the drawing, so printing one
#' summarises what the algorithm did: the calculation periods it formed, where
#' it re-established limits, which points it excluded, and why it stopped.
#' `autospc()` returns an `autospc_plot`, which draws.
#'
#' @param x An `autospc_chart`.
#' @param ... Ignored, for consistency with the generic.
#'
#' @return `x`, invisibly.
#' @export
print.autospc_chart <- function(x, ...) {
  cat(sprintf(
    "<%s> %s chart, %d points, period_min = %d\n",
    class(x)[1],
    chart_type_label(x),
    nrow(x$data),
    x$period_min
  ))

  if (length(x$result) == 0L) {
    cat("\nNot analysed.\n")
    return(invisible(x))
  }

  if (!enough_data_for_limits(x)) {
    cat("\nNo limits: too few points to form a calculation period.\n")
    return(invisible(x))
  }

  cat("\nCalculation periods\n")
  cat(format_calculation_periods(x$result$table), sep = "\n")

  if (length(x$result$re_establish_rows) > 0L) {
    cat(sprintf(
      "\nLimits re-established at %s\n",
      paste(x$result$re_establish_rows, collapse = ", ")
    ))
  }

  if (length(x$result$exclusions) > 0L) {
    cat(sprintf(
      "%d point%s excluded from the limit calculations\n",
      length(x$result$exclusions),
      if (length(x$result$exclusions) == 1L) "" else "s"
    ))
  }

  candidates <- x$history$candidates
  if (length(candidates) > 0L) {
    accepted <- sum(vapply(
      candidates,
      function(candidate) isTRUE(candidate$accepted),
      logical(1)
    ))
    cat(sprintf(
      "%d candidate period%s considered, %d accepted\n",
      length(candidates),
      if (length(candidates) == 1L) "" else "s",
      accepted
    ))
  }

  if (!is.null(x$history$stopped)) {
    cat(sprintf(
      "Stopped at row %d: %s\n",
      x$history$stopped$counter,
      x$history$stopped$reason
    ))
  }

  return(invisible(x))
}


#' One line per calculation period, for `print.autospc_chart()`
#'
#' Long analyses are truncated, because a chart with many periods would
#' otherwise fill the console.
#'
#' @return A character vector of lines.
#' @noRd
format_calculation_periods <- function(table,
                                       max_shown = 10L) {
  periods <- unique(table$plot_period[table$period_type == "calculation"])

  lines <- vapply(periods, function(period) {
    rows <- which(table$plot_period == period)

    sprintf(
      "  rows %4d-%4d   cl = %s",
      min(rows),
      max(rows),
      format(signif(table$cl[rows[1]], 6))
    )
  }, character(1))

  if (length(lines) > max_shown) {
    lines <- c(
      lines[seq_len(max_shown)],
      sprintf("  ... and %d more", length(lines) - max_shown)
    )
  }

  return(unname(lines))
}
