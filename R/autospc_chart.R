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
#' among them are whole numbers. `prepare_data()` then adds `series`, *the
#' values under analysis*, which are not always the values the user passed: for
#' MR they are the moving ranges, and for P and P' the percentages. `y` keeps
#' what the user supplied, aggregated where the class aggregates.
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

  require_column(
    data = x$data,
    column = "x",
    message = paste(
      "x not specified. Every chart type needs x: name the column with",
      "the x argument, or call it x in the data."
    )
  )

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
    "extend_limits_to",
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
  chart_types <- c(names(autospc_pair_types()), "X", "MR", "C", "C'", "P", "P'")

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
#' A pair's chart type, such as `"XMR"`, asks for two charts, named `location`
#' and `dispersion` as `autospc_pair_types()` gives them. Every other chart type
#' asks for one, unnamed. This is the only place a chart type is read as a
#' string rather than dispatched on: everything after it holds chart objects.
#'
#' Both halves of a pair are built from the same data, and each class prepares
#' its own series from it: `prepare_data.autospc_chart_mr()` derives the moving
#' ranges from `y`.
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
  chart_types <- autospc_pair_types()[[chart_type]]

  if (is.null(chart_types)) {
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


#' The chart types that are pairs, and the two charts each asks for
#'
#' A pair is one analysis shown as two charts: a chart of location and a chart
#' of dispersion. Each element is named by the pair's chart type and holds the
#' chart types of its two halves, named `location` and `dispersion`. A pair is
#' registered here and nowhere else.
#'
#' @return A named list of named character vectors.
#' @noRd
autospc_pair_types <- function() {
  return(list(
    XMR = c(location = "X", dispersion = "MR")
  ))
}


#' The chart type of the pair these charts make
#'
#' The charts make a pair where they are named `location` and `dispersion`, in
#' that order, and their chart types are those of a pair in
#' `autospc_pair_types()`.
#'
#' @param charts A list of `autospc_chart` objects.
#'
#' @return The pair's chart type, or NULL where the charts are not a pair.
#' @noRd
pair_type <- function(charts) {
  if (!identical(names(charts), c("location", "dispersion"))) {
    return(NULL)
  }

  # an anonymous function, so that the unregistered methods are found from the
  # package namespace rather than from inside vapply()
  halves <- vapply(
    charts,
    function(chart) chart_type_label(chart),
    character(1L)
  )

  for (type in names(autospc_pair_types())) {
    if (identical(autospc_pair_types()[[type]], halves)) {
      return(type)
    }
  }

  return(NULL)
}


#' Are these charts a pair?
#'
#' @param charts A list of `autospc_chart` objects.
#'
#' @return TRUE or FALSE
#' @noRd
is_chart_pair <- function(charts) {
  return(!is.null(pair_type(charts)))
}


#' The chart type of a pair's location chart
#'
#' A pair's chart type gives the chart type of its location half. Any other
#' chart type, including NULL, is returned unchanged.
#'
#' @param chart_type A chart type, as the caller gave it.
#'
#' @return A chart type.
#' @noRd
location_chart_type <- function(chart_type) {
  if (is.character(chart_type) && length(chart_type) == 1L &&
    chart_type %in% names(autospc_pair_types())) {
    return(autospc_pair_types()[[chart_type]][["location"]])
  }

  return(chart_type)
}


#' The location half of a pair, or the one element of a list that holds one
#'
#' Used for a list of charts or of their plot data, which holds the two halves
#' of a pair by name, or a single chart.
#'
#' @param items A list holding a `location` element, or one element.
#'
#' @return The `location` element, or the one element.
#' @noRd
location_component <- function(items) {
  if ("location" %in% names(items)) {
    return(items$location)
  }

  if (length(items) != 1L) {
    stop(
      "Expected a pair named location and dispersion, or a single chart.",
      call. = FALSE
    )
  }

  return(items[[1L]])
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
  extend_limits_to = autospc_default("extend_limits_to"),
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
    extend_limits_to = extend_limits_to,
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
#' The series is the column the user supplied. Overridden by the classes that
#' analyse something derived from it.
#'
#' @return autospc_chart object of the same class as chart
#' @noRd
prepare_data.autospc_chart <- function(chart) {
  chart$data$series <- chart$data$y

  return(chart)
}


#' Number of points available for analysis
#'
#' The non-missing values of `series`.
#'
#' @return integer
#' @noRd
n_effective_points.autospc_chart <- function(chart,
                                             data) {
  points <- data %>%
    dplyr::filter(!is.na(series)) %>%
    nrow()

  return(points)
}


#' @noRd
observed_rows.autospc_chart <- function(chart,
                                        data) {
  return(!is.na(data$series))
}


#' Columns the limits table carries beside the series under analysis
#'
#' `y`, what the caller supplied, by default. The proportion classes add the
#' denominator, which their limits are calculated from as well.
#'
#' @return character vector
#' @noRd
limits_table_columns.autospc_chart <- function(chart) {
  return("y")
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


#' The range the plotted statistic can take
#'
#' No bound, which is right for an individuals value and is the safe answer for
#' a class that has not said otherwise. Overridden by the classes whose
#' statistic is a count, a moving range or a percentage.
#'
#' @return list of two numbers, low and high
#' @noRd
limit_bounds.autospc_chart <- function(chart) {
  return(list(
    low = -Inf,
    high = Inf
  ))
}


#' A period's standard deviation estimate at each of a set of rows
#'
#' The period's one estimate, at every row.
#'
#' @return numeric, one value per row of `rows`
#' @noRd
sd_estimate_at.autospc_chart <- function(chart, statistics, rows) {
  return(rep_len(statistics$sd_estimate, nrow(rows)))
}


#' The standard error at each of a set of rows
#'
#' The estimate itself, at every row. Overridden by the classes whose limits
#' vary with the denominator.
#'
#' @return numeric, one value per row of `rows`
#' @noRd
standard_error_at.autospc_chart <- function(chart, sd_estimate, rows) {
  return(rep_len(sd_estimate, nrow(rows)))
}


#' Control limits from a period's statistics
#'
#' Three standard errors either side of the centre line. Overridden by the
#' moving range chart, whose lower limit is defined differently.
#'
#' @return list of two numeric vectors named ucl and lcl
#' @noRd
limits_from_statistics.autospc_chart <- function(chart, statistics, rows) {
  half_width <- 3 * standard_error_at(
    chart = chart,
    sd_estimate = statistics$sd_estimate,
    rows = rows
  )

  return(list(
    ucl = statistics$cl + half_width,
    lcl = statistics$cl - half_width
  ))
}


#' Lower and upper ends of the y axis
#'
#' Zero to 110 wherever the limits and the points lie within 0 to 100, which is
#' where constraining the limits keeps them. Where they do not, the axis
#' follows them, so that nothing the chart draws falls outside it. Overridden
#' by the classes whose axis follows the data.
#'
#' @return list of two numbers, low and high
#' @noRd
y_axis_range.autospc_chart <- function(chart,
                                       data) {
  low <- min(0, data$lcl, data$series, na.rm = TRUE)
  high <- max(data$ucl, data$series, na.rm = TRUE)

  if (high <= 100) {
    return(list(
      low = low,
      high = 110
    ))
  }

  return(list(
    low = low,
    high = high * 1.1
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
