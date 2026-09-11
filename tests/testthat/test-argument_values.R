# The checks on arguments that accept a fixed set of values: floating_median,
# the Boolean arguments, and verbosity. Both entry points run them, and the
# chart object holds the matched value whatever built it.

values_data <- data.frame(
  x = 1:30,
  y = rep(c(10L, 12L, 11L, 13L, 9L, 14L), 5L)
)

analyse <- function(...) {
  autospc(values_data,
    chart_type = "C\'",
    x = "x",
    y = "y",
    plot_chart = FALSE,
    ...
  )
}

# the same call, leaving plot_chart to the caller
analyse_plotting <- function(...) {
  autospc(values_data,
    chart_type = "C\'",
    x = "x",
    y = "y",
    ...
  )
}


# floating_median


test_that("floating_median takes each of the values it accepts", {
  expect_false("median" %in% names(analyse(floating_median = "no")))
  expect_true("median" %in% names(analyse(floating_median = "yes")))
  expect_false("median" %in% names(analyse(floating_median = "auto")))
})


test_that("floating_median defaults to no when it is not given", {
  expect_false("median" %in% names(analyse()))
})


test_that("a floating_median value in the wrong case is an error", {
  expect_error(
    analyse(floating_median = "Yes"),
    "must be one of"
  )
})


test_that("the floating_median error names the argument and the value", {
  expect_error(
    analyse(floating_median = "banana"),
    "`floating_median`.*\"banana\""
  )
})


test_that("floating_median does not accept an abbreviation", {
  expect_error(
    analyse(floating_median = "a"),
    "must be one of"
  )
})


test_that("floating_median does not accept a value that is not a string", {
  expect_error(analyse(floating_median = TRUE), "character")
  expect_error(analyse(floating_median = NULL), "character")
})


test_that("the chart holds one floating_median value however it was built", {
  chart <- autospc_chart(
    chart_type = "C\'",
    data = values_data,
    x = "x",
    y = "y"
  )

  expect_identical(chart$floating_median, "no")
})


# the Boolean arguments


test_that("the Boolean arguments are the ones autospc defaults to TRUE or FALSE", {
  expect_setequal(
    autospc_flag_arguments(),
    c(
      "aggregation_na_rm",
      "baseline_only",
      "establish_every_shift",
      "no_regrets",
      "overhanging_reversions",
      "na_ends_run",
      "highlight_exclusions",
      "plot_chart",
      "show_limits",
      "keep_candidate_tables",
      "use_caption",
      "include_annotations",
      "basic_annotations",
      "align_labels",
      "flip_labels",
      "annotation_arrows"
    )
  )
})


# plot_chart is one of the arguments under test, so the call is built here
# rather than through analyse(), which sets plot_chart itself.
call_with <- function(name,
                      value) {
  given <- list(values_data, chart_type = "C\'", x = "x", y = "y")
  given[[name]] <- value

  return(given)
}


test_that("every Boolean argument rejects a string", {
  for (name in autospc_flag_arguments()) {
    expect_error(
      do.call(autospc, call_with(name, "banana")),
      paste0("`", name, "` must be TRUE or FALSE"),
      info = name
    )
  }
})


test_that("every Boolean argument rejects NA", {
  for (name in autospc_flag_arguments()) {
    expect_error(
      do.call(autospc, call_with(name, NA)),
      paste0("`", name, "` must be TRUE or FALSE"),
      info = name
    )
  }
})


test_that("a Boolean argument accepts 1 and 0", {
  expect_identical(
    match_flag(1, "use_caption"),
    TRUE
  )
  expect_identical(
    match_flag(0L, "use_caption"),
    FALSE
  )
})


test_that("1 and 0 reach the argument they were given for", {
  # plot_chart decides what autospc() returns, so the coerced value shows in
  # the result rather than only in the object
  expect_s3_class(
    suppressWarnings(analyse_plotting(plot_chart = 1)),
    "autospc_plot"
  )
  expect_s3_class(analyse_plotting(plot_chart = 0), "data.frame")
})


test_that("a Boolean argument rejects a number that is not 1 or 0", {
  expect_error(
    match_flag(2, "align_labels"),
    "`align_labels` must be TRUE or FALSE, not 2"
  )
})


# verbosity


test_that("verbosity above the range is taken as the top of it", {
  expect_identical(match_range(5, "verbosity", range = c(0L, 2L)), 2L)
})


test_that("verbosity below the range is taken as the bottom of it", {
  expect_identical(match_range(-1, "verbosity", range = c(0L, 2L)), 0L)
})


test_that("verbosity between two whole numbers is the next one up", {
  expect_identical(match_range(0.4, "verbosity", range = c(0L, 2L)), 1L)
  expect_identical(match_range(1.5, "verbosity", range = c(0L, 2L)), 2L)
})


# capture.output() prints the value of the expression as well, so the result is
# assigned rather than returned
log_of <- function(...) {
  return(capture.output({
    result <- analyse(...)
  }))
}


test_that("a verbosity above the range logs as much as the top of it", {
  top <- log_of(verbosity = 2)

  expect_gt(length(top), 0L)
  expect_identical(log_of(verbosity = 5), top)
})


test_that("a verbosity below the range logs nothing", {
  expect_length(log_of(verbosity = -1), 0L)
})


test_that("a fractional verbosity logs as much as the next whole number up", {
  expect_identical(log_of(verbosity = 0.4), log_of(verbosity = 1))
  expect_identical(log_of(verbosity = 1.6), log_of(verbosity = 2))
})


test_that("verbosity rejects a value that is not a number", {
  expect_error(
    analyse(verbosity = "two"),
    "`verbosity` must be a number from 0 to 2"
  )
})


# facet_stages runs the same checks


test_that("facet_stages checks the values it is given", {
  expect_error(
    facet_stages(values_data,
      split_at = 20,
      chart_type = "C\'",
      x = "x",
      y = "y",
      floating_median = "Yes"
    ),
    "must be one of"
  )

  expect_error(
    facet_stages(values_data,
      split_at = 20,
      chart_type = "C\'",
      x = "x",
      y = "y",
      use_caption = "banana"
    ),
    "`use_caption` must be TRUE or FALSE"
  )
})


test_that("facet_stages checks its own plot_chart argument", {
  expect_error(
    facet_stages(values_data,
      split_at = 20,
      chart_type = "C\'",
      x = "x",
      y = "y",
      plot_chart = "banana"
    ),
    "`plot_chart` must be TRUE or FALSE"
  )
})


# the numeric arguments


test_that("every numeric argument is checked", {
  # the kinds cannot be read off the signature, so this is the list that is
  # expected
  expect_setequal(
    names(autospc_numeric_arguments()),
    c(
      "period_min",
      "baseline_length",
      "shift_rule_threshold",
      "floating_median_n",
      "max_exclusions",
      "mr_screen_max_loops",
      "centre_line_tolerance",
      "point_size",
      "line_width_sf",
      "annotation_size",
      "annotation_arrow_curve",
      "upper_annotation_sf",
      "lower_annotation_sf",
      "override_y_lim",
      "x_break",
      "x_pad_end",
      "extend_limits_to"
    )
  )
})


test_that("every numeric argument rejects a string", {
  for (name in names(autospc_numeric_arguments())) {
    expect_error(
      do.call(autospc, call_with(name, "banana")),
      paste0("`", name, "` must be"),
      info = name
    )
  }
})


test_that("a count argument rejects zero, a negative and a fraction", {
  expect_error(match_number(0, "period_min", "count"), "one or more")
  expect_error(match_number(-5, "period_min", "count"), "one or more")
  expect_error(match_number(21.5, "period_min", "count"), "one or more")

  expect_identical(match_number(21L, "period_min", "count"), 21L)
})


test_that("max_exclusions accepts zero, which excludes nothing", {
  expect_identical(match_number(0, "max_exclusions", "count_from_zero"), 0)

  result <- analyse(max_exclusions = 0)

  expect_false(any(result$excluded, na.rm = TRUE))
})


test_that("max_exclusions rejects Inf, which would not terminate", {
  # find_extremes() loops max_exclusions times with no other way out
  expect_error(
    match_number(Inf, "max_exclusions", "count_from_zero"),
    "whole number of zero or more"
  )
})


test_that("mr_screen_max_loops accepts zero and Inf", {
  expect_identical(match_number(0, "mr_screen_max_loops", "loops"), 0)
  expect_identical(match_number(Inf, "mr_screen_max_loops", "loops"), Inf)
})


test_that("centre_line_tolerance takes a fraction but not a negative", {
  expect_identical(
    match_number(0.5, "centre_line_tolerance", "non_negative"),
    0.5
  )
  expect_error(
    match_number(-1, "centre_line_tolerance", "non_negative"),
    "zero or more"
  )
})


test_that("a size argument rejects zero and a negative", {
  expect_error(match_number(0, "point_size", "positive"), "above zero")
  expect_error(match_number(-1, "point_size", "positive"), "above zero")

  expect_identical(match_number(1.5, "point_size", "positive"), 1.5)
})


test_that("an axis value is not restricted to a number", {
  # x_break is a difftime where the horizontal axis holds dates, and the axis
  # is not meant to be limited to the types it holds today
  expect_identical(
    match_axis_value(as.difftime(2, units = "days"), "x_break"),
    as.difftime(2, units = "days")
  )
  expect_identical(
    match_axis_value(as.Date("2020-01-01"), "extend_limits_to"),
    as.Date("2020-01-01")
  )
  expect_identical(match_axis_value(40, "extend_limits_to"), 40)
})


test_that("an axis value rejects more than one value and NA", {
  expect_error(match_axis_value(c(1, 2), "x_break"), "a single value")
  expect_error(match_axis_value(NA, "x_break"), "a single value")
})


test_that("an argument autospc declares as NULL accepts NULL", {
  for (name in names(autospc_numeric_arguments())) {
    if (!is.null(autospc_default(name))) {
      next
    }

    # given[[name]] <- NULL would remove the element rather than set it
    given <- call_with(name, 1)
    given[name] <- list(NULL)
    given$plot_chart <- FALSE

    expect_s3_class(do.call(autospc, given), "data.frame")
  }
})


# extend_limits_to against the data, which is the one check that cannot be
# made from an argument's value alone


test_that("limits are only extended beyond the end of the data", {
  # values_data runs x = 1 to 30, so 30 is the end of it and not beyond it
  expect_error(analyse(extend_limits_to = 30), "beyond the end of the data")

  expect_error(analyse(extend_limits_to = 25), "beyond the end of the data")

  expect_s3_class(analyse(extend_limits_to = 40), "data.frame")
})


test_that("the error names the function the caller called", {
  # validate_arguments_against_data() is internal, so the error is reported
  # against autospc() rather than against itself
  caught <- tryCatch(analyse(extend_limits_to = 30), error = function(e) e)

  expect_identical(as.character(conditionCall(caught)[[1L]]), "autospc")
})


test_that("facet_stages checks against the whole series", {
  # every stage but the last is a prefix of the series, so a point inside the
  # series is beyond some stages and not others. The last stage is always the
  # whole series, so the whole series decides
  expect_error(
    facet_stages(values_data,
      split_at = c(10L, 20L, 30L),
      chart_type = "C", period_min = 5L,
      extend_limits_to = 25, plot_chart = FALSE
    ),
    "beyond the end of the data"
  )

  expect_s3_class(
    facet_stages(values_data,
      split_at = c(10L, 20L, 30L),
      chart_type = "C", period_min = 5L,
      extend_limits_to = 40, plot_chart = FALSE
    ),
    "data.frame"
  )
})


test_that("the check steps aside where there is no x to compare against", {
  # a column that is not in the data, so there is nothing to take a maximum
  # of. The column itself is reported by the code that looks for it
  caught <- character()

  withCallingHandlers(
    try(
      autospc(values_data,
        chart_type = "C", x = nosuch, y = y,
        period_min = 5L, extend_limits_to = 40, plot_chart = FALSE
      ),
      silent = TRUE
    ),
    warning = function(w) {
      caught <<- c(caught, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_length(caught, 0L)

  # every x missing, which warns about the rows it drops but not about taking
  # a maximum of nothing
  no_x <- data.frame(x = rep(NA_integer_, 30L), y = values_data$y)

  caught <- character()

  withCallingHandlers(
    autospc(no_x,
      chart_type = "C", period_min = 5L,
      extend_limits_to = 40, plot_chart = FALSE
    ),
    warning = function(w) {
      caught <<- c(caught, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_false(any(grepl("maximum", caught, fixed = TRUE)))
})


# the arguments that take a single string


test_that("a string argument takes a single string, and NULL where it may", {
  # the value each one is given where it is accepted: log_file_path is written
  # to, so it is given a path rather than a word
  accepted <- list(
    title = "Something",
    subtitle = "Something",
    override_x_title = "Something",
    override_y_title = "Something",
    log_file_path = file.path(tempdir(), "argument_values_log.csv")
  )

  expect_setequal(names(accepted), autospc_string_arguments())

  for (name in autospc_string_arguments()) {
    expect_s3_class(
      do.call(analyse, stats::setNames(accepted[name], name)),
      "data.frame"
    )

    expect_error(
      do.call(analyse, stats::setNames(list(42), name)),
      "must be a single string",
      info = name
    )

    expect_error(
      do.call(analyse, stats::setNames(list(c("a", "b")), name)),
      "must be a single string",
      info = name
    )

    # every one of them defaults to NULL, so NULL is left alone
    expect_s3_class(
      do.call(analyse, stats::setNames(list(NULL), name)),
      "data.frame"
    )
  }

  unlink(accepted$log_file_path)
})


# the arguments that take a colour


test_that("a colour argument takes what R accepts as a colour", {
  expect_setequal(autospc_colour_arguments(), c("r1_col", "r2_col"))

  for (name in autospc_colour_arguments()) {
    for (value in list("red", "#ff0000", 2L)) {
      expect_s3_class(
        do.call(analyse, stats::setNames(list(value), name)),
        "data.frame"
      )
    }

    expect_error(
      do.call(analyse, stats::setNames(list("notacolour"), name)),
      "must be a colour",
      info = name
    )

    expect_error(
      do.call(analyse, stats::setNames(list(c("red", "blue")), name)),
      "must be a colour",
      info = name
    )
  }
})


test_that("a bad colour is refused whether or not the chart would use it", {
  # values_data breaks no rule, so neither highlight colour is drawn. The
  # colour is still checked, because a later series would use it
  expect_error(analyse(r2_col = "notacolour"), "must be a colour")
})


# x_date_format


test_that("x_date_format holds at least one % code", {
  expect_s3_class(analyse(x_date_format = "%b %Y"), "data.frame")

  expect_error(analyse(x_date_format = 42), "must be a single string")

  # a format with no % code formats every date as itself, so every label on
  # the axis would read the same
  expect_error(analyse(x_date_format = "nonsense"), "at least one % code")
})
