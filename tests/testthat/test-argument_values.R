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
      "baseline_only",
      "establish_every_shift",
      "no_regrets",
      "overhanging_reversions",
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
      split_rows = 20,
      chart_type = "C\'",
      x = "x",
      y = "y",
      floating_median = "Yes"
    ),
    "must be one of"
  )

  expect_error(
    facet_stages(values_data,
      split_rows = 20,
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
      split_rows = 20,
      chart_type = "C\'",
      x = "x",
      y = "y",
      plot_chart = "banana"
    ),
    "`plot_chart` must be TRUE or FALSE"
  )
})
