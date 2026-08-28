test_chart_c <- function(...) {
  autospc_chart_c(data = test_data, x = "x", y = "y", ...)
}

dup_chart_c <- function(...) {
  autospc_chart_c(data = dup_data, x = "x", y = "y", ...)
}


test_that("autospc_chart_c returns an object of the expected class", {
  expect_identical(
    class(test_chart_c()),
    c("autospc_chart_c", "autospc_chart")
  )
})


test_that("autospc_chart_c returns the common elements and no others", {
  chart <- test_chart_c()

  expect_true(all(autospc_chart_elements() %in% names(chart)))
  expect_length(chart, length(autospc_chart_elements()))
})


test_that("baseline_length is present and NULL by default", {
  chart <- test_chart_c()

  expect_true("baseline_length" %in% names(chart))
  expect_null(chart$baseline_length)
})


test_that("defaults populated as expected", {
  chart <- test_chart_c()

  expect_identical(chart$period_min, 21L)
  expect_identical(chart$shift_rule_threshold, 8L)
  expect_identical(chart$baseline_only, FALSE)
  expect_identical(chart$establish_every_shift, FALSE)
  expect_identical(chart$no_regrets, TRUE)
  expect_identical(chart$overhanging_reversions, TRUE)
  expect_identical(chart$max_exclusions, 3L)
  expect_identical(chart$mr_screen_max_loops, 1L)
  expect_identical(chart$centre_line_tolerance, 0)
})


test_that("a chart built without chart parameters takes autospc()'s defaults", {
  chart <- test_chart_c()

  for (parameter in autospc_chart_parameters()) {
    expect_identical(chart[[parameter]],
      autospc_default(parameter),
      info = parameter
    )
  }
})


test_that("data_original is populated correctly", {
  chart <- test_chart_c()

  expect_identical(chart$data_original, test_data)
})


test_that("arguments passed through ... reach the object", {
  chart <- test_chart_c(
    period_min = 30L,
    no_regrets = FALSE,
    baseline_length = 10L
  )

  expect_identical(chart$period_min, 30L)
  expect_identical(chart$no_regrets, FALSE)
  expect_identical(chart$baseline_length, 10L)
})


test_that("an unrecognised argument name is rejected", {
  expect_error(test_chart_c(period_mn = 30L), "unused argument")
})


test_that("data_original cannot be set by the caller", {
  expect_error(test_chart_c(data_original = test_data), "unused argument")
})


test_that("a missing element is caught on the constructor path", {
  expect_error(
    validate_autospc_chart_c(new_autospc_chart_c(list(data = test_data))),
    "element\\(s\\) not present"
  )
})


test_that("validate_autospc_chart_c rejects a bare autospc_chart object", {
  expect_error(
    validate_autospc_chart_c(new_autospc_chart(autospc_chart_list(data = test_data, x = "x", y = "y"))),
    "Not an autospc_chart_c object"
  )
})


test_that("validate_autospc_chart_c returns a valid object unchanged", {
  chart <- test_chart_c()

  expect_identical(validate_autospc_chart_c(chart), chart)
})


test_that("y_axis_title returns the C chart axis title", {
  expect_identical(y_axis_title(test_chart_c()), "Number")
})


test_that("aggregate_data sums y over x", {
  chart <- aggregate_data(dup_chart_c())

  expect_identical(chart$data$x, 1:3)
  expect_identical(chart$data$y, c(3, 30, 300))
})


test_that("aggregate_data returns a chart, not a data frame", {
  chart <- aggregate_data(dup_chart_c())

  expect_identical(class(chart), c("autospc_chart_c", "autospc_chart"))
  expect_true(all(autospc_chart_elements() %in% names(chart)))
})


test_that("aggregate_data leaves data_original untouched", {
  chart <- aggregate_data(dup_chart_c())

  expect_identical(chart$data_original, dup_data)
})


test_that("aggregate_data preserves x and y when every x is unique", {
  chart <- aggregate_data(test_chart_c())

  expect_identical(chart$data$x, test_data$x)
  expect_identical(chart$data$y, test_data$y)
})


test_that("chart_type_label returns the C chart label", {
  expect_identical(chart_type_label(test_chart_c()), "C")
})


test_that("calculate_limits matches get_c_limits", {
  expect_identical(
    calculate_limits(test_chart_c(),
      period = count_period_data,
      exclusion_points = NULL
    ),
    get_c_limits(y = count_period_data$y, exclusion_points = NULL)
  )
})


test_that("calculate_limits passes exclusion_points through", {
  expect_identical(
    calculate_limits(test_chart_c(),
      period = count_period_data,
      exclusion_points = 6L
    ),
    get_c_limits(y = count_period_data$y, exclusion_points = 6L)
  )

  # excluding the high point must actually lower the centre line, otherwise the
  # comparison above would pass even if the argument were ignored
  expect_lt(
    calculate_limits(test_chart_c(),
      period = count_period_data,
      exclusion_points = 6L
    )$cl[1],
    calculate_limits(test_chart_c(),
      period = count_period_data,
      exclusion_points = NULL
    )$cl[1]
  )
})


test_that("the C chart y axis leaves headroom above the upper limit", {
  expect_identical(
    y_axis_range(test_chart_c(), data = limits_data),
    list(low = 0, high = 18 + 18 / 10 + 10)
  )
})
