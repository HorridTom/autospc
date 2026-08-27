test_chart_cp <- function(...) {
  autospc_chart_cp(data = test_data, x = "x", y = "y", ...)
}

dup_chart_cp <- function(...) {
  autospc_chart_cp(data = dup_data, x = "x", y = "y", ...)
}


test_that("autospc_chart_cp returns an object of the expected class", {

  expect_identical(class(test_chart_cp()),
                   c("autospc_chart_cp", "autospc_chart"))

})


test_that("autospc_chart_cp returns the common elements and no others", {

  chart <- test_chart_cp()

  expect_true(all(autospc_chart_elements() %in% names(chart)))
  expect_length(chart, length(autospc_chart_elements()))

})


test_that("data_original is populated correctly", {

  expect_identical(test_chart_cp()$data_original, test_data)

})


test_that("an unrecognised argument name is rejected", {

  expect_error(test_chart_cp(period_mn = 30L), "unused argument")

})


test_that("a missing element is caught on the constructor path", {

  expect_error(
    validate_autospc_chart_cp(new_autospc_chart_cp(list(data = test_data))),
    "element\\(s\\) not present"
  )

})


test_that("validate_autospc_chart_cp rejects a bare autospc_chart object", {

  expect_error(
    validate_autospc_chart_cp(new_autospc_chart(autospc_chart_list(data = test_data, x = "x", y = "y"))),
    "Not an autospc_chart_cp object"
  )

})


test_that("validate_autospc_chart_cp rejects an autospc_chart_c object", {

  expect_error(
    validate_autospc_chart_cp(autospc_chart_c(data = test_data,
                                              x = "x",
                                              y = "y")),
    "Not an autospc_chart_cp object"
  )

})


test_that("validate_autospc_chart_cp returns a valid object unchanged", {

  chart <- test_chart_cp()

  expect_identical(validate_autospc_chart_cp(chart), chart)

})


test_that("y_axis_title returns the C' chart axis title", {

  expect_identical(y_axis_title(test_chart_cp()), "Number")

})


test_that("chart_type_label returns the C' chart label", {

  expect_identical(chart_type_label(test_chart_cp()), "C'")

})


test_that("aggregate_data sums y over x", {

  chart <- aggregate_data(dup_chart_cp())

  expect_identical(chart$data$x, 1:3)
  expect_identical(chart$data$y, c(3, 30, 300))

})


test_that("aggregate_data returns a chart, not a data frame", {

  chart <- aggregate_data(dup_chart_cp())

  expect_identical(class(chart), c("autospc_chart_cp", "autospc_chart"))
  expect_true(all(autospc_chart_elements() %in% names(chart)))

})


test_that("aggregate_data leaves data_original untouched", {

  expect_identical(aggregate_data(dup_chart_cp())$data_original, dup_data)

})


test_that("calculate_limits matches get_cp_limits", {

  expect_identical(
    calculate_limits(test_chart_cp(), period = count_period_data,
                     exclusion_points = NULL),
    get_cp_limits(y = count_period_data$y,
                  exclusion_points = NULL,
                  mr_screen_max_loops = 1L)
  )

})


test_that("calculate_limits passes exclusion_points through", {

  expect_identical(
    calculate_limits(test_chart_cp(), period = count_period_data,
                     exclusion_points = 6L),
    get_cp_limits(y = count_period_data$y,
                  exclusion_points = 6L,
                  mr_screen_max_loops = 1L)
  )

  # excluding the high point must actually lower the centre line, otherwise the
  # comparison above would pass even if the argument were ignored
  expect_lt(calculate_limits(test_chart_cp(),
                             period = count_period_data,
                             exclusion_points = 6L)$cl[1],
            calculate_limits(test_chart_cp(),
                             period = count_period_data,
                             exclusion_points = NULL)$cl[1])

})


test_that("calculate_limits takes mr_screen_max_loops from the chart", {

  # the C' method is the first to read a field off the chart, so this pins that
  # it is read rather than defaulted
  chart <- test_chart_cp(mr_screen_max_loops = 0L)

  expect_identical(
    calculate_limits(chart, period = screening_data, exclusion_points = NULL),
    get_cp_limits(y = screening_data$y,
                  exclusion_points = NULL,
                  mr_screen_max_loops = 0L)
  )

  # and screening must actually change the limits on this data, otherwise the
  # comparison above would pass even if the field were ignored
  expect_false(
    identical(calculate_limits(test_chart_cp(mr_screen_max_loops = 0L),
                               period = screening_data,
                               exclusion_points = NULL),
              calculate_limits(test_chart_cp(mr_screen_max_loops = 1L),
                               period = screening_data,
                               exclusion_points = NULL))
  )

})


test_that("the C' chart y axis leaves headroom above the upper limit", {

  expect_identical(y_axis_range(test_chart_cp(), data = limits_data),
                   list(low = 0, high = 18 + 18 / 10 + 10))

})
