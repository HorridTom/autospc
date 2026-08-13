test_chart_x <- function(...) {
  autospc_chart_x(data = test_data, x = "x", y = "y", ...)
}

dup_chart_x <- function(...) {
  autospc_chart_x(data = dup_data, x = "x", y = "y", ...)
}


test_that("autospc_chart_x returns an object of the expected class", {

  expect_identical(class(test_chart_x()),
                   c("autospc_chart_x", "autospc_chart"))

})


test_that("autospc_chart_x returns the common elements and no others", {

  chart <- test_chart_x()

  expect_true(all(autospc_chart_elements() %in% names(chart)))
  expect_length(chart, length(autospc_chart_elements()))

})


test_that("data_original is populated correctly", {

  expect_identical(test_chart_x()$data_original, test_data)

})


test_that("an unrecognised argument name is rejected", {

  expect_error(test_chart_x(period_mn = 30L), "unused argument")

})


test_that("a missing element is caught on the constructor path", {

  expect_error(
    validate_autospc_chart_x(new_autospc_chart_x(list(data = test_data))),
    "element\\(s\\) not present"
  )

})


test_that("validate_autospc_chart_x rejects a bare autospc_chart object", {

  expect_error(
    validate_autospc_chart_x(autospc_chart(data = test_data, x = "x", y = "y")),
    "Not an autospc_chart_x object"
  )

})


test_that("validate_autospc_chart_x rejects an autospc_chart_c object", {

  expect_error(
    validate_autospc_chart_x(autospc_chart_c(data = test_data,
                                             x = "x",
                                             y = "y")),
    "Not an autospc_chart_x object"
  )

})


test_that("validate_autospc_chart_x returns a valid object unchanged", {

  chart <- test_chart_x()

  expect_identical(validate_autospc_chart_x(chart), chart)

})


test_that("y_axis_title returns the X chart axis title", {

  expect_identical(y_axis_title(test_chart_x()), "X")

})


test_that("chart_type_label returns the X chart label", {

  expect_identical(chart_type_label(test_chart_x()), "X")

})


test_that("X charts have no aggregate_data method of their own", {

  # X plots the observations as supplied, so the superclass default is correct.
  expect_null(getS3method("aggregate_data",
                          "autospc_chart_x",
                          optional = TRUE))

})


test_that("aggregate_data leaves an X chart untouched", {

  # duplicated x values, which every other class would collapse
  chart <- dup_chart_x()

  expect_identical(aggregate_data(chart), chart)
  expect_identical(aggregate_data(chart)$data, dup_data)

})


test_that("calculate_limits matches get_i_limits", {

  expect_identical(
    calculate_limits(test_chart_x(), count_period_data,
                     exclusion_points = NULL),
    get_i_limits(y = count_period_data$y,
                 mr_screen_max_loops = 1L,
                 exclusion_points = NULL)
  )

})


test_that("calculate_limits passes exclusion_points through", {

  expect_identical(
    calculate_limits(test_chart_x(), count_period_data,
                     exclusion_points = 6L),
    get_i_limits(y = count_period_data$y,
                 mr_screen_max_loops = 1L,
                 exclusion_points = 6L)
  )

  # excluding the high point must actually lower the centre line, otherwise the
  # comparison above would pass even if the argument were ignored
  expect_lt(calculate_limits(test_chart_x(), count_period_data, 6L)$cl[1],
            calculate_limits(test_chart_x(), count_period_data, NULL)$cl[1])

})


test_that("calculate_limits takes mr_screen_max_loops from the chart", {

  chart <- test_chart_x(mr_screen_max_loops = 0L)

  expect_identical(
    calculate_limits(chart, screening_data, exclusion_points = NULL),
    get_i_limits(y = screening_data$y,
                 mr_screen_max_loops = 0L,
                 exclusion_points = NULL)
  )

  # screening must actually change the limits on this data, otherwise the
  # comparison above would pass even if the field were ignored
  expect_false(
    identical(calculate_limits(test_chart_x(mr_screen_max_loops = 0L),
                               screening_data, NULL),
              calculate_limits(test_chart_x(mr_screen_max_loops = 1L),
                               screening_data, NULL))
  )

})
