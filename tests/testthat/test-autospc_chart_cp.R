test_data <- data.frame(x = 1:3, y = 1:3)

# two rows per subgroup, plus a column the aggregation is expected to drop
dup_data <- data.frame(x = rep(1:3, each = 2),
                       y = c(1, 2, 10, 20, 100, 200),
                       site = "a")

# a calculation period with one obvious high point, so that excluding it
# demonstrably moves the limits
period_data <- data.frame(x = 1:10,
                          y = c(12, 15, 11, 14, 13, 30, 12, 14, 13, 11))

# a more extreme point, needed for the moving-range screening test: with a peak
# of 30 the two large moving ranges (17, 18) fall just under the MR upper limit,
# so screening changes nothing and the test could not tell whether
# mr_screen_max_loops was read at all
screening_data <- data.frame(x = 1:10,
                             y = c(12, 15, 11, 14, 13, 60, 12, 14, 13, 11))

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
    validate_autospc_chart_cp(autospc_chart(data = test_data, x = "x", y = "y")),
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
    calculate_limits(test_chart_cp(), period_data, exclusion_points = NULL),
    get_cp_limits(y = period_data$y,
                  exclusion_points = NULL,
                  mr_screen_max_loops = 1L)
  )

})


test_that("calculate_limits passes exclusion_points through", {

  expect_identical(
    calculate_limits(test_chart_cp(), period_data, exclusion_points = 6L),
    get_cp_limits(y = period_data$y,
                  exclusion_points = 6L,
                  mr_screen_max_loops = 1L)
  )

  # excluding the high point must actually lower the centre line, otherwise the
  # comparison above would pass even if the argument were ignored
  expect_lt(calculate_limits(test_chart_cp(), period_data, 6L)$cl[1],
            calculate_limits(test_chart_cp(), period_data, NULL)$cl[1])

})


test_that("calculate_limits takes mr_screen_max_loops from the chart", {

  # the C' method is the first to read a field off the chart, so this pins that
  # it is read rather than defaulted
  chart <- test_chart_cp(mr_screen_max_loops = 0L)

  expect_identical(
    calculate_limits(chart, screening_data, exclusion_points = NULL),
    get_cp_limits(y = screening_data$y,
                  exclusion_points = NULL,
                  mr_screen_max_loops = 0L)
  )

  # and screening must actually change the limits on this data, otherwise the
  # comparison above would pass even if the field were ignored
  expect_false(
    identical(calculate_limits(test_chart_cp(mr_screen_max_loops = 0L),
                               screening_data, NULL),
              calculate_limits(test_chart_cp(mr_screen_max_loops = 1L),
                               screening_data, NULL))
  )

})
