test_data <- data.frame(x = 1:3, y = 1:3)

test_chart_c <- function(...) {
  autospc_chart_c(data = test_data, x = "x", y = "y", ...)
}


test_that("autospc_chart_c returns an object of the expected class", {

  expect_identical(class(test_chart_c()),
                   c("autospc_chart_c", "autospc_chart"))

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


test_that("arguments passed through ... reach the object", {

  chart <- test_chart_c(period_min = 30L,
                        no_regrets = FALSE,
                        baseline_length = 10L)

  expect_identical(chart$period_min, 30L)
  expect_identical(chart$no_regrets, FALSE)
  expect_identical(chart$baseline_length, 10L)

})


test_that("an unrecognised argument name is rejected", {

  expect_error(test_chart_c(period_mn = 30L), "unused argument")

})


test_that("a missing element is caught on the constructor path", {

  expect_error(
    validate_autospc_chart_c(new_autospc_chart_c(list(data = test_data))),
    "element\\(s\\) not present"
  )

})


test_that("validate_autospc_chart_c rejects a bare autospc_chart object", {

  expect_error(
    validate_autospc_chart_c(autospc_chart(data = test_data, x = "x", y = "y")),
    "Not an autospc_chart_c object"
  )

})


test_that("validate_autospc_chart_c returns a valid object unchanged", {

  chart <- test_chart_c()

  expect_identical(validate_autospc_chart_c(chart), chart)

})


test_that("internal defaults match autospc()", {

  skip("Pending worklist Deferred #4 - integer/double defaults")

  expect_identical(formals(autospc)$period_min,
                   formals(autospc_chart_list)$period_min)
  expect_identical(formals(autospc)$max_exclusions,
                   formals(autospc_chart_list)$max_exclusions)

})
