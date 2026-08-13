# Tests for both functions in R/column_names.R, in the order they run:
# resolve_column_name() on the arguments, then normalise_columns() on the data.
#
# For normalise_columns(), every class test file builds charts whose columns
# are already named x, y and n, so no renaming happens there. What is tested
# here is data whose columns are named something else.

# resolve_column_name() takes a quosure, so it is called through a wrapper that
# applies rlang::enquo() to its argument, as in autospc().
resolve <- function(column, fallback = "x") {
  resolve_column_name(rlang::enquo(column), fallback)
}


test_that("a bare symbol resolves to its name", {

  expect_identical(resolve(month_start), "month_start")

})


test_that("a string resolves to itself", {

  expect_identical(resolve("month_start"), "month_start")

})


test_that("a missing argument resolves to the fallback", {

  expect_identical(resolve(), "x")
  expect_identical(resolve(fallback = "n"), "n")

})


user_data <- data.frame(month_start = 1:3,
                        att_all = c(10, 20, 30),
                        denom = c(100L, 100L, 100L),
                        site = "a")


test_that("data is normalised to x and y at construction", {

  chart <- autospc_chart_c(data = user_data, x = "month_start", y = "att_all")

  expect_true(all(c("x", "y") %in% names(chart$data)))
  expect_identical(chart$data$x, user_data$month_start)
  expect_identical(chart$data$y, user_data$att_all)

})


test_that("data_original keeps the user's column names exactly", {

  chart <- autospc_chart_c(data = user_data, x = "month_start", y = "att_all")

  expect_identical(chart$data_original, user_data)

})


test_that("the source column names are retained on the object", {

  chart <- autospc_chart_c(data = user_data, x = "month_start", y = "att_all")

  expect_identical(chart$x, "month_start")
  expect_identical(chart$y, "att_all")

})


test_that("columns other than the named ones are left alone", {

  chart <- autospc_chart_c(data = user_data, x = "month_start", y = "att_all")

  expect_true("site" %in% names(chart$data))

})


test_that("n is normalised for proportion charts", {

  chart <- autospc_chart_p(data = user_data,
                           x = "month_start",
                           y = "att_all",
                           n = "denom")

  expect_identical(chart$data$n, user_data$denom)
  expect_identical(chart$n, "denom")

})


test_that("no rename is attempted when source and target already match", {

  # individual binary observations: no denominator column is supplied, n falls
  # back to "n", and there is nothing to rename. A blind rename would error.
  binary <- data.frame(x = 1:4, y = c(TRUE, FALSE, TRUE, TRUE))

  expect_no_error(autospc_chart_p(data = binary, x = "x", y = "y", n = "n"))

  chart <- autospc_chart_p(data = binary, x = "x", y = "y", n = "n")

  expect_false("n" %in% names(chart$data))

})


test_that("naming a column that is not there errors", {

  expect_error(
    autospc_chart_c(data = user_data, x = "month_start", y = "not_a_column")
  )

})


test_that("normalisation is silent", {

  # rename_columns() emits a warning of its own when renaming; this must not,
  # or users would see it twice while autospc() still calls both
  expect_silent(autospc_chart_c(data = user_data,
                                x = "month_start",
                                y = "att_all"))

})
