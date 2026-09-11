# Tests for both functions in R/column_names.R, in the order they run:
# resolve_column_name() on the arguments, then normalise_columns() on the data.
#
# For normalise_columns(), every class test file builds charts whose columns
# are already named x, y and n, so no renaming happens there. What is tested
# here is data whose columns are named something else.

# resolve_column_name() takes a quosure, so it is called through a wrapper that
# applies rlang::enquo() to its argument, as in autospc().
resolve <- function(column, fallback = "x") {
  resolve_column_name(rlang::enquo(column), fallback = fallback)
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


user_data <- data.frame(
  month_start = 1:3,
  att_all = c(10, 20, 30),
  denom = c(100L, 100L, 100L),
  site = "a"
)


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


test_that("columns other than the named ones are not carried into the analysis", {
  chart <- autospc_chart_c(data = user_data, x = "month_start", y = "att_all")

  expect_identical(names(chart$data), c("x", "y"))
})


test_that("the data as passed is kept whole, under the names it was passed with", {
  chart <- autospc_chart_c(data = user_data, x = "month_start", y = "att_all")

  expect_identical(chart$data_original, user_data)
})


test_that("a column already named x does not stop another being named as x", {
  clashing <- user_data
  clashing$x <- seq_len(nrow(clashing))

  chart <- autospc_chart_c(data = clashing, x = "month_start", y = "att_all")

  expect_identical(chart$data$x, user_data$month_start)
})


test_that("a column named as x that is not there is still an error", {
  expect_error(
    autospc_chart_c(data = user_data, x = "not_a_column", y = "att_all")
  )
})


test_that("n is normalised for proportion charts", {
  chart <- autospc_chart_p(
    data = user_data,
    x = "month_start",
    y = "att_all",
    n = "denom"
  )

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
  expect_silent(autospc_chart_c(
    data = user_data,
    x = "month_start",
    y = "att_all"
  ))
})


# a column the caller named that is not in the data


test_that("a column that is not in the data is named in the error", {
  named_data <- data.frame(
    month = 1:30,
    att = as.integer(rep(c(50L, 48L, 52L, 47L, 51L, 49L), 5L)),
    denom = rep(100L, 30L)
  )

  expect_error(
    autospc(named_data,
      chart_type = "C", x = nosuch, y = att,
      period_min = 5L, plot_chart = FALSE
    ),
    'Columns not found in the data: "nosuch" \\(named by `x`\\)',
    fixed = FALSE
  )

  # each absent column is named, with the argument it came from
  expect_error(
    autospc(named_data,
      chart_type = "P", x = nosuch, y = att, n = alsonot,
      period_min = 5L, plot_chart = FALSE
    ),
    '"alsonot" \\(named by `n`\\)',
    fixed = FALSE
  )

  # a denominator column that takes its default name may be absent: a P chart
  # given individual observations has none
  binary <- data.frame(x = 1:30, y = rep(c(TRUE, FALSE, TRUE), 10L))

  expect_s3_class(
    autospc(binary, chart_type = "P", period_min = 5L, plot_chart = FALSE),
    "data.frame"
  )
})


test_that("a missing x column is reported wherever its name came from", {
  # x taking its default name is not an error at the point the columns are
  # selected, because a column that may be absent is skipped there. Every
  # chart type needs x, so the chart validator reports it
  no_x <- data.frame(
    month = 1:30,
    att = as.integer(rep(c(50L, 48L, 52L, 47L, 51L, 49L), 5L))
  )

  expect_error(
    autospc(no_x, chart_type = "C", y = att, period_min = 5L,
      plot_chart = FALSE
    ),
    "x not specified"
  )

  # and where the caller names one that is not there, the column is named
  expect_error(
    autospc(no_x, chart_type = "C", x = nosuch, y = att, period_min = 5L,
      plot_chart = FALSE
    ),
    "Columns not found in the data"
  )
})
