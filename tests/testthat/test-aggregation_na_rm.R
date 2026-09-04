# aggregation_na_rm decides what an observation with no value does to the
# subgroup it is summed into.

binary_observations <- function(missing_at = integer(0)) {
  d <- data.frame(
    x = rep(1:30, each = 5L),
    y = rep(c(TRUE, FALSE, TRUE, FALSE, FALSE), 30)
  )
  d$y[missing_at] <- NA

  return(d)
}

counts <- function(missing_at = integer(0)) {
  d <- data.frame(
    x = rep(1:30, each = 3L),
    y = as.numeric(rep(c(4, 5, 3), 30))
  )
  d$y[missing_at] <- NA

  return(d)
}

analyse <- function(d, chart_type, ...) {
  return(autospc(d,
    chart_type = chart_type, x = "x", y = "y",
    plot_chart = FALSE, period_min = 21L, ...
  ))
}


# the default


test_that("aggregation_na_rm defaults to FALSE", {
  expect_false(autospc_default("aggregation_na_rm"))
})


test_that("by default one missing observation makes the subgroup missing", {
  # subgroup 3 is rows 11 to 15, so row 13 is inside it
  result <- analyse(binary_observations(13L), "P\'")

  expect_true(is.na(result$y[3]))
  expect_true(is.na(result$n[3]))
  expect_false(is.na(result$y[2]))
})


# discarding the observation


test_that("aggregation_na_rm discards the observation and keeps the subgroup", {
  result <- analyse(binary_observations(13L), "P\'",
    aggregation_na_rm = TRUE
  )

  expect_false(is.na(result$y[3]))
  expect_identical(result$n[3], 4L)
})


test_that("the numerator and denominator count the same observations", {
  # four observations remain in subgroup 3, one of which is TRUE
  result <- analyse(binary_observations(13L), "P\'",
    aggregation_na_rm = TRUE
  )

  expect_identical(result$y_numerator[3], 1L)
  expect_identical(result$n[3], 4L)
  expect_equal(result$y[3], 25)
})


test_that("a subgroup with no observations left is missing, not absent", {
  # every observation in subgroup 3
  result <- analyse(binary_observations(11:15), "P\'",
    aggregation_na_rm = TRUE
  )

  expect_identical(nrow(result), 30L)
  expect_true(is.na(result$y[3]))
  expect_identical(result$x[3], 3L)
})


test_that("a row is discarded when its denominator has no value", {
  # several records per subgroup, each with its own numerator and denominator
  d <- data.frame(
    x = rep(1:30, each = 3L),
    y = as.numeric(rep(c(4, 5, 3), 30)),
    n = as.numeric(rep(c(10, 10, 10), 30))
  )
  d$n[8] <- NA

  by_default <- autospc(d,
    chart_type = "P\'", x = "x", y = "y", n = "n",
    plot_chart = FALSE, period_min = 21L
  )

  discarded <- autospc(d,
    chart_type = "P\'", x = "x", y = "y", n = "n",
    plot_chart = FALSE, period_min = 21L, aggregation_na_rm = TRUE
  )

  expect_true(is.na(by_default$y[3]))

  # the row with no denominator takes its numerator out with it: 4 + 3 of 20,
  # not 4 + 5 + 3 of 20
  expect_identical(discarded$y_numerator[3], 7)
  expect_identical(discarded$n[3], 20)
})


# every chart type that aggregates


test_that("aggregation_na_rm reaches every chart type that sums observations", {
  # subgroup 3 is rows 7 to 9 of the count data
  for (chart_type in c("C", "C\'")) {
    expect_true(
      is.na(analyse(counts(7L), chart_type)$y[3]),
      info = chart_type
    )
    expect_identical(
      analyse(counts(7L), chart_type, aggregation_na_rm = TRUE)$y[3],
      8,
      info = chart_type
    )
  }

  for (chart_type in c("P", "P\'")) {
    expect_true(
      is.na(analyse(binary_observations(13L), chart_type)$y[3]),
      info = chart_type
    )
    expect_false(
      is.na(analyse(binary_observations(13L), chart_type,
        aggregation_na_rm = TRUE
      )$y[3]),
      info = chart_type
    )
  }
})


test_that("aggregation_na_rm does nothing to a chart that does not aggregate", {
  d <- data.frame(
    x = 1:40,
    y = as.numeric(rep(c(10, 14, 11, 16, 12), 8L))
  )

  for (chart_type in c("X", "MR")) {
    expect_identical(
      analyse(d, chart_type, aggregation_na_rm = TRUE),
      analyse(d, chart_type, aggregation_na_rm = FALSE),
      info = chart_type
    )
  }
})


test_that("aggregation_na_rm does nothing to data already one row per subgroup", {
  d <- data.frame(
    x = 1:30,
    y = as.numeric(rep(c(4, 5, 3, 6, 2), 6L))
  )
  d$y[3] <- NA

  expect_identical(
    analyse(d, "C\'", aggregation_na_rm = TRUE),
    analyse(d, "C\'", aggregation_na_rm = FALSE)
  )
})
