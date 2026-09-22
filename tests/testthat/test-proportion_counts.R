# The range a P or P' chart's numerator and denominator have to lie in, and
# what the chart does with a subgroup that had no opportunities.

counts_series <- function() {
  set.seed(5)

  data <- data.frame(x = 1:30, n = rep(10L, 30L))
  data$y <- as.integer(stats::rbinom(30L, 10L, 0.5))

  return(data)
}


analysed <- function(data, chart_type = "P", ...) {
  return(suppressWarnings(autospc(data,
    chart_type = chart_type, x = x, y = y, n = n, period_min = 21L,
    plot_chart = FALSE, ...
  )))
}


with_row <- function(row, y = NULL, n = NULL) {
  data <- counts_series()

  if (!is.null(y)) {
    data$y[row] <- y
  }

  if (!is.null(n)) {
    data$n[row] <- n
  }

  return(data)
}


test_that("a numerator above the denominator is refused", {
  for (chart_type in c("P", "P'")) {
    expect_error(
      analysed(with_row(7L, y = 13L), chart_type = chart_type),
      paste0("For a ", chart_type, " chart, y must be a count from 0 to n"),
      fixed = TRUE
    )
  }
})


test_that("a numerator below zero is refused", {
  expect_error(
    analysed(with_row(5L, y = -3L)),
    "y must be a count from 0 to n"
  )
})


test_that("the error names the rows at fault and their values", {
  expect_error(
    analysed(with_row(7L, y = 13L)),
    "Outside it: row 7 (y = 13, n = 10).",
    fixed = TRUE
  )
})


test_that("up to five rows are named, and the rest counted", {
  data <- counts_series()
  data$y[c(2L, 5L, 7L, 9L, 12L, 15L, 20L)] <- 13L

  expect_error(analysed(data), "row 2 (y = 13, n = 10)", fixed = TRUE)
  expect_error(analysed(data), "row 12 (y = 13, n = 10), and 2 more.",
    fixed = TRUE
  )

  # the sixth and seventh are counted rather than named
  reported <- tryCatch(analysed(data), error = conditionMessage)

  expect_false(grepl("row 15", reported, fixed = TRUE))
  expect_false(grepl("row 20", reported, fixed = TRUE))
})


test_that("a negative denominator is refused", {
  expect_error(
    analysed(with_row(3L, n = -5L)),
    "For a P chart, n cannot be negative. Negative: row 3 (n = -5).",
    fixed = TRUE
  )
})


test_that("the denominator is reported before the numerator", {
  data <- with_row(3L, n = -5L)
  data$y[7] <- 13L

  # both rules are broken; the denominator is the one that makes the numerator
  # rule meaningless, so it is the one named
  expect_error(analysed(data), "n cannot be negative")
})


test_that("a subgroup with no opportunities and no events is kept", {
  # a week with no patients at a small clinic
  data <- with_row(3L, y = 0L, n = 0L)

  table <- analysed(data)

  expect_true(is.na(table$series[3L]))
  expect_false(is.na(table$cl[3L]))
  expect_identical(table$n[3L], 0L)
  expect_identical(table$y[3L], 0L)
})


test_that("events against no opportunities are refused", {
  expect_error(
    analysed(with_row(3L, y = 7L, n = 0L)),
    "y must be a count from 0 to n"
  )
})


test_that("the range is checked after the counts are rounded", {
  rounds_in <- counts_series()
  rounds_in$y <- as.numeric(rounds_in$y)
  rounds_in$y[7] <- 10.4

  # 10.4 rounds to 10, which the denominator of 10 allows
  expect_identical(max(analysed(rounds_in)$series), 100)

  rounds_out <- rounds_in
  rounds_out$y[7] <- 10.6

  expect_error(analysed(rounds_out), "row 7 (y = 11, n = 10)", fixed = TRUE)
})


test_that("a missing numerator or denominator is not at fault", {
  missing_y <- with_row(4L, y = NA_integer_)
  missing_n <- with_row(4L, n = NA_integer_)

  expect_true(is.na(analysed(missing_y)$series[4L]))
  expect_true(is.na(analysed(missing_n)$series[4L]))
})


test_that("individual binary observations are not range checked", {
  set.seed(2)

  data <- data.frame(x = rep(1:15, each = 4L))
  data$y <- as.logical(stats::rbinom(60L, 1L, 0.5))

  table <- suppressWarnings(autospc(data,
    chart_type = "P", x = x, y = y, period_min = 10L, plot_chart = FALSE
  ))

  # y is TRUE or FALSE and n is one observation per row, so the range cannot be
  # exceeded and there is nothing to check
  expect_lte(max(table$series), 100)
})
