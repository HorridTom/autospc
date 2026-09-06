# The limit arithmetic the P and P' charts share. The behaviour these produce
# on a whole chart is covered by test-missing_y.R; this covers what each of
# them returns when it is given nothing to work from.


# limit_width_of


test_that("limit_width_of returns the first value that is not missing", {
  rows <- data.frame(limit_width = c(NA_real_, 150, 150))

  expect_identical(limit_width_of(rows), 150)
})


test_that("limit_width_of returns NA where the column is absent", {
  rows <- data.frame(cl = 20, ucl = 30, n = 100)

  expect_true(is.na(limit_width_of(rows)))
})


test_that("limit_width_of returns NA where every value is missing", {
  rows <- data.frame(limit_width = c(NA_real_, NA_real_))

  expect_true(is.na(limit_width_of(rows)))
})


# denominators_for_missing_rows


test_that("a row's own denominator is used where it has one", {
  period <- data.frame(n = c(100, 100))
  rows <- data.frame(n = c(400, 25))

  expect_identical(
    denominators_for_missing_rows(period = period, rows = rows),
    c(400, 25)
  )
})


test_that("a denominator that is missing or zero takes the period's mean", {
  period <- data.frame(n = c(50, 150))
  rows <- data.frame(n = c(NA_real_, 0))

  expect_identical(
    denominators_for_missing_rows(period = period, rows = rows),
    c(100, 100)
  )
})


test_that("a denominator stays missing where the period has none either", {
  period <- data.frame(n = c(NA_real_, NA_real_))
  rows <- data.frame(n = NA_real_)

  expect_true(is.na(denominators_for_missing_rows(
    period = period,
    rows = rows
  )))
})


# proportion_limits_for_missing_rows


test_that("rows keep the limits they were given where the period holds no
          limit width", {
  period <- data.frame(cl = 20, ucl = 30, n = 100)
  rows <- data.frame(n = 400)
  limits <- list(cl = 20, ucl = 30, lcl = 10)

  expect_identical(
    proportion_limits_for_missing_rows(
      limits = limits,
      period = period,
      rows = rows
    ),
    limits
  )
})


test_that("rows keep the limits they were given where there is no denominator
          to calculate at", {
  period <- data.frame(cl = 20, ucl = 30, n = NA_real_, limit_width = 120)
  rows <- data.frame(n = NA_real_)
  limits <- list(cl = 20, ucl = 30, lcl = 10)

  expect_identical(
    proportion_limits_for_missing_rows(
      limits = limits,
      period = period,
      rows = rows
    ),
    limits
  )
})
