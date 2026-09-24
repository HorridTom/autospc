# The functions that give limits to the rows outside the calculation rows. The
# behaviour they produce on a whole chart is covered by
# test-form_display_limits.R, test-missing_y.R and test-extend_limits.R; this
# covers what each returns.


# limits_at_rows


test_that("limits_at_rows gives every row the period's limits", {
  chart <- structure(list(), class = c("autospc_chart_c", "autospc_chart"))

  limits <- limits_at_rows(chart,
    statistics = list(cl = 11, sd_estimate = 7 / 3),
    rows = data.frame(x = 1:2)
  )

  expect_equal(limits, list(
    cl = c(11, 11), ucl = c(18, 18), lcl = c(4, 4),
    sd_estimate = c(7 / 3, 7 / 3)
  ))
})


test_that("limits_at_rows forms a P chart's limits at each row's denominator", {
  chart <- structure(list(), class = c("autospc_chart_p", "autospc_chart"))

  limits <- limits_at_rows(chart,
    statistics = list(cl = 15, sd_estimate = 20),
    rows = data.frame(n = c(25, 400))
  )

  expect_equal(limits$ucl, 15 + 3 * 20 / sqrt(c(25, 400)))
  expect_equal(limits$lcl, 15 - 3 * 20 / sqrt(c(25, 400)))
})


test_that("limits_at_rows constrains the limits", {
  chart <- structure(list(), class = c("autospc_chart_c", "autospc_chart"))

  limits <- limits_at_rows(chart,
    statistics = list(cl = 2, sd_estimate = 1),
    rows = data.frame(x = 1)
  )

  expect_identical(limits$lcl, 0)
})


test_that("limits_at_rows gives NA to every row where there are no statistics", {
  chart <- structure(list(), class = c("autospc_chart_c", "autospc_chart"))

  limits <- limits_at_rows(chart, statistics = NULL, rows = data.frame(x = 1:2))

  expect_named(limits, c("cl", "ucl", "lcl", "sd_estimate"))
  expect_true(all(is.na(unlist(limits))))
  expect_length(limits$cl, 2L)
})


# sd_estimate_at


test_that("the default gives every row the period's one estimate", {
  chart <- structure(list(), class = c("autospc_chart_c", "autospc_chart"))

  expect_identical(
    sd_estimate_at(chart,
      statistics = list(cl = 11, sd_estimate = 7 / 3),
      rows = data.frame(x = 1:3)
    ),
    rep(7 / 3, 3)
  )
})


# period_statistics


test_that("period_statistics reads the first row that holds both", {
  rows <- data.frame(
    cl = c(10, 10, 12),
    sd_estimate = c(NA_real_, 150, 160)
  )

  expect_identical(period_statistics(rows), list(cl = 10, sd_estimate = 150))
})


test_that("period_statistics reads sbar from the same row where it is there", {
  rows <- data.frame(
    cl = c(10, 10, 10),
    sd_estimate = c(NA_real_, 4, 5),
    sbar = c(3, 3.5, 3.5)
  )

  expect_identical(
    period_statistics(rows),
    list(cl = 10, sd_estimate = 4, sbar = 3.5)
  )
})


test_that("period_statistics is NULL where a column is absent", {
  expect_null(period_statistics(data.frame(cl = 20, ucl = 30, n = 100)))
})


test_that("period_statistics is NULL where no row holds both", {
  rows <- data.frame(cl = c(10, NA_real_), sd_estimate = c(NA_real_, 150))

  expect_null(period_statistics(rows))
})


# has_denominator


test_that("has_denominator is TRUE for the classes whose limits vary with n", {
  classes <- c("c", "cp", "p", "pp", "x", "mr")

  has <- vapply(classes, function(class) {
    has_denominator(structure(list(),
      class = c(paste0("autospc_chart_", class), "autospc_chart")
    ))
  }, logical(1), USE.NAMES = FALSE)

  expect_identical(has, c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE))
})


# denominators_for_missing_rows


p_chart <- structure(list(), class = c("autospc_chart_p", "autospc_chart"))


test_that("a row's own denominator is used where it has one", {
  period <- data.frame(series = c(10, 12), n = c(100, 100), excluded = FALSE)
  rows <- data.frame(n = c(400, 25))

  expect_identical(
    denominators_for_missing_rows(p_chart, period = period, rows = rows),
    c(400, 25)
  )
})


test_that("a denominator that is missing or zero takes the period's mean", {
  period <- data.frame(series = c(10, 12), n = c(50, 150), excluded = FALSE)
  rows <- data.frame(n = c(NA_real_, 0))

  expect_identical(
    denominators_for_missing_rows(p_chart, period = period, rows = rows),
    c(100, 100)
  )
})


test_that("a denominator stays missing where the period has none either", {
  period <- data.frame(series = NA_real_, n = NA_real_, excluded = NA)
  rows <- data.frame(n = NA_real_)

  expect_true(is.na(denominators_for_missing_rows(p_chart,
    period = period,
    rows = rows
  )))
})


# mean_denominator


test_that("the mean denominator leaves out excluded points and gaps", {
  # the excluded point and the row with no observation each carry a
  # denominator that would move the mean
  period <- data.frame(
    series = c(10, 12, 60, NA_real_),
    n = c(50, 150, 10, 1000),
    excluded = c(FALSE, FALSE, TRUE, NA)
  )

  expect_identical(mean_denominator(p_chart, period = period), 100)
})


test_that("the mean denominator counts display rows", {
  # a display row is not part of any calculation, so its excluded is NA
  period <- data.frame(series = c(10, 12), n = c(50, 150), excluded = NA)

  expect_identical(mean_denominator(p_chart, period = period), 100)
})


test_that("the mean denominator is NA where no row counts", {
  period <- data.frame(
    series = c(60, NA_real_), n = c(10, 1000), excluded = c(TRUE, NA)
  )

  expect_identical(mean_denominator(p_chart, period = period), NA_real_)
})
