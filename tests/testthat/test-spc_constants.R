# The constants for a subgroup of two, and the option that chooses between the
# exact values and the published rounded ones.


test_that("the exact constants are used unless the option says otherwise", {
  expect_equal(d2_constant(), 2 / sqrt(pi))
  expect_equal(mr_upper_limit_factor(), 1 + 3 * sqrt(2 * (1 - 2 / pi)) /
    (2 / sqrt(pi)))
})


test_that("the option selects the published rounded constants", {
  previous <- options(autospc.rounded_constants = TRUE)
  on.exit(options(previous))

  expect_identical(d2_constant(), 1.128)

  # the published D4, not one worked out from the rounded d2 and d3
  expect_identical(mr_upper_limit_factor(), 3.267)
})


test_that("a value other than TRUE leaves the exact constants in use", {
  previous <- options(autospc.rounded_constants = "banana")
  on.exit(options(previous))

  expect_equal(d2_constant(), 2 / sqrt(pi))
})


test_that("the rounded constants give slightly wider X chart limits", {
  exact <- autospc(example_series_1,
    chart_type = "X", period_min = 21L, plot_chart = FALSE
  )

  previous <- options(autospc.rounded_constants = TRUE)
  on.exit(options(previous))

  rounded <- autospc(example_series_1,
    chart_type = "X", period_min = 21L, plot_chart = FALSE
  )

  # sigma is the mean moving range over d2, so a smaller d2 widens the limits
  # by the ratio of the two values
  expect_equal(
    (rounded$ucl - rounded$cl) / (exact$ucl - exact$cl),
    rep((2 / sqrt(pi)) / 1.128, nrow(exact))
  )
})


test_that("the rounded constants give a slightly higher MR upper limit", {
  exact <- autospc(example_series_1,
    chart_type = "MR", period_min = 21L, plot_chart = FALSE
  )

  previous <- options(autospc.rounded_constants = TRUE)
  on.exit(options(previous))

  rounded <- autospc(example_series_1,
    chart_type = "MR", period_min = 21L, plot_chart = FALSE
  )

  expect_equal(
    rounded$ucl / exact$ucl,
    rep(3.267 / (1 + 3 * sqrt(2 * (1 - 2 / pi)) / (2 / sqrt(pi))), nrow(exact))
  )
})
