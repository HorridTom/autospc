# The SPC constants, and the option that chooses between the exact values and
# the published rounded ones.


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


test_that("c4 has its closed form for subgroups of two and three", {
  expect_equal(c4_constant(2), sqrt(2 / pi))
  expect_equal(c4_constant(3), sqrt(pi) / 2)
})


test_that("c4 agrees with the published table to four decimal places", {
  n <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 25)
  published <- c(
    0.7979, 0.8862, 0.9213, 0.9400, 0.9515, 0.9594, 0.9650, 0.9693, 0.9727,
    0.9823, 0.9869, 0.9896
  )

  expect_equal(round(c4_constant(n), 4), published)
})


test_that("c4 is finite for subgroups too large for gamma()", {
  # c4 is 1 - 1 / (4 * n) to within about 2e-7 at n = 1000
  expect_equal(c4_constant(1000), 1 - 1 / 4000, tolerance = 1e-6)
})


test_that("c4 is NA for a subgroup size that is missing or less than 2", {
  expect_no_warning(c4 <- c4_constant(c(NA, -1, 0, 1, 2)))

  expect_identical(is.na(c4), c(TRUE, TRUE, TRUE, TRUE, FALSE))
})


test_that("c4 is the exact value whatever the rounded constants option says", {
  exact <- c4_constant(2:25)

  previous <- options(autospc.rounded_constants = TRUE)
  on.exit(options(previous))

  expect_identical(c4_constant(2:25), exact)
})
