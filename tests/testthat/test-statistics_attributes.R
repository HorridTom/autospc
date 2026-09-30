# The steps shared by the statistics of the C', P, P', U and U' charts.


test_that("the excluded points are left out of the counts and denominators", {
  values <- ratio_values_for_limits(
    y = c(1, 2, 3, 4),
    n = c(10, 20, 30, 40),
    exclusion_points = 2
  )

  expect_identical(values$y, c(1, 3, 4))
  expect_identical(values$n, c(10, 30, 40))
})


test_that("a row missing its count or its denominator is missing in both", {
  values <- ratio_values_for_limits(
    y = c(1, NA, 3),
    n = c(10, 20, NA),
    exclusion_points = NULL
  )

  expect_identical(values$y, c(1, NA, NA))
  expect_identical(values$n, c(10, NA, NA))
})


test_that("counts and denominators that cannot be used are refused", {
  expect_error(
    ratio_values_for_limits(
      y = numeric(0),
      n = numeric(0),
      exclusion_points = NULL
    ),
    "The input data has zero observations."
  )

  expect_error(
    ratio_values_for_limits(
      y = c(1, 2),
      n = 10,
      exclusion_points = NULL
    ),
    "The input y vector is not the same length as the input n vector."
  )

  expect_error(
    ratio_values_for_limits(
      y = c("1", "2"),
      n = c(10, 20),
      exclusion_points = NULL
    ),
    "The input data is not numeric."
  )
})


test_that("sigma_z is the mean moving range of the z-scores over d2", {
  # moving ranges 1 and 2, neither above the screening limit
  expect_equal(
    laney_sigma_z(z = c(0, 1, 3), mr_screen_max_loops = 1),
    1.5 / d2_constant()
  )
})


test_that("sigma_z is taken from the screened moving ranges", {
  # moving ranges 0.1, 0.1, 0.1, 0.1 and 10: the mean is 2.08, which puts the
  # screening limit near 6.8, so one pass removes the 10
  z <- c(0, 0.1, 0, 0.1, 0, 10)

  expect_equal(
    laney_sigma_z(z = z, mr_screen_max_loops = 1),
    0.1 / d2_constant()
  )

  expect_equal(
    laney_sigma_z(z = z, mr_screen_max_loops = 0),
    2.08 / d2_constant()
  )
})
