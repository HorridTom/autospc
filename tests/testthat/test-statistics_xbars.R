# The Xbar statistics against Provost and Murray, pp. 193-194: the centre line
# is the mean of the subgroup means weighted by subgroup size, and sbar the
# mean of the subgroup standard deviations weighted by subgroup size.


test_that("the Xbar centre line and sbar are weighted by subgroup size", {
  statistics <- get_xbar_statistics(
    y = c(10, 12, 11, 14),
    n = c(4, 6, 1, 5),
    s = c(2, 3, NA, 1)
  )

  # the subgroup of one counts towards the centre line but not towards sbar
  expect_equal(statistics$cl, rep((40 + 72 + 11 + 70) / 16, 4))
  expect_equal(statistics$sbar, rep((8 + 18 + 5) / 15, 4))
})


test_that("the Xbar estimate is sbar over c4, and missing for a subgroup of one", {
  statistics <- get_xbar_statistics(
    y = c(10, 12, 11, 14),
    n = c(4, 6, 1, 5),
    s = c(2, 3, NA, 1)
  )

  sbar <- 31 / 15

  expect_equal(
    statistics$sd_estimate,
    c(sbar / c4_constant(4), sbar / c4_constant(6), NA, sbar / c4_constant(5))
  )
})


test_that("an excluded subgroup counts towards neither the centre line nor sbar", {
  statistics <- get_xbar_statistics(
    y = c(10, 12, 11, 14),
    n = c(4, 6, 1, 5),
    s = c(2, 3, NA, 1),
    exclusion_points = 2L
  )

  expect_equal(statistics$cl[[1]], (40 + 11 + 70) / 10)
  expect_equal(statistics$sbar[[1]], (8 + 5) / 9)
})


test_that("three Xbar standard errors are the published A3 times sbar", {
  previous <- options(autospc.rounded_constants = TRUE)
  on.exit(options(previous))

  n <- c(4, 5, 6)
  statistics <- get_xbar_statistics(y = c(10, 12, 11), n = n, s = c(2, 3, 1))

  sbar <- (8 + 15 + 6) / 15

  expect_equal(
    3 * statistics$sd_estimate / sqrt(n),
    c(1.628, 1.427, 1.287) * sbar
  )
})


test_that("sbar is missing where no subgroup holds a standard deviation", {
  expect_identical(sbar_of(n = c(1, 1), s = c(NA_real_, NA_real_)), NA_real_)
})


test_that("the S centre line is sbar, weighted by subgroup size", {
  statistics <- get_s_statistics(s = c(2, 3, 1), n = c(4, 6, 5))

  expect_equal(statistics$cl, rep((8 + 18 + 5) / 15, 3))
})


test_that("the S estimate is that of a subgroup's standard deviation", {
  n <- c(4, 6, 5)
  statistics <- get_s_statistics(s = c(2, 3, 1), n = n)

  sbar <- 31 / 15
  c4 <- c4_constant(n)

  expect_equal(statistics$sd_estimate, sbar * sqrt(1 - c4^2) / c4)
})


test_that("an excluded subgroup does not count towards the S centre line", {
  statistics <- get_s_statistics(
    s = c(2, 3, 1), n = c(4, 6, 5),
    exclusion_points = 2L
  )

  expect_equal(statistics$cl[[1]], (8 + 5) / 9)
})
