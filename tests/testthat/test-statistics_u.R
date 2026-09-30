# U and U' chart statistics for a period, checked against Provost and Murray,
# The Health Care Data Guide, p. 200, and Laney (2002), worked out in each test
# from the data.


test_that("the U centre line is the total count over the total denominator", {
  y <- c(1, 4, 3, 8)
  n <- c(1, 2, 1, 2)

  statistics <- get_u_statistics(y = y, n = n)

  expect_equal(statistics$cl, rep(16 / 6, 4))
  expect_equal(statistics$sd_estimate, rep(sqrt(16 / 6), 4))
})


test_that("the U statistics leave out the excluded points", {
  statistics <- get_u_statistics(
    y = c(1, 4, 3, 8),
    n = c(1, 2, 1, 2),
    exclusion_points = 4
  )

  expect_equal(statistics$cl, rep(8 / 4, 4))
  expect_length(statistics$sd_estimate, 4)
})


test_that("the U statistics leave out a row with a missing count", {
  statistics <- get_u_statistics(
    y = c(1, NA, 3, 8),
    n = c(1, 2, 1, 2)
  )

  expect_equal(statistics$cl, rep(12 / 4, 4))
})


test_that("the U statistics take denominators that are not whole numbers", {
  statistics <- get_u_statistics(
    y = c(1, 2),
    n = c(0.5, 1.5)
  )

  expect_equal(statistics$cl, rep(3 / 2, 2))
})


test_that("the U' estimate is the U estimate times Laney's sigma_z", {
  y <- c(1, 4, 3, 8, 2)
  n <- c(1, 2, 1, 2, 1)

  statistics <- get_up_statistics(y = y, n = n)

  ubar <- sum(y) / sum(n)
  z <- (y / n - ubar) / sqrt(ubar / n)
  mr <- abs(diff(z))

  # no moving range is above the screening limit, so none is removed
  expect_true(all(mr <= dd4_constant() * mean(mr)))

  sigma_z <- mean(mr) / d2_constant()

  expect_equal(statistics$cl, rep(ubar, 5))
  expect_equal(statistics$sd_estimate, rep(sqrt(ubar) * sigma_z, 5))
})


test_that("with equal denominators U' has the limits of an X chart of rates", {
  # Laney (2002), observation 2
  y <- c(12, 7, 15, 9, 11, 20, 8)
  n <- rep(4, 7)

  statistics <- get_up_statistics(y = y, n = n)

  u <- y / n
  mr <- abs(diff(u))
  expect_true(all(mr <= dd4_constant() * mean(mr)))

  expect_equal(
    statistics$sd_estimate / sqrt(n),
    rep(mean(mr) / d2_constant(), 7)
  )
})


test_that("the U' statistics screen the moving ranges as many times as asked", {
  # the rates move by 0.1 until the last, which jumps; screening removes the
  # jump's moving range
  y <- c(10, 11, 10, 11, 10, 11, 40)
  n <- rep(10, 7)

  screened <- get_up_statistics(y = y, n = n, mr_screen_max_loops = 1)
  unscreened <- get_up_statistics(y = y, n = n, mr_screen_max_loops = 0)

  u <- y / n
  mr <- abs(diff(u))

  expect_equal(
    screened$sd_estimate / sqrt(n),
    rep(mean(mr[1:5]) / d2_constant(), 7)
  )
  expect_equal(
    unscreened$sd_estimate / sqrt(n),
    rep(mean(mr) / d2_constant(), 7)
  )
})


test_that("the U' statistics leave out the excluded points", {
  y <- c(1, 4, 3, 8, 2)
  n <- c(1, 2, 1, 2, 1)

  statistics <- get_up_statistics(y = y, n = n, exclusion_points = 2)

  kept_y <- y[-2]
  kept_n <- n[-2]
  ubar <- sum(kept_y) / sum(kept_n)
  z <- (kept_y / kept_n - ubar) / sqrt(ubar / kept_n)
  sigma_z <- mean(abs(diff(z))) / d2_constant()

  expect_equal(statistics$cl, rep(ubar, 5))
  expect_equal(statistics$sd_estimate, rep(sqrt(ubar) * sigma_z, 5))
})
