# The P and P' methods delegate here, so the aggregation behaviour itself is
# covered by their test files. What is tested here is the part no class
# exercises yet: allow_individual_observations = FALSE, which is what the rate
# charts (u and u') will pass when they exist.

ratio_binary_data <- data.frame(
  x = rep(1:5, each = 4),
  y = rep(c(TRUE, FALSE, TRUE, TRUE), 5)
)

ratio_counts_data <- data.frame(
  x = rep(1:3, each = 2),
  y = c(1, 2, 3, 4, 5, 6),
  n = rep(10L, 6)
)


test_that("aggregate_ratios sums y and n over x regardless of the flag", {
  chart <- autospc_chart_p(ratio_counts_data, x = "x", y = "y", n = "n")

  allowed <- aggregate_ratios(chart, allow_individual_observations = TRUE)
  refused <- aggregate_ratios(chart, allow_individual_observations = FALSE)

  expect_identical(allowed$data$y, c(3, 7, 11))
  expect_identical(allowed$data, refused$data)
})


test_that("aggregate_ratios materialises n when individual observations are allowed", {
  chart <- autospc_chart_p(ratio_binary_data, x = "x", y = "y", n = "n")

  aggregated <- aggregate_ratios(chart, allow_individual_observations = TRUE)

  expect_identical(aggregated$data$y, rep(3L, 5))
  expect_identical(aggregated$data$n, rep(4L, 5))
})


test_that("aggregate_ratios does not materialise n when they are not allowed", {
  # a rate chart has no individual-observation form, so the denominator must
  # come from the data; with none supplied there is nothing to sum
  chart <- autospc_chart_p(ratio_binary_data, x = "x", y = "y", n = "n")

  expect_error(aggregate_ratios(chart, allow_individual_observations = FALSE))
})


test_that("aggregate_ratios preserves the chart class", {
  chart <- autospc_chart_pp(ratio_counts_data, x = "x", y = "y", n = "n")

  aggregated <- aggregate_ratios(chart, allow_individual_observations = TRUE)

  expect_identical(
    class(aggregated),
    c("autospc_chart_pp", "autospc_chart")
  )
})


# aggregate_xbars_statistics, shared by the Xbar and S classes

# three subgroups of observations: four, one and three
subgroup_1 <- c(10, 12, 11, 15)
subgroup_2 <- 20
subgroup_3 <- c(7, 9, 8)

subgroup_observations <- data.frame(
  x = rep(1:3, c(4, 1, 3)),
  y = c(subgroup_1, subgroup_2, subgroup_3)
)


test_that("observations are combined into each subgroup's mean, size and sd", {
  chart <- aggregate_data(autospc_chart_xbar(
    data = subgroup_observations, x = "x", y = "y"
  ))

  expect_equal(
    chart$data$y,
    c(mean(subgroup_1), subgroup_2, mean(subgroup_3))
  )
  expect_equal(chart$data$n, c(4, 1, 3))

  # a subgroup of one observation has no standard deviation
  expect_equal(
    chart$data$s,
    c(stats::sd(subgroup_1), NA, stats::sd(subgroup_3))
  )
})


test_that("a missing observation leaves its subgroup with no mean by default", {
  # the second observation of subgroup 1
  data <- subgroup_observations
  data$y[2] <- NA

  chart <- aggregate_data(autospc_chart_xbar(data = data, x = "x", y = "y"))

  expect_true(is.na(chart$data$y[1]))
  expect_true(is.na(chart$data$s[1]))
  expect_equal(chart$data$n[1], 4)
})


test_that("aggregation_na_rm leaves the missing observation out", {
  # the second observation of subgroup 1
  data <- subgroup_observations
  data$y[2] <- NA

  chart <- aggregate_data(autospc_chart_xbar(
    data = data, x = "x", y = "y", aggregation_na_rm = TRUE
  ))

  expect_equal(chart$data$y[1], mean(subgroup_1[-2]))
  expect_equal(chart$data$n[1], 3)
  expect_equal(chart$data$s[1], stats::sd(subgroup_1[-2]))
})


test_that("summarised rows sharing an x combine to the observations' figures", {
  # subgroup 1 given as two rows, each summarising two of its observations;
  # subgroups 2 and 3 given as one row each
  first_half <- subgroup_1[1:2]
  second_half <- subgroup_1[3:4]

  summarised <- data.frame(
    x = c(1, 1, 2, 3),
    y = c(mean(first_half), mean(second_half), subgroup_2, mean(subgroup_3)),
    n = c(2, 2, 1, 3),
    s = c(
      stats::sd(first_half), stats::sd(second_half), NA,
      stats::sd(subgroup_3)
    )
  )

  chart <- aggregate_data(autospc_chart_xbar(
    data = summarised, x = "x", y = "y", n = "n", s = "s"
  ))

  expect_equal(
    chart$data$y,
    c(mean(subgroup_1), subgroup_2, mean(subgroup_3))
  )
  expect_equal(chart$data$n, c(4, 1, 3))
  expect_equal(
    chart$data$s,
    c(stats::sd(subgroup_1), NA, stats::sd(subgroup_3))
  )
})


test_that("rows of one observation count towards the standard deviation", {
  # the same five observations as one summarised row, and as five rows of one
  five <- c(10, 12, 11, 15, 13)

  one_row <- data.frame(x = 1, y = mean(five), n = 5, s = stats::sd(five))
  five_rows <- data.frame(x = 1, y = five, n = 1, s = NA_real_)

  from_one_row <- aggregate_data(autospc_chart_xbar(
    data = one_row, x = "x", y = "y", n = "n", s = "s"
  ))
  from_five_rows <- aggregate_data(autospc_chart_xbar(
    data = five_rows, x = "x", y = "y", n = "n", s = "s"
  ))

  expect_equal(from_five_rows$data$s, stats::sd(five))
  expect_equal(from_five_rows$data, from_one_row$data)
})
