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
