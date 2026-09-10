# What split_at counts: points in the analysed series, which holds one point
# per subgroup in x order. It used to count rows of the data as supplied.

observations <- data.frame(
  x = rep(1:24, each = 3L),
  y = rep(c(10L, 12L, 11L), 24L)
)


test_that("split_at counts subgroups, not rows of the data as supplied", {
  # seventy-two observations of twenty-four subgroups
  result <- suppressWarnings(
    facet_stages(observations,
      split_at = c(12L, 24L),
      chart_type = "C", period_min = 5L, plot_chart = FALSE
    )
  )

  expect_identical(as.integer(table(result$stage)), c(12L, 24L))
})


test_that("split_at counts the series in x order", {
  # the analysed series is ordered by x, so the first twelve points are the
  # twelve lowest x whatever order the rows arrived in
  shuffled <- observations[order(observations$x %% 5L), ]

  result <- suppressWarnings(
    facet_stages(shuffled,
      split_at = 12L,
      chart_type = "C", period_min = 5L, plot_chart = FALSE
    )
  )

  first_stage <- result[result$stage == unique(result$stage)[1L], ]

  expect_identical(first_stage$x, 1:12)
})


test_that("aggregation_na_rm reaches the aggregation before the split", {
  # the series is aggregated before it is split and again for each stage, so
  # an aggregation before the split that kept a missing observation would
  # leave a subgroup with no value that the caller asked to be summed over the
  # observations that have one
  with_gap <- observations
  with_gap$y[5L] <- NA

  result <- suppressWarnings(
    facet_stages(with_gap,
      split_at = c(12L, 24L),
      chart_type = "C", period_min = 5L, aggregation_na_rm = TRUE,
      plot_chart = FALSE
    )
  )

  first_stage <- result[result$stage == unique(result$stage)[1L], ]

  expect_identical(first_stage$y[2L], 21L)
})
