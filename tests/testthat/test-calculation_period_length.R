# load in test data
test_data <- readRDS("testdata/test_data.rds")

test_that("Calculation period is correct length", {
  period_min <- 21L

  algorithm_results <- autospc(test_data,
    chart_type = "C'",
    period_min = period_min,
    plot_chart = FALSE,
    no_regrets = TRUE
  )

  calculation_period_lengths <- algorithm_results %>%
    dplyr::filter(period_type == "calculation") %>%
    dplyr::group_by(plot_period) %>%
    dplyr::summarise(calc_period_length = dplyr::n()) %>%
    dplyr::pull(calc_period_length)

  correct_lengths <- rep(period_min,
    times = length(calculation_period_lengths)
  )

  testthat::expect_equal(
    calculation_period_lengths,
    correct_lengths
  )
})
