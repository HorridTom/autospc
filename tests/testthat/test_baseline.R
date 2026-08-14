test_that("baseline_length sets the first period and the limits it gives", {

  result_data <- autospc(
    ed_attendances_monthly,
    chart_type = "C'",
    x = month_start,
    y = att_all,
    period_min = 21L,
    baseline_length = 63L,
    plot_chart = FALSE
  )

  result <- result_data %>%
    dplyr::group_by(plotPeriod) %>%
    dplyr::summarise(n_points = dplyr::n(),
                     periodType = dplyr::first(periodType)) %>%
    dplyr::filter(periodType == "calculation") %>%
    dplyr::arrange(plotPeriod)

  expect_equal(nrow(result),
               2L)

  expect_equal(result %>%
                 dplyr::pull(n_points),
               c(63, 21))

  baseline <- result_data[1:63, ]

  # Extremes are searched for across all baseline_length points, so row 59 is
  # excluded even though it lies beyond period_min
  expect_equal(which(baseline$excluded),
               c(2L, 21L, 59L))

  expect_equal(result_data$cl[1],
               mean(baseline$y[!baseline$excluded]))

})
