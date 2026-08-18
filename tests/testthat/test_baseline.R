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


test_that("a baseline longer than the series uses all of the series", {

  thirty <- data.frame(x = 1:30, y = rep(c(10, 14, 11, 16, 12), 6))

  result <- autospc(thirty,
                    chart_type = "C\'",
                    period_min = 21L,
                    baseline_length = 63L,
                    plot_chart = FALSE)

  expect_true("cl" %in% colnames(result))
  expect_identical(sum(result$periodType == "calculation"), 30L)
  expect_equal(unique(result$cl), mean(thirty$y))

})


test_that("period_min is the floor, not baseline_length", {

  # fewer points than period_min, so no limits whatever baseline_length says
  ten <- data.frame(x = 1:10, y = c(10, 14, 11, 16, 12, 13, 15, 11, 14, 12))

  expect_warning(
    result <- autospc(ten,
                      chart_type = "C\'",
                      period_min = 21L,
                      baseline_length = 63L,
                      plot_chart = FALSE),
    "fewer than the minimum number of points")

  expect_false("cl" %in% colnames(result))

})
