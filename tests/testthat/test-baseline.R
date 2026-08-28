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
    dplyr::group_by(plot_period) %>%
    dplyr::summarise(
      n_points = dplyr::n(),
      period_type = dplyr::first(period_type)
    ) %>%
    dplyr::filter(period_type == "calculation") %>%
    dplyr::arrange(plot_period)

  expect_equal(
    nrow(result),
    2L
  )

  expect_equal(
    result %>%
      dplyr::pull(n_points),
    c(63, 21)
  )

  baseline <- result_data[1:63, ]

  # Extremes are searched for across all baseline_length points, so row 59 is
  # excluded even though it lies beyond period_min
  expect_equal(
    which(baseline$excluded),
    c(2L, 21L, 59L)
  )

  expect_equal(
    result_data$cl[1],
    mean(baseline$y[!baseline$excluded])
  )
})


test_that("a baseline longer than the series uses all of the series", {
  thirty <- data.frame(x = 1:30, y = rep(c(10, 14, 11, 16, 12), 6))

  result <- autospc(thirty,
    chart_type = "C\'",
    period_min = 21L,
    baseline_length = 63L,
    plot_chart = FALSE
  )

  expect_true("cl" %in% colnames(result))
  expect_identical(sum(result$period_type == "calculation"), 30L)
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
      plot_chart = FALSE
    ),
    "fewer than the minimum number of points"
  )

  expect_false("cl" %in% colnames(result))
})


# baseline_only


analysed_ed <- function(...) {
  autospc(ed_attendances_monthly,
    chart_type = "C'",
    x = month_start,
    y = att_all,
    period_min = 21L,
    plot_chart = FALSE,
    ...
  )
}


n_calculation_periods <- function(result) {
  calculation <- result[result$period_type == "calculation", ]

  length(unique(calculation$plot_period))
}


test_that("baseline_only stops the limits being re-established", {
  # this series re-establishes its limits several times when it is allowed to,
  # so a single calculation period is the effect of the setting rather than of
  # the data
  expect_identical(n_calculation_periods(analysed_ed(baseline_only = TRUE)), 1L)

  expect_gt(n_calculation_periods(analysed_ed(baseline_only = FALSE)), 1L)
})


test_that("baseline_only keeps the limits of the first period throughout", {
  result <- analysed_ed(baseline_only = TRUE)

  expect_identical(unique(result$cl), result$cl[1])

  # the points after the calculation period are drawn against those limits
  expect_setequal(unique(result$period_type), c("calculation", "display"))
})
