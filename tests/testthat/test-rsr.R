test_rsr_data <- readRDS("testdata/test_rsr_data.rds")

test_that("establish_every_shift works correctly", {
  test_rsr_data <- test_rsr_data %>%
    dplyr::mutate(Total_Att = round(Total_Att))

  result_data <- autospc(
    test_rsr_data,
    chart_type = "C'",
    x = Month_Start,
    y = Total_Att,
    establish_every_shift = TRUE,
    plot_chart = FALSE
  )

  result <- result_data %>%
    dplyr::group_by(plot_period) %>%
    dplyr::summarise(
      n_points = dplyr::n(),
      period_type = dplyr::first(period_type),
      period_start = dplyr::first(period_start)
    ) %>%
    dplyr::filter(period_type == "calculation") %>%
    dplyr::arrange(plot_period)

  expect_equal(
    nrow(result),
    3L
  )

  expect_equal(
    result %>%
      dplyr::pull(period_start),
    c(1L, 30L, 59L)
  )
})
