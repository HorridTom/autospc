test_that("", {
  
  result_data <- autospc(
    ed_attendances_monthly,
    chart_type = "C'",
    x = Month_Start,
    y = Att_All,
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
  
})
