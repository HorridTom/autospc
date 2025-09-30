test_that("", {
  
  result_data <- plot_auto_SPC(
    ed_attendances_monthly,
    chartType = "C'",
    x = Month_Start,
    y = Att_All,
    periodMin = 21L,
    baseline = 63L,
    plotChart = FALSE
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