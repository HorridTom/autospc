test_rsr_data <- readRDS("testdata/test_rsr_data.rds")

test_that("recalEveryShift works correctly", {
  
  result_data <- autospc(
    test_rsr_data,
    chart_type = "C'",
    x = Month_Start,
    y = Total_Att,
    recalEveryShift = TRUE,
    plotChart = FALSE
  )
  
  result <- result_data %>%
    dplyr::group_by(plotPeriod) %>%
    dplyr::summarise(n_points = dplyr::n(),
                     periodType = dplyr::first(periodType),
                     periodStart = dplyr::first(periodStart)) %>% 
    dplyr::filter(periodType == "calculation") %>%
    dplyr::arrange(plotPeriod)
  
  expect_equal(nrow(result),
               3L)
  
  expect_equal(result %>%
                 dplyr::pull(periodStart),
               c(1L, 30L, 59L))
  
})

