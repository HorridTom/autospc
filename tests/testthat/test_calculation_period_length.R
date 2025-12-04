#load in test data
test_data <- readRDS("testdata/test_data.rds")

test_that("Calculation period is correct length",{
  
  period_min <- 21L
  
  algorithm_results <- autospc(test_data,
                                     chart_type = "C'",
                                     period_min = period_min,
                                     plotChart = FALSE,
                                     noRegrets = TRUE)
  
  calculation_period_lengths <- algorithm_results %>% 
    dplyr::filter(periodType == "calculation") %>% 
    dplyr::group_by(plotPeriod) %>% 
    dplyr::summarise(calcPeriodLength = dplyr::n()) %>% 
    dplyr::pull(calcPeriodLength)
  
  correct_lengths <- rep(period_min,
                         times = length(calculation_period_lengths))
  
  testthat::expect_equal(calculation_period_lengths,
                         correct_lengths)
  
})
