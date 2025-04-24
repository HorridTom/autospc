mnoe_bug_data <- readRDS(file.path("testdata",
                                   "mnoe_bug_data.rds"))

test_that("Bug 90 is fixed: periodMin respected even with NAs", {
  
  result_with_missing_data <- plot_auto_SPC(df = mnoe_bug_data,
                                            chartType = "P'",
                                            periodMin = 24L,
                                            maxNoOfExclusions = 0L,
                                            plotChart = FALSE)
  
  calc_period_lengths <- result_with_missing_data %>% 
    dplyr::filter(!is.na(y),
           periodType == "calculation") %>% 
    dplyr::group_by(plotPeriod) %>%
    dplyr::summarise(period_length = dplyr::n())
  
  periodMin_compliant <- calc_period_lengths %>% 
    dplyr::mutate(compliant = period_length >= 24L) %>%
    dplyr::pull(compliant)
  
  expect_true(all(periodMin_compliant))
  
})
