mnoe_bug_data <- readRDS(file.path("testdata",
                                   "mnoe_bug_data.rds"))

test_that("Bug 90 is fixed: periodMin respected even with NAs", {
  
  # The mnoe_bug_data has missing data such that there is 
  # insufficient data to re-establish limits
  result_with_missing_data <- autospc(mnoe_bug_data,
                                      chart_type = "P'",
                                      periodMin = 24L,
                                      maxNoOfExclusions = 0L,
                                      plotChart = FALSE)
  
  # Establish the number of (non-missing) data points within each calculation
  # period in the algorithm results
  calc_period_lengths <- result_with_missing_data %>% 
    dplyr::filter(!is.na(y),
                  periodType == "calculation") %>% 
    dplyr::group_by(plotPeriod) %>%
    dplyr::summarise(period_length = dplyr::n())
  
  # Check whether each calculation period is compliant with the specified 
  # periodMin = 24
  periodMin_compliant <- calc_period_lengths %>% 
    dplyr::mutate(compliant = period_length >= 24L) %>%
    dplyr::pull(compliant)
  
  # All calculation periods should be compliant in this way
  expect_true(all(periodMin_compliant))
  
})
