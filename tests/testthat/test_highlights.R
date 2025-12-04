#load in test data
test_data <- readRDS("testdata/test_highlights_data.rds")
test_data2 <- readRDS("testdata/test_highlights_data2.rds")

test_that("Test rule 2 highlights don't go across periods",{
  
  results_data <- autospc(test_data, chart_type = "C'", plotChart = FALSE)
  results <- results_data$highlight
  
  correct_answers <- c("None", "None", "None", "None", "None", "None", "None", "None", 
                       "None", "None", "None", "None", "None", "None", "None", "None", 
                       "None", "None", "None", "None", "None", "None", "None", "None", 
                       "None", "None", "None", "None", "None", "None", "None", "None", 
                       "None", "None", "None", "None", "None", "None", "None", "None", 
                       "None", "None", "Rule 2", "Rule 2", "Rule 2", "Rule 2", "Rule 2", 
                       "Rule 2", "Rule 2", "Rule 2", "None", "None", "None", "None", 
                       "None", "None", "None", "None", "Excluded from limits calculation", 
                       "None", "None", "None", "None", "None", "None", "None", "None", 
                       "None", "None", "None", "None", "None", "None", "None", "None", 
                       "None", "None", "None", "None", "None", "None")
  
  testthat::expect_equal(results, correct_answers)
  
})


test_that("Test rule 2 highlights don't happen incorrectly at the end of periods",{
  
  results_data <- autospc(test_data2, chart_type = "C'", plotChart = FALSE)
  results <- results_data$highlight
  
  correct_answers <- c("None", "None", "None", "None", "None", "None", "None", "None", 
                       "None", "None", "None", "None", "None", "None", "None", "None", 
                       "None", "None", "None", "None", "None", "None", "None", "None", 
                       "None", "None", "None", "None", "None", "None", "None", "None", 
                       "None", "None", "None", "None", "None", "None", "None", "None", 
                       "None", "None", "Rule 2", "Rule 2", "Rule 2", "Rule 2", "Rule 2", 
                       "Rule 2", "Rule 2", "Rule 2", "None", "None", "None", "None", 
                       "None", "None", "None", "None", "Excluded from limits calculation", 
                       "None", "None", "None", "None", "None", "None", "None", "None", 
                       "None", "None", "None", "None", "None", "None", "None", "None", 
                       "None", "None", "None", "None", "None", "None", "None", "None", 
                       "None", "None", "None", "None", "None", "None", "None", "Rule 1", 
                       "None", "None", "None", "None", "None", "None", "Excluded from limits calculation", 
                       "Excluded from limits calculation", "None", "None", "None", "None", 
                       "None", "None", "None", "None", "None", "None", "None", "None", 
                       "None", "None", "None", "None", "None", "None", "None", "None", 
                       "None")
  
  testthat::expect_equal(results, correct_answers)
  
})

