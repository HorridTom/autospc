context("Test rule 2 highlights are correct")

#load in test data
test_data <- readRDS("testdata/test_highlights_data.rds")

testthat::test_that("Test rule 2 highlights don't go across periods",{
  
  results_data <- plot_auto_SPC(test_data, plotChart = FALSE)
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