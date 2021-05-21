context("Test formation of display limits following calculation limits")
library(tidyverse)
#source("functions.R")


#load in test data
test_data <- readRDS("testdata/test_data_display_limits.rds")

testthat::test_that("Display period is formed correctly",{
  
  result <- form_display_limits(test_data, counter = 22)
  result_ucl <- result$ucl[22:nrow(test_data)]
  result_cl <- result$cl[22:nrow(test_data)]
  result_lcl <- result$lcl[22:nrow(test_data)]
  results <- list(result_ucl, result_cl, result_lcl)
  
  correct_answer_ucl <- rep(test_data$ucl[21], nrow(test_data) - 21)
  correct_answer_cl <- rep(test_data$cl[21], nrow(test_data) - 21)
  correct_answer_lcl <- rep(test_data$lcl[21], nrow(test_data) - 21)
  correct_answers <- list(correct_answer_ucl, correct_answer_cl, correct_answer_lcl)
  
  testthat::expect_equal(results, correct_answers)
  
})