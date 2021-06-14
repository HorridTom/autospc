context("Test scanning of rule 2 breaks")
library(tidyverse)
source("../../R/functions.R")


#load in test data
test_data <- readRDS("testdata/test_data_rule2_break.rds")

testthat::test_that("Start of subsequent rule 2 breaks identified correctly",{
  
  results <- rule2_break_start_positions(test_data, counter = 22)
  
  correct_answers <- c(22, 36, 123, 161, 176, 218)
  
  testthat::expect_equal(results, correct_answers)
  
})