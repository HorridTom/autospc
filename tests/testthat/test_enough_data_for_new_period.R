context("Test if there is enough data to form a new period")
library(tidyverse)
library(testthat)
#source("../../R/functions.R")
#testthat::test_dir("tests/testthat")


#load in test data
#test_data <- readRDS("tests/testthat/testdata/test_data.rds")
test_data <- readRDS("testdata/test_data.rds")

testthat::test_that("Determines correctly whether there are enough data point to form a new period",{
  
  #case when counter is 0
  result1 <- enough_data_for_new_period(data = test_data, periodMin = 21, counter = 0)
  
  #case when counter is much less than nrow(data)
  result2 <- enough_data_for_new_period(data = test_data, periodMin = 21, counter = 54)
  
  #case when counter is close to end 
  result3 <- enough_data_for_new_period(data = test_data, periodMin = 21, counter = 140)
  
  results <- c(result1, result2, result3)
  
  correct_answers <- c(T,T,F)
  
  
  testthat::expect_equal(results, correct_answers)
  
})
