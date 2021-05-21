context("Test formation of calculation limits for specified period after counter")
library(tidyverse)
#source("functions.R")


#load in test data
test_data <- readRDS("testdata/test_data.rds")

testthat::test_that("Calculation period is formed correctly",{
  
  result_counter_zero <- form_calculation_limits(test_data, counter = 0, periodMin = 21)
  result_counter_zero_ucl <- result_counter_zero$ucl[1:21]
  result_counter_zero_cl <- result_counter_zero$cl[1:21]
  result_counter_zero_lcl <- result_counter_zero$lcl[1:21]
  
  result_counter_100 <- form_calculation_limits(result_counter_zero, counter = 100, periodMin = 21)
  result_counter_100_ucl <- result_counter_100$ucl[100:120]
  result_counter_100_cl <- result_counter_100$cl[100:120]
  result_counter_100_lcl <- result_counter_100$lcl[100:120]
  
  correct_answer_counter_zero_ucl <- rep(93.6978506306795, 21)
  correct_answer_counter_zero_cl <- rep(68.8119585108865, 21)
  correct_answer_counter_zero_lcl <- rep(43.9260663910934, 21)

  correct_answer_counter_100_ucl <- rep(58.89114, 21)
  correct_answer_counter_100_cl <- rep(39.9333, 21)
  correct_answer_counter_100_lcl <- rep(20.97546, 21)
  
  correct_answer_counter_100_ucl
  correct_answer_counter_100_cl
  correct_answer_counter_100_lcl
  
  
  testthat::expect_equal(result_counter_zero_ucl,correct_answer_counter_zero_ucl)
  testthat::expect_equal(result_counter_zero_cl,correct_answer_counter_zero_cl)
  testthat::expect_equal(result_counter_zero_lcl,correct_answer_counter_zero_lcl)
  
  testthat::expect_equal(result_counter_100_ucl,correct_answer_counter_100_ucl)
  testthat::expect_equal(result_counter_100_cl,correct_answer_counter_100_cl)
  testthat::expect_equal(result_counter_100_lcl,correct_answer_counter_100_lcl)
  
})