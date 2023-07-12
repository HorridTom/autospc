library(testthat)
#load in test data
test_data <- readRDS("testdata/test_individual_data.rds")

#test that the i chart limits is the same as qicharts2 results 
test_that("I chart limits the same as live qicharts2 v.0.7.2",{
  results <- get_i_limits(y = test_data$y)
  
  correct_answers <- qicharts2::qic(x, y, data = test_data, chart = 'i', return.data = TRUE)
  
  expect_equal(results$cl, correct_answers$cl)
  expect_equal(results$lcl, correct_answers$lcl)
  expect_equal(results$ucl, correct_answers$ucl)
  
})

#test that moving range limits that are above ucl_mr are removed 
extreme_mr_data <- readRDS("testdata/test_mr_data.rds")
test_that("I chart limits the same as live qicharts2 v.0.7.2",{
  results <- get_i_limits(y = extreme_mr_data$y)
  
  correct_answers <- qicharts2::qic(x, y, data = extreme_mr_data, chart = 'i', return.data = TRUE)
  
  expect_equal(results$cl, correct_answers$cl)
  expect_equal(results$lcl, correct_answers$lcl)
  expect_equal(results$ucl, correct_answers$ucl)
  
})