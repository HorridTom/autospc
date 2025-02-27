#load in test data
#test_data <- readRDS("tests/testthat/testdata/test_data.rds")
test_data <- readRDS("testdata/test_data.rds")

test_that("Determines correctly whether there are enough data point to form a new period",{
  
  #case when counter is 1
  result1 <- enough_data_for_new_period(data = test_data, periodMin = 21, counter = 1)
  
  #case when counter is much less than nrow(data)
  result2 <- enough_data_for_new_period(data = test_data, periodMin = 21, counter = 54)
  
  #case when counter is close to end 
  result3 <- enough_data_for_new_period(data = test_data, periodMin = 21, counter = 140)
  
  results <- c(result1, result2, result3)
  
  correct_answers <- c(TRUE,TRUE,FALSE)
  
  
  testthat::expect_equal(results, correct_answers)
  
})


test_that("Correctly established enough data point to form a new period with baseline",{
  
  # case when baseline = NULL
  result1 <- enough_data_for_new_period(data = test_data,
                                        periodMin = 21,
                                        counter = 1)
  
  # case when baseline = periodMin
  result2 <- enough_data_for_new_period(data = test_data,
                                        periodMin = 21,
                                        baseline = 21,
                                        counter = 1)
  
  # case when baseline > length of data
  
  bl <- nrow(test_data) + 1
  
  result3 <- enough_data_for_new_period(data = test_data,
                                        periodMin = 21,
                                        baseline = bl,
                                        counter = 1)
  
  
  results <- c(result1, result2, result3)
  
  correct_answers <- c(TRUE, TRUE, FALSE)
  
  testthat::expect_equal(results, correct_answers)
  
})
