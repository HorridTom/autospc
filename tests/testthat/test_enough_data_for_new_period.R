#load in test data
test_data <- readRDS("testdata/test_data.rds")

test_that("Determines correctly whether there are enough data point to form a new period",{
  
  #case when counter is 1
  result1 <- enough_data_for_new_period(data = test_data,
                                        periodMin = 21,
                                        counter = 1,
                                        chartType = "C")
  
  #case when counter is much less than nrow(data)
  result2 <- enough_data_for_new_period(data = test_data,
                                        periodMin = 21,
                                        counter = 54,
                                        chartType = "C")
  
  #case when counter is close to end 
  result3 <- enough_data_for_new_period(data = test_data,
                                        periodMin = 21,
                                        counter = 140,
                                        chartType = "C")
  
  results <- c(result1,
               result2,
               result3)
  
  correct_answers <- c(TRUE,
                       TRUE,
                       FALSE)
  
  
  testthat::expect_equal(results,
                         correct_answers)
  
})
