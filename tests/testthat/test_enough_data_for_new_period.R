#load in test data
test_data <- readRDS("testdata/test_data.rds")

test_that("Determines correctly whether there are enough data point to form a new period",{
  
  #case when counter is 1
  result1 <- enough_data_for_new_period(data = test_data,
                                        period_min = 21,
                                        baseline = NULL,
                                        counter = 1,
                                        chart_type = "C")
  
  #case when counter is much less than nrow(data)
  result2 <- enough_data_for_new_period(data = test_data,
                                        period_min = 21,
                                        baseline = NULL,
                                        counter = 54,
                                        chart_type = "C")
  
  #case when counter is close to end 
  result3 <- enough_data_for_new_period(data = test_data,
                                        period_min = 21,
                                        baseline = NULL,
                                        counter = 140,
                                        chart_type = "C")
  
  results <- c(result1,
               result2,
               result3)
  
  correct_answers <- c(TRUE,
                       TRUE,
                       FALSE)
  
  
  testthat::expect_equal(results,
                         correct_answers)
  
})
