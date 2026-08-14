#load in test data
test_data <- readRDS("testdata/test_data.rds")

# enough_data_for_new_period() takes both the point count and the thresholds it
# compares them against from the chart object, so one has to be supplied. The
# defaults are period_min 21 and no baseline_length.
test_chart <- autospc_chart_c(data = test_data, x = "x", y = "y")

test_that("Determines correctly whether there are enough data point to form a new period",{
  
  #case when counter is 1
  result1 <- enough_data_for_new_period(data = test_data,
                                        counter = 1,
                                        chart = test_chart)
  
  #case when counter is much less than nrow(data)
  result2 <- enough_data_for_new_period(data = test_data,
                                        counter = 54,
                                        chart = test_chart)
  
  #case when counter is close to end 
  result3 <- enough_data_for_new_period(data = test_data,
                                        counter = 140,
                                        chart = test_chart)
  
  results <- c(result1,
               result2,
               result3)
  
  correct_answers <- c(TRUE,
                       TRUE,
                       FALSE)
  
  
  testthat::expect_equal(results,
                         correct_answers)
  
})
