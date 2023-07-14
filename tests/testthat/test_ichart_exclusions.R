library(testthat)
#load in test data
test_data <- readRDS("testdata/test_individual_data.rds")

#modify the data to include some extreme values
test_data[6,2] <- 200
test_data[15,2] <- 150


#test that the i chart limits is the same as qicharts2 results 
test_that("I chart limits the same as live qicharts2 v.0.7.2",{
  results <- get_i_limits(y = test_data$y,
                          exclusion_points = c(6, 15))
  
  correct_answers <- qicharts2::qic(x,
                                    y,
                                    data = test_data,
                                    chart = 'i',
                                    exclude = c(6, 15),
                                    return.data = TRUE)
  
  expect_equal(results$cl, correct_answers$cl)
  expect_equal(results$lcl, correct_answers$lcl)
  expect_equal(results$ucl, correct_answers$ucl)
  
})
