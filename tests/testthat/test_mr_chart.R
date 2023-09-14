#load test data 
mr_data <- readRDS("testdata/test_mr_data.rds")

#test that mr control limits match the recalculated mr control limits
library(testthat)
test_that("mR limits match the recalculated mR limits",{
  results <- get_mr_limits(y = mr_data$y)
  
  correct_answers <- qicharts2::qic(y, data = mr_data, chart = 'mr', return.data = TRUE)
  expect_equal(results$cl, correct_answers$cl)
  expect_equal(results$ucl, correct_answers$ucl)
})

test_that("mR chart created without error",{
  expect_no_error(
    chart <- plot_auto_SPC(df = mr_data, chartType = "MR", plotChart = TRUE)
  )
})
