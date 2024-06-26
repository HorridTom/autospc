#load test data 
mr_data <- readRDS("testdata/test_mr_data.rds")

#test that mr control limits match the recalculated mr control limits
library(testthat)
test_that("mR limits match the recalculated mR limits",{
  mrs = get_mrs(y = mr_data$y)
  results <- get_mr_limits(mr = mrs)
  
  correct_answers <- qicharts2::qic(y, data = mr_data, chart = 'mr', return.data = TRUE)
  expect_equal(results$cl, correct_answers$cl)
  expect_equal(results$ucl, correct_answers$ucl)
  expect_equal(results$lcl, rlang::rep_along(correct_answers$ucl, 0))
  expect_equal(results$mr, correct_answers$y)
})

test_that("mR chart created without error",{
  expect_no_error(
    chart <- plot_auto_SPC(df = mr_data, chartType = "MR", plotChart = TRUE)
  )
})
