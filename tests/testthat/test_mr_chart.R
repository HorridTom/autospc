library(testthat)

#load test data 
mr_data <- readRDS("testdata/test_mr_data.rds")
extreme_mr_data <- readRDS("testdata/test_mr_data.rds")

# Correct answer created using:
# test_mr_limit_answer <- qicharts2::qic(y, data = mr_data, chart = 'mr',
#                                         return.data = TRUE)
# qicharts2 v.0.7.2
test_mr_limit_answer <- readRDS(file.path("testdata",
                                         "test_mr_limit_answer.rds"))

# test that mr control limits match those from qicharts2 v.0.7.2
# when mr_screen_max_loops = 0
test_that("mR chart limits the same as qicharts2 v.0.7.2",{
  
  mrs = get_mrs(y = mr_data$y)
  results <- get_mr_limits(mr = mrs,
                           mr_screen_max_loops = 0)
  
  expect_equal(results$cl, test_mr_limit_answer$cl)
  expect_equal(results$ucl, test_mr_limit_answer$ucl)
  expect_equal(results$lcl, rlang::rep_along(test_mr_limit_answer$ucl, 0))
  expect_equal(results$mr, test_mr_limit_answer$y)
})

test_that("mR chart created without error",{
  expect_no_error(
    chart <- plot_auto_SPC(df = mr_data, chartType = "MR", plotChart = TRUE)
  )
})

test_that("mr_screen_max_loops makes no difference to mr chart limits",{
  
  results_table_0 <- plot_auto_SPC(df = extreme_mr_data,
                                   chartType = "MR",
                                   plotChart = FALSE,
                                   mr_screen_max_loops = 0)
  
  results_table_1 <- plot_auto_SPC(df = extreme_mr_data,
                                   chartType = "MR",
                                   plotChart = FALSE,
                                   mr_screen_max_loops = 1)
  
  results_table_inf <- plot_auto_SPC(df = extreme_mr_data,
                                   chartType = "MR",
                                   plotChart = FALSE,
                                   mr_screen_max_loops = Inf)
  
  expect_equal(results_table_1, results_table_0)
  expect_equal(results_table_inf, results_table_0)
  
})
