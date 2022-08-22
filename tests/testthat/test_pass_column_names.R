test_data <- readRDS("testdata/test_data.rds")

test_that("Renaming columns doesn't change the result",{
  
  test_data1 <- test_data %>%
    dplyr::select(x, y)
  
  test_data2 <- test_data %>%
    dplyr::select(month = x, count = y)
  
  result1 <- plot_auto_SPC(test_data1,
                           plotChart = FALSE)
  
  result2 <- plot_auto_SPC(test_data2,
                           x = month,
                           y = count,
                           plotChart = FALSE)
  
  testthat::expect_equal(result1, result2)
  
})
