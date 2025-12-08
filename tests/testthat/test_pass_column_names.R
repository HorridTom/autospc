test_data <- readRDS("testdata/test_data.rds")

test_that("Renaming columns doesn't change the result",{
  
  test_data1 <- test_data %>%
    dplyr::select(x, y)
  
  test_data2 <- test_data %>%
    dplyr::select(month = x, count = y)
  
  result1 <- autospc(test_data1,
                           chart_type = "C'",
                           plot_chart = FALSE)
  
  result2 <- autospc(test_data2,
                           x = month,
                           y = count,
                           chart_type = "C'",
                           plot_chart = FALSE)
  
  testthat::expect_equal(result1, result2)
  
})
