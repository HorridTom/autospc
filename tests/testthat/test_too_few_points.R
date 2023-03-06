#load in test data
test_data <- data.frame(x = 1:10, 
                        y = c(49, 50, 50, 50, 48, 49, 50, 49, 50, 47),
                        n = c(100, 104, 108, 110, 120, 102, 111, 108, 109, 130))

test_that("Charts with fewer points than min period error handle",{
  
  result_C <- plot_auto_SPC(test_data, plotChart = FALSE, chartType = "C'")
  result_P <- plot_auto_SPC(test_data, plotChart = FALSE, chartType = "P'")

  testthat::expect_equal(ncol(result_C), 3)
  testthat::expect_equal(ncol(result_P), 4)
  testthat::expect_warning(plot_auto_SPC(test_data, plotChart = TRUE, chartType = "C'"))
  testthat::expect_warning(plot_auto_SPC(test_data, plotChart = TRUE, chartType = "P'"))
  
})