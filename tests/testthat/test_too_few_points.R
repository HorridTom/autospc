#load in test data
test_data <- data.frame(x = 1:10, 
                        y = c(49, 50, 50, 50, 48, 49, 50, 49, 50, 47),
                        n = c(100, 104, 108, 110, 120, 102, 111, 108, 109, 130))

test_data2 <- data.frame(x = 1:50, 
                         y = c(50, 48, 49, 50, 52, 52, 48, 53, 51, 51, 47, 52, 51, 47, 48, 
                               49, 51, 51, 45, 49, 49, 50, 48, 53, 49, 48, 51, 46, 48, 49, 49, 
                               51, 47, 53, 49, 52, 50, 58, 57, 51, 48, 52, 52, 54, 56, 50, 51, 
                               51, 52, 52),
                         n = c(98, 104, 94, 104, 102, 103, 102, 100, 96, 98, 106, 104, 102, 
                               101, 102, 101, 100, 96, 98, 100, 101, 101, 99, 99, 99, 98, 103, 
                               100, 97, 101, 102, 99, 97, 99, 100, 96, 101, 101, 108, 100, 96, 
                               102, 100, 99, 104, 96, 95, 108, 95, 97))
    

test_that("Charts with fewer points than min period error handle",{
  
  #hide warnings for part of this test
  result_C <- suppressWarnings(autospc(test_data, plotChart = FALSE, chartType = "C", periodMin = 21))
  result_P <- suppressWarnings(autospc(test_data, plotChart = FALSE, chartType = "P", periodMin = 21))

  testthat::expect_equal(ncol(result_C), 4)
  testthat::expect_equal(ncol(result_P), 5)
  testthat::expect_warning(autospc(test_data, plotChart = TRUE, chartType = "C"))
  testthat::expect_warning(autospc(test_data, plotChart = TRUE, chartType = "P"))
  
})

test_that("Charts with showLimits = FALSE behave as expected",{
  
  result_C <- autospc(test_data2, plotChart = FALSE, chartType = "C", periodMin = 21, showLimits = FALSE)
  result_P <- autospc(test_data2, plotChart = FALSE, chartType = "P", periodMin = 21, showLimits = FALSE)
  
  #expect full limits table to be returned regardless of showLimits status
  testthat::expect_equal(ncol(result_C), 14)
  testthat::expect_equal(ncol(result_P), 20)
  
  #expect no warning even for data passed in with too few points
  testthat::expect_warning(autospc(test_data, plotChart = TRUE, chartType = "C", showLimits = FALSE), regexp = NA)
  testthat::expect_warning(autospc(test_data, plotChart = TRUE, chartType = "P", showLimits = FALSE), regexp = NA)
  testthat::expect_warning(autospc(test_data, plotChart = TRUE, chartType = "C", showLimits = FALSE), regexp = NA)
  testthat::expect_warning(autospc(test_data, plotChart = TRUE, chartType = "P", showLimits = FALSE), regexp = NA)
  
  
})
