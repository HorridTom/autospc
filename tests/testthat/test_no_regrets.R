context("Test no regrets policy as more data is added")

#data ends at end of second calculation period
test_data <- structure(list(x = 1:51, 
                            y = c(14.9, 
                                  22.9, 21.9, 18.0, 18.4, 
                                  13.0, 22.3, 19.5, 24.6, 
                                  19.7, 14.4, 18.8, 19.1, 
                                  18.8, 21.3, 23.5, 18.3, 
                                  25.4, 14.8, 17.4, 23.3, 
                                  16.8, 15.6, 19.0, 20.3, 
                                  21.6, 16.6, 24.0, 16.7, 
                                  20.3, 29.3, 27.2, 26.2, 
                                  23.1, 22.5, 22.0, 27.9, 
                                  27.0, 24.8, 31.0, 24.2, 
                                  24.5, 31.2, 20.6, 23, 
                                  26, 
                                  20, 19, 20, 18, 21)),  
                            row.names = c(NA, 51L), class = "data.frame")

#point added below line in display period
test_data2 <- dplyr::add_row(test_data, x = 52, y = 18)
test_data3 <- dplyr::add_row(test_data2, x = 53, y = 16)

#eighth rule break point
test_data4 <- dplyr::add_row(test_data3, x = 54, y = 17)

#above the line scenario - stops rule break
test_data5 <- dplyr::add_row(test_data3, x = 54, y = 27)

testthat::test_that("No regrets = TRUE",{
  
  output_no_regrets <- autospc::plot_auto_SPC(test_data, noRegrets = T, plotChart = F)
  output_no_regrets2 <- autospc::plot_auto_SPC(test_data2, noRegrets = T, plotChart = F)
  output_no_regrets3 <- autospc::plot_auto_SPC(test_data3, noRegrets = T, plotChart = F)
  output_no_regrets4 <- autospc::plot_auto_SPC(test_data4, noRegrets = T, plotChart = F)
  output_no_regrets5 <- autospc::plot_auto_SPC(test_data5, noRegrets = T, plotChart = F)

  #expect no breakpoint (no recalculation)
  testthat::expect_equal(sum(output_no_regrets$breakPoint, na.rm = T), 0)
  testthat::expect_equal(sum(output_no_regrets2$breakPoint, na.rm = T), 0)
  testthat::expect_equal(sum(output_no_regrets3$breakPoint, na.rm = T), 0)
  testthat::expect_equal(sum(output_no_regrets4$breakPoint, na.rm = T), 0)
  testthat::expect_equal(sum(output_no_regrets5$breakPoint, na.rm = T), 1)
  
})



testthat::test_that("No regrets = FALSE",{

  output_regrets <- autospc::plot_auto_SPC(test_data, noRegrets = F, plotChart = F)
  output_regrets2 <- autospc::plot_auto_SPC(test_data2, noRegrets = F, plotChart = F)
  output_regrets3 <- autospc::plot_auto_SPC(test_data3, noRegrets = F, plotChart = F)
  output_regrets4 <- autospc::plot_auto_SPC(test_data4, noRegrets = F, plotChart = F)
  output_regrets5 <- autospc::plot_auto_SPC(test_data5, noRegrets = F, plotChart = F)
  
  #expect no breakpoint (no recalculation)
  testthat::expect_equal(sum(output_regrets$breakPoint, na.rm = T), 1)
  testthat::expect_equal(sum(output_regrets2$breakPoint, na.rm = T), 1)
  testthat::expect_equal(sum(output_regrets3$breakPoint, na.rm = T), 1)
  testthat::expect_equal(sum(output_regrets4$breakPoint, na.rm = T), 0)
  testthat::expect_equal(sum(output_regrets5$breakPoint, na.rm = T), 1)
  
})