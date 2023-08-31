#data ends at end of second calculation period
test_data <- structure(list(x = 1:51, 
                            y = c(15L, 
                                  23L, 22L, 18L, 18L, 
                                  13L, 22L, 20L, 25L, 
                                  20L, 14L, 19L, 19L, 
                                  19L, 21L, 24L, 18L, 
                                  25L, 15L, 17L, 23L, 
                                  17L, 16L, 19L, 20L, 
                                  22L, 17L, 24L, 17L, 
                                  21L, 29L, 27L, 26L, 
                                  23L, 23L, 22L, 28L, 
                                  27L, 25L, 31L, 24L, 
                                  25L, 31L, 21L, 23L, 
                                  26L, 
                                  20L, 19L, 20L, 18L, 21L)),  
                       row.names = c(NA, 51L), class = "data.frame")

#point added below line in display period
test_data2 <- dplyr::add_row(test_data, x = 52, y = 18)
test_data3 <- dplyr::add_row(test_data2, x = 53, y = 16)

#eighth rule break point
test_data4 <- dplyr::add_row(test_data3, x = 54, y = 17)

#above the line scenario - stops rule break
test_data5 <- dplyr::add_row(test_data3, x = 54, y = 27)

test_that("No regrets = TRUE",{
  
  output_no_regrets <- autospc::plot_auto_SPC(test_data, noRegrets = T, chartType = "C'", plotChart = F)
  output_no_regrets2 <- autospc::plot_auto_SPC(test_data2, noRegrets = T, chartType = "C'", plotChart = F)
  output_no_regrets3 <- autospc::plot_auto_SPC(test_data3, noRegrets = T, chartType = "C'", plotChart = F)
  output_no_regrets4 <- autospc::plot_auto_SPC(test_data4, noRegrets = T, chartType = "C'", plotChart = F)
  output_no_regrets5 <- autospc::plot_auto_SPC(test_data5, noRegrets = T, chartType = "C'", plotChart = F)

  #expect no breakpoint (no recalculation)
  testthat::expect_equal(sum(output_no_regrets$breakPoint, na.rm = T), 0)
  testthat::expect_equal(sum(output_no_regrets2$breakPoint, na.rm = T), 0)
  testthat::expect_equal(sum(output_no_regrets3$breakPoint, na.rm = T), 0)
  testthat::expect_equal(sum(output_no_regrets4$breakPoint, na.rm = T), 0)
  testthat::expect_equal(sum(output_no_regrets5$breakPoint, na.rm = T), 1)
  
})


test_that("No regrets = FALSE",{

  output_regrets <- autospc::plot_auto_SPC(test_data, noRegrets = F, chartType = "C'", plotChart = F)
  output_regrets2 <- autospc::plot_auto_SPC(test_data2, noRegrets = F, chartType = "C'", plotChart = F)
  output_regrets3 <- autospc::plot_auto_SPC(test_data3, noRegrets = F, chartType = "C'", plotChart = F)
  output_regrets4 <- autospc::plot_auto_SPC(test_data4, noRegrets = F, chartType = "C'", plotChart = F)
  output_regrets5 <- autospc::plot_auto_SPC(test_data5, noRegrets = F, chartType = "C'", plotChart = F)
  
  #expect no breakpoint (no recalculation)
  testthat::expect_equal(sum(output_regrets$breakPoint, na.rm = T), 1)
  testthat::expect_equal(sum(output_regrets2$breakPoint, na.rm = T), 1)
  testthat::expect_equal(sum(output_regrets3$breakPoint, na.rm = T), 1)
  testthat::expect_equal(sum(output_regrets4$breakPoint, na.rm = T), 0)
  testthat::expect_equal(sum(output_regrets5$breakPoint, na.rm = T), 1)
  
})