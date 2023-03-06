#load in test data
#zeros in n column only
test_data1 <- data.frame(x = 1:50, 
                         y = c(53, 53, 53, 45, 49, 54, 48, 48,
                               55, 52, 52, 50, 45, 52, 49, 51, 
                               52, 50, 54, 47, 27, 25, 23,
                               28, 22, 23, 25, 27,
                               26, 28, 20, 22, 
                               24, 21, 29, 28, 28, 21, 20, 22, 
                               22, 25, 27, 29, 21, 27, 20, 23, 
                               22, 22), 
                         n = c(197, 196, 203, 201, 201, 200, 195, 
                               197, 200, 202, 203, 199, 197, 201, 
                               205, 201, 199, 201, 200, 192, 201, 
                               202, 203,
                               0, 0, 0, 0, 0, 
                               200, 197, 201, 205, 198, 194, 200, 
                               201, 201, 195, 200, 191, 194, 206, 
                               205, 199, 202, 195, 199, 197, 201, 
                               198))

#zeros in both y and n
test_data2 <- data.frame(x = 1:50, 
                         y = c(53, 53, 53, 45, 49, 54, 48, 48,
                               55, 52, 52, 50, 45, 52, 49, 51, 
                               52, 50, 54, 47, 27, 25, 23,
                               0, 0, 0, 0, 0,
                               26, 28, 20, 22, 
                               24, 21, 29, 28, 28, 21, 20, 22, 
                               22, 25, 27, 29, 21, 27, 20, 23, 
                               22, 22), 
                         n = c(197, 196, 203, 201, 201, 200, 195, 
                               197, 200, 202, 203, 199, 197, 201, 
                               205, 201, 199, 201, 200, 192, 201, 
                               202, 203,
                               0, 0, 0, 0, 0, 
                               200, 197, 201, 205, 198, 194, 200, 
                               201, 201, 195, 200, 191, 194, 206, 
                               205, 199, 202, 195, 199, 197, 201, 
                               198))
                                                                                                 

test_that("P charts with zero attendances error handle",{
  
  result1 <- plot_auto_SPC(test_data1, chartType = "P'", plotChart = FALSE, periodMin = 21) %>%
    dplyr::select(x, y, n, y_numerator, ucl, lcl, cl)
  
  result2 <- plot_auto_SPC(test_data2, chartType = "P'", plotChart = FALSE, periodMin = 21) %>%
    dplyr::select(x, y, n, y_numerator, ucl, lcl, cl)
  
  testthat::expect_equal(all(is.na(result1$y[24:28])), TRUE)
  testthat::expect_equal(all(is.na(result1$ucl[24:28])), TRUE)
  testthat::expect_equal(all(is.na(result1$lcl[24:28])), TRUE)
  testthat::expect_equal(all(is.na(result1$cl[24:28])), TRUE)
  
  testthat::expect_equal(all(is.na(result2$y[24:28])), TRUE)
  testthat::expect_equal(all(is.na(result2$ucl[24:28])), TRUE)
  testthat::expect_equal(all(is.na(result2$lcl[24:28])), TRUE)
  testthat::expect_equal(all(is.na(result2$cl[24:28])), TRUE)
  
})