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
  result_C <- suppressWarnings(autospc(test_data, plot_chart = FALSE, chart_type = "C", period_min = 21))
  result_P <- suppressWarnings(autospc(test_data, plot_chart = FALSE, chart_type = "P", period_min = 21))

  # x, y and log. aggregate_data() summarises to the columns the class
  # analyses, so a C chart drops n
  testthat::expect_equal(ncol(result_C), 3)
  testthat::expect_equal(ncol(result_P), 5)
  testthat::expect_warning(autospc(test_data, plot_chart = TRUE, chart_type = "C"))
  testthat::expect_warning(autospc(test_data, plot_chart = TRUE, chart_type = "P"))
  
})

test_that("Charts with show_limits = FALSE behave as expected",{
  
  result_C <- autospc(test_data2, plot_chart = FALSE, chart_type = "C", period_min = 21, show_limits = FALSE)
  result_P <- autospc(test_data2, plot_chart = FALSE, chart_type = "P", period_min = 21, show_limits = FALSE)
  
  #expect full limits table to be returned regardless of show_limits status,
  #including the four columns describing the periods
  testthat::expect_equal(ncol(result_C), 18)
  testthat::expect_equal(ncol(result_P), 24)

  testthat::expect_true(all(c("limitChange", "periodStart", "plotPeriod",
                              "cl_change") %in% colnames(result_C)))
  
  #expect no warning even for data passed in with too few points
  testthat::expect_warning(autospc(test_data, plot_chart = TRUE, chart_type = "C", show_limits = FALSE), regexp = NA)
  testthat::expect_warning(autospc(test_data, plot_chart = TRUE, chart_type = "P", show_limits = FALSE), regexp = NA)
  testthat::expect_warning(autospc(test_data, plot_chart = TRUE, chart_type = "C", show_limits = FALSE), regexp = NA)
  testthat::expect_warning(autospc(test_data, plot_chart = TRUE, chart_type = "P", show_limits = FALSE), regexp = NA)
  
  
})


test_that("the warning is about the input data, not about named charts", {

  # a faceted chart names the stages that are short; a single chart or a pair
  # has only the one series to talk about
  expect_warning(
    autospc(test_data, plot_chart = FALSE, chart_type = "XMR",
            period_min = 21L),
    "^The input data has fewer than the minimum number of points"
  )

})


test_that("centre_line_present answers whether a table carries a centre line", {

  expect_true(centre_line_present(data.frame(x = 1, y = 1, cl = 1)))

  expect_false(centre_line_present(data.frame(x = 1, y = 1)))

})


test_that("a series with limits takes the limits path", {

  result <- autospc(test_data2, plot_chart = FALSE, chart_type = "C",
                    period_min = 21)

  # postprocess_spc() runs, so the presentation columns are there
  expect_true(all(c("limitChange", "annotation_level", "plotPeriod") %in%
                    colnames(result)))

})


test_that("a series without limits does not take the limits path", {

  result <- suppressWarnings(
    autospc(test_data, plot_chart = FALSE, chart_type = "C", period_min = 21)
  )

  expect_false(centre_line_present(result))

  expect_false("limitChange" %in% colnames(result))

})
