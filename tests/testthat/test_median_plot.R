library(testthat)
library(dplyr)
library(autospc)

# Load test data
test_median_data <- readRDS("testdata/test_median_data.rds")

test_that("the number of medians being plotted are correctly calculated", {
  # Set the n points for the median 
  test_median_n <- 12L 

  # View the data being plotted in XmR chart 
  chart_result <- autospc::plot_auto_SPC(df = test_median_data,
                                  chartType = 'XMR',
                                  floatingMedian = "yes",
                                  floatingMedian_n = test_median_n,
                                  showMR = FALSE)
  
  # View df being used to plot XmR chart 
  chart_result_data <- chart_result$data 

  # Extract all median values from the result data  
  result_median <- chart_result_data %>% 
    dplyr::filter(!is.na(median)) %>% 
    dplyr::pull(median)

  # Test that the length of the test_median__n points matches the length of the median points calculated in the df
  expect_equal(length(result_median),test_median_n)
  
  # Identify how many medians are being calculated 
  unique_result_median <- unique(result_median)
  
  # Test that only one median is being calculated 
  expect_equal(length(unique_result_median), 1L)
  
  # Summaries the df of medians into a singular median value
  result_median <- chart_result_data %>% 
    dplyr::filter(!is.na(median)) %>% 
    dplyr::summarise(medi = median(median)) %>%
    dplyr::pull(medi)
  
  # Calculates the median from slicing: total rows minus (test_median_n + 1)
  correct_median <- chart_result_data %>%
    dplyr::slice((n() - test_median_n + 1):n()) %>%
    dplyr::summarise(med = median(y, na.rm = TRUE)) %>%
    dplyr::pull(med)
  
  # Test that the median displayed is calculated correctly 
  expect_equal(result_median, correct_median)
})

test_that("the median is not generated nor plotted when floatingMedian is set to no", {
  #set the n points for the median 
  test_median_n <- 12L 
  
  #view the data being plotted in XmR chart 
  chart_result <- autospc::plot_auto_SPC(df = test_median_data,
                                         chartType = 'XMR',
                                         floatingMedian = "no",
                                         floatingMedian_n = test_median_n,
                                         showMR = FALSE)
  
  expect_false("median" %in% names(test_median_data))
})

test_that("the median is plotted after a shift rule 2 break when floatingMedian is set to auto", {
  # Load test df where last floatingMedian_n contains at least 1 shift rule 2 break 
  test_autoMedian <- readRDS("testdata/test_medianWithRule2Break.rds")
  
  test_median_n <- 12L 
  
  auto_medianResult <- autospc::plot_auto_SPC(df = test_autoMedian,
                                         chartType = 'XMR',
                                         floatingMedian = "auto",
                                         floatingMedian_n = test_median_n,
                                         showMR = FALSE)
  
  auto_medianResult_data <- auto_medianResult$data 
  
  # Check that the median is calculated when there is a shift rule 2 break in the last floatingMedian_n points 
  expect_true("median" %in% names(auto_medianResult_data))
})

test_that("Median is not plotted when floatingMedian is set to auto and there is not a shift rule 2 break in the last floatingMedian_n rows", {
  # Load test dataset where last 12 points does not contain a shift rule 2 break 
  test_medianNoRule2 <- readRDS("testdata/test_medianNoRule2Breaks.rds")
  
  # Set the n points for the median
  test_median_n <- 12L

  # View the data being plotted in XmR chart
  chart_result <- autospc::plot_auto_SPC(df = test_medianNoRule2,
                                         chartType = 'XMR',
                                         floatingMedian = "auto",
                                         floatingMedian_n = test_median_n,
                                         showMR = FALSE)

  chart_result_data <- chart_result$data
  
  # Test that the median is not calculated not plotted when there is not a shift rule 2 break in last 12L points 
  expect_false("median" %in% names(chart_result_data))
})