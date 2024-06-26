library(testthat)
library(dplyr)
library(autospc)

# Load test data
test_median_data <- readRDS("testdata/test_median_data.rds")

test_that("the number of medians calculated match the number of data points used for median calculation", {
  # Set the n points for the median 
  test_median_n <- 12L 

  # View the data being plotted in XmR chart 
  chart_result <- autospc::plot_auto_SPC(df = test_median_data,
                                  chartType = 'XMR',
                                  floatingMedian = "yes",
                                  floatingMedian_n = test_median_n,
                                  showMR = FALSE)

  chart_result_data <- chart_result$data 

  # Filter the median for the the last n points i.e. test_median_n points 
  result_median <- chart_result_data %>% 
    dplyr::filter(!is.na(median)) %>% 
    dplyr::pull(median)

  # Test that the length of the test_median__n points matches the length of the 
  # median points listed 
  expect_equal(length(result_median),test_median_n)
})

test_that("only one median is being calculated",{
  #set the n points for the median 
  test_median_n <- 12L 
  
  #view the data being plotted in XmR chart 
  chart_result <- autospc::plot_auto_SPC(df = test_median_data,
                                         chartType = 'XMR',
                                         floatingMedian = "yes",
                                         floatingMedian_n = test_median_n,
                                         showMR = FALSE)
  
  chart_result_data <- chart_result$data 
  
  # Filter the median for the the last n points i.e. test_median_n points 
  result_median <- chart_result_data %>% 
    dplyr::filter(!is.na(median)) %>% 
    dplyr::pull(median)
  
  unique_result_median <- unique(result_median)

  expect_equal(length(unique_result_median), 1L)
})

test_that("the calculated median uses the last test_median_n rows", {
  #set the n points for the median 
  test_median_n <- 12L 
  
  #view the data being plotted in XmR chart 
  chart_result <- autospc::plot_auto_SPC(df = test_median_data,
                                         chartType = 'XMR',
                                         floatingMedian = "yes",
                                         floatingMedian_n = test_median_n,
                                         showMR = FALSE)
  
  chart_result_data <- chart_result$data 
  
  # Summaries the list of medians into a singular median value
  result_median <- chart_result_data %>% 
    dplyr::filter(!is.na(median)) %>% 
    dplyr::summarise(medi = median(median)) %>%
    dplyr::pull(medi)
  
  # Calculates the median from slicing: total rows minus (test_median_n + 1)
  correct_median <- chart_result_data %>%
    dplyr::slice((n() - test_median_n + 1):n()) %>%
    dplyr::summarise(med = median(y, na.rm = TRUE)) %>%
    dplyr::pull(med)

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
  # Load test dataset where last floatingMedian_n contains at least 1 shift rule 2 break 
  test_autoMedian <- readRDS("testdata/test_oppositeRuleBreak_later.rds")
  
  test_median_n <- 12L 
  
  auto_medianResult <- autospc::plot_auto_SPC(df = test_autoMedian,
                                         chartType = 'XMR',
                                         floatingMedian = "auto",
                                         floatingMedian_n = test_median_n,
                                         showMR = FALSE)
  
  auto_medianResult_data <- auto_medianResult$data 
  
  # Check that both the median and rule2 columns exist 
  expect_true("median" %in% names(auto_medianResult_data))
  expect_true("rule2" %in% names(auto_medianResult_data))
  
  # iterate through each row in the median column that is not NA
  # if the median column has a value, the corresponding rule2 column should be TRUE
  for (i in seq_len(nrow(auto_medianResult_data))){
    if (!is.na(auto_medianResult_data$median[i])){
      expect_true(auto_medianResult_data$rule2[i])
    }
  }
})

test_that("Median is not plotted when floatingMedian is set to auto and there is not a shift rule 2 break in the last floatingMedian_n rows", {
  # Load test dataset whose floatingMedian_n does not contain a shift rule 2 break 
  test_medianNoRule2 <- readRDS("testdata/test_medianNoRule2Breaks.rds")
  
  #set the n points for the median
  test_median_n <- 12L

  #view the data being plotted in XmR chart
  chart_result <- autospc::plot_auto_SPC(df = test_medianNoRule2,
                                         chartType = 'XMR',
                                         floatingMedian = "auto",
                                         floatingMedian_n = test_median_n,
                                         showMR = FALSE)

  chart_result_data <- chart_result$data
  
  expect_false("median" %in% names(chart_result_data))
})