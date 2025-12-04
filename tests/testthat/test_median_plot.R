# Load test data
test_median_data <- readRDS("testdata/test_median_data.rds")

test_that("the series of medians being plotted are correctly calculated when floatingMedian is set to yes", {
  # Set the n points for the median 
  test_median_n <- 12L 
  
  # Create and store XmR chart 
  chart_result <- autospc::autospc(test_median_data,
                                   chart_type = 'XMR',
                                   floatingMedian = "yes",
                                   floatingMedian_n = test_median_n,
                                   showMR = FALSE)
  
  # Store XmR chart data  
  chart_result_data <- chart_result$data 
  
  # Extract all median values from the result data  
  result_median <- chart_result_data %>% 
    dplyr::filter(!is.na(median)) %>% 
    dplyr::pull(median)
  
  # Test that the length of the test_median__n points matches the length of the median points calculated
  expect_equal(length(result_median),test_median_n)
  
  # Identify how many distinct values for the median are being calculated 
  unique_result_median <- unique(result_median)
  
  # Test that only one median is being calculated 
  expect_equal(length(unique_result_median), 1L)
  
  # Summarises the column of medians into a singular median value
  result_median <- chart_result_data %>% 
    dplyr::filter(!is.na(median)) %>% 
    dplyr::summarise(medi = median(median)) %>%
    dplyr::pull(medi)
  
  # Calculates the correct median from the data
  correct_median <- chart_result_data %>%
    dplyr::slice((dplyr::n() - test_median_n + 1):dplyr::n()) %>%
    dplyr::summarise(med = median(y, na.rm = TRUE)) %>%
    dplyr::pull(med)
  
  # Test that the median displayed is calculated correctly 
  expect_equal(result_median, correct_median)
})

test_that("the median is not generated nor plotted when floatingMedian is set to no", {
  # Set the n points for the median 
  test_median_n <- 12L 
  
  # Create and store XmR chart 
  chart_result <- autospc::autospc(test_median_data,
                                   chart_type = 'XMR',
                                   floatingMedian = "no",
                                   floatingMedian_n = test_median_n,
                                   showMR = FALSE)
  # Store XmR chart data  
  chart_result_data <- chart_result$data
  
  # Test that a median column is not generated 
  expect_false("median" %in% names(chart_result_data))
})

test_that("the series of medians being plotted are correctly calculated when floatingMedian is set to auto", {
  # Load test df where last floatingMedian_n contains at least 1 shift rule 2 break 
  test_data_with_rule2_break <- readRDS("testdata/test_medianWithRule2Break.rds")
  
  # Set the n points for the median 
  test_median_n <- 12L 
  
  # Create and store XmR chart
  auto_median_result <- autospc::autospc(test_data_with_rule2_break,
                                         chart_type = 'XMR',
                                         floatingMedian = "auto",
                                         floatingMedian_n = test_median_n,
                                         showMR = FALSE)
  # Store XmR chart data 
  auto_median_result_data <- auto_median_result$data 
  
  # Extract all median values from the result data  
  auto_result_median <- auto_median_result_data %>% 
    dplyr::filter(!is.na(median)) %>% 
    dplyr::pull(median)
  
  # Test that the length of the test_median__n points matches the length of the median points calculated
  expect_equal(length(auto_result_median), test_median_n)
  
  # Identify how many distinct values for the median are being calculated 
  auto_unique_result_median <- unique(auto_result_median)
  
  # Test that only one median is being calculated 
  expect_equal(length(auto_unique_result_median), 1L)
  
  # Summarises the column of medians into a singular median value
  auto_result_median <- auto_median_result_data %>% 
    dplyr::filter(!is.na(median)) %>% 
    dplyr::summarise(med = median(median)) %>%
    dplyr::pull(med)
  
  # Calculates the correct median from the data
  correct_median_auto <- auto_median_result_data %>%
    dplyr::slice((dplyr::n() - test_median_n + 1L):dplyr::n()) %>%
    dplyr::summarise(med = median(y, na.rm = TRUE)) %>%
    dplyr::pull(med)
  
  # Test that the median displayed is calculated correctly 
  expect_equal(auto_result_median, correct_median_auto)
})

test_that("Median is not plotted when floatingMedian is set to auto and there is not a shift rule 2 break in the last floatingMedian_n rows", {
  # Load test df where last 12 points does not contain a shift rule 2 break 
  test_median_without_rule2_break <- readRDS("testdata/test_medianNoRule2Breaks.rds")
  
  # Set the n points for the median
  test_median_n <- 12L
  
  # Create and store XmR chart 
  chart_result <- autospc::autospc(test_median_without_rule2_break,
                                   chart_type = 'XMR',
                                   floatingMedian = "auto",
                                   floatingMedian_n = test_median_n,
                                   showMR = FALSE)
  
  chart_result_data <- chart_result$data
  
  # Test that the median is not calculated nor plotted when there is not a shift rule 2 break in last 12L points 
  expect_false("median" %in% names(chart_result_data))
})


test_that("NAs do not prevent median from being plotted",{
  # Set the n points for the median 
  test_median_n <- 12L 
  
  # Introduce NA withing last floatingMedian_n points
  test_median_data_na <- test_median_data %>% 
    dplyr::mutate(y = dplyr::if_else(dplyr::row_number() == 124L,
                                     NA_integer_,
                                     y))
  
  # Create and store XmR chart 
  chart_result <- autospc::autospc(test_median_data_na,
                                   chart_type = 'XMR',
                                   floatingMedian = "yes",
                                   floatingMedian_n = test_median_n,
                                   showMR = FALSE)
  # Store XmR chart data  
  chart_result_data <- chart_result$data
  
  # Test that a median column is generated 
  expect_true("median" %in% names(chart_result_data))
  
  # Test it is not NA and has the correct value
  result_median <- chart_result_data %>% 
    dplyr::filter(!is.na(median)) %>% 
    dplyr::summarise(medi = median(median)) %>%
    dplyr::pull(medi)
  
  expect_false(is.na(result_median))
  expect_equal(result_median, 9.5)
})

