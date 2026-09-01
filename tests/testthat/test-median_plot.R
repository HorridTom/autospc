# Load test data
test_median_data <- readRDS("testdata/test_median_data.rds")

test_that("the series of medians being plotted are correctly calculated when floating_median is set to yes", {
  # Set the n points for the median
  test_median_n <- 12L

  # Create and store XmR chart
  chart_result <- autospc::autospc(test_median_data,
    chart_type = "X",
    floating_median = "yes",
    floating_median_n = test_median_n
  )

  # Store XmR chart data
  chart_result_data <- chart_result$data

  # Extract all median values from the result data
  result_median <- chart_result_data %>%
    dplyr::filter(series == "y") %>%
    dplyr::filter(!is.na(median)) %>%
    dplyr::pull(median)

  # Test that the length of the test_median__n points matches the length of
  # the median points calculated
  expect_equal(length(result_median), test_median_n)

  # Identify how many distinct values for the median are being calculated
  unique_result_median <- unique(result_median)

  # Test that only one median is being calculated
  expect_equal(length(unique_result_median), 1L)

  # Summarises the column of medians into a singular median value
  result_median <- chart_result_data %>%
    dplyr::filter(series == "y") %>%
    dplyr::filter(!is.na(median)) %>%
    dplyr::summarise(medi = median(median)) %>%
    dplyr::pull(medi)

  # Calculates the correct median from the data
  correct_median <- chart_result_data %>%
    dplyr::filter(series == "y") %>%
    dplyr::slice((dplyr::n() - test_median_n + 1):dplyr::n()) %>%
    dplyr::summarise(med = median(value,
      na.rm = TRUE
    )) %>%
    dplyr::pull(med)

  # Test that the median displayed is calculated correctly
  expect_equal(result_median, correct_median)
})

test_that("the median is not generated nor plotted when floating_median is set to no", {
  # Set the n points for the median
  test_median_n <- 12L

  # Create and store XmR chart
  chart_result <- autospc::autospc(test_median_data,
    chart_type = "X",
    floating_median = "no",
    floating_median_n = test_median_n
  )
  # Store XmR chart data
  chart_result_data <- chart_result$data

  # Test that a median column is not generated
  expect_false("median" %in% names(chart_result_data))
})

test_that("the series of medians being plotted are correctly calculated when floating_median is set to auto", {
  # Load test df where last floating_median_n contains at least 1 shift rule 2
  # break
  test_data_with_rule2_break <- readRDS(
    "testdata/test_medianWithRule2Break.rds"
  )

  # Set the n points for the median
  test_median_n <- 12L

  # Create and store XmR chart
  auto_median_result <- autospc::autospc(test_data_with_rule2_break,
    chart_type = "X",
    floating_median = "auto",
    floating_median_n = test_median_n
  )
  # Store XmR chart data
  auto_median_result_data <- auto_median_result$data

  # Extract all median values from the result data
  auto_result_median <- auto_median_result_data %>%
    dplyr::filter(series == "y") %>%
    dplyr::filter(!is.na(median)) %>%
    dplyr::pull(median)

  # Test that the length of the test_median__n points matches the length of
  # the median points calculated
  expect_equal(length(auto_result_median), test_median_n)

  # Identify how many distinct values for the median are being calculated
  auto_unique_result_median <- unique(auto_result_median)

  # Test that only one median is being calculated
  expect_equal(length(auto_unique_result_median), 1L)

  # Summarises the column of medians into a singular median value
  auto_result_median <- auto_median_result_data %>%
    dplyr::filter(series == "y") %>%
    dplyr::filter(!is.na(median)) %>%
    dplyr::summarise(med = median(median)) %>%
    dplyr::pull(med)

  # Calculates the correct median from the data
  correct_median_auto <- auto_median_result_data %>%
    dplyr::filter(series == "y") %>%
    dplyr::slice((dplyr::n() - test_median_n + 1L):dplyr::n()) %>%
    dplyr::summarise(med = median(value, na.rm = TRUE)) %>%
    dplyr::pull(med)

  # Test that the median displayed is calculated correctly
  expect_equal(auto_result_median, correct_median_auto)
})

test_that("Median is not plotted when floating_median is set to auto and there is not a shift rule 2 break in the last floating_median_n rows", {
  # Load test df where last 12 points does not contain a shift rule 2 break
  test_median_without_rule2_break <- readRDS(
    "testdata/test_medianNoRule2Breaks.rds"
  )

  # Set the n points for the median
  test_median_n <- 12L

  # Create and store XmR chart
  chart_result <- autospc::autospc(test_median_without_rule2_break,
    chart_type = "X",
    floating_median = "auto",
    floating_median_n = test_median_n
  )

  chart_result_data <- chart_result$data

  # Test that the median is not calculated nor plotted when there is not a
  # shift rule 2 break in last 12L points
  expect_false("median" %in% names(chart_result_data))
})


test_that("NAs do not prevent median from being plotted", {
  # Set the n points for the median
  test_median_n <- 12L

  # Introduce NA withing last floating_median_n points
  test_median_data_na <- test_median_data %>%
    dplyr::mutate(y = dplyr::if_else(dplyr::row_number() == 124L,
      NA_integer_,
      y
    ))

  # Create and store XmR chart
  chart_result <- autospc::autospc(test_median_data_na,
    chart_type = "X",
    floating_median = "yes",
    floating_median_n = test_median_n
  )
  # Store XmR chart data
  chart_result_data <- chart_result$data

  # Test that a median column is generated
  expect_true("median" %in% names(chart_result_data))

  # Test it is not NA and has the correct value
  result_median <- chart_result_data %>%
    dplyr::filter(series == "y") %>%
    dplyr::filter(!is.na(median)) %>%
    dplyr::summarise(medi = median(median)) %>%
    dplyr::pull(medi)

  expect_false(is.na(result_median))
  expect_equal(result_median, 9.5)
})


# the window is a chart field, so it has to travel from autospc() to both
# the analysis and the label


test_that("a non-default floating_median_n reaches the analysis", {
  medians <- function(n) {
    result <- autospc(test_median_data,
      chart_type = "X",
      floating_median = "yes",
      floating_median_n = n,
      plot_chart = FALSE
    )
    sum(!is.na(result$median))
  }

  expect_identical(medians(8L), 8L)

  expect_identical(medians(20L), 20L)
})


test_that("a non-default floating_median_n reaches the label", {
  label_x <- function(n) {
    plot <- autospc(test_median_data,
      chart_type = "X",
      floating_median = "yes",
      floating_median_n = n
    )
    built <- ggplot2::ggplot_build(plot)$data
    labelled <- Filter(function(layer) "label" %in% names(layer), built)
    median_label <- Filter(
      function(layer) all(layer$label == "Median"),
      labelled
    )
    median_label[[1]]$x
  }

  # the label sits at the start of the window, so a wider window moves it left
  expect_lt(label_x(20L), label_x(8L))
})
