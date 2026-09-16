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
    dplyr::filter(plotted_line == "series") %>%
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
    dplyr::filter(plotted_line == "series") %>%
    dplyr::filter(!is.na(median)) %>%
    dplyr::summarise(medi = median(median)) %>%
    dplyr::pull(medi)

  # Calculates the correct median from the data
  correct_median <- chart_result_data %>%
    dplyr::filter(plotted_line == "series") %>%
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

  # Test that no median is calculated
  expect_true(all(is.na(chart_result_data$median)))
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
    dplyr::filter(plotted_line == "series") %>%
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
    dplyr::filter(plotted_line == "series") %>%
    dplyr::filter(!is.na(median)) %>%
    dplyr::summarise(med = median(median)) %>%
    dplyr::pull(med)

  # Calculates the correct median from the data
  correct_median_auto <- auto_median_result_data %>%
    dplyr::filter(plotted_line == "series") %>%
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
  expect_true(all(is.na(chart_result_data$median)))
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

  # Test that a median is calculated
  expect_false(all(is.na(chart_result_data$median)))

  # Test it is not NA and has the correct value
  result_median <- chart_result_data %>%
    dplyr::filter(plotted_line == "series") %>%
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


# A series holding fewer points with a value than floating_median_n has none
# for the median to be taken over


short_median_data <- data.frame(
  x = 1:9,
  y = as.integer(c(10, 11, 10, 12, 11, 18, 19, 20, 19))
)


short_median_chart <- function(floating_median, data = short_median_data) {
  return(autospc(data,
    chart_type = "C",
    period_min = 5L,
    floating_median = floating_median,
    plot_chart = FALSE
  ))
}


test_that("too few points with a value means no floating median", {
  expect_true(all(is.na(
    suppressWarnings(short_median_chart("yes"))$median
  )))
})


test_that("asking for a floating median on too short a series warns", {
  expect_warning(
    short_median_chart("yes"),
    "taken over the last 12 points that have a value, and this series has 9"
  )
})


test_that("auto is silent on too short a series", {
  # "auto" asked the package to decide, and deciding not to draw one is an
  # answer rather than a failure
  expect_no_warning(short_median_chart("auto"))

  expect_true(all(is.na(short_median_chart("auto")$median)))
})


test_that("a short series does not warn about taking a maximum of nothing", {
  # the position the median window starts at used to be worked out before
  # anything asked whether there was a median to draw, and max() of no values
  # warns and gives -Inf
  expect_no_warning(short_median_chart("no"))
})


test_that("a series of exactly floating_median_n points gets a median", {
  twelve <- data.frame(
    x = 1:12,
    y = as.integer(c(10, 11, 10, 12, 11, 18, 19, 20, 19, 17, 18, 19))
  )

  result <- short_median_chart("yes", data = twelve)

  expect_identical(sum(!is.na(result$median)), 12L)
})


test_that("points with no value do not count towards floating_median_n", {
  # fourteen points, three of them holding no value, so eleven have one
  with_gaps <- data.frame(
    x = 1:14,
    y = as.integer(c(10, NA, 11, 10, 12, NA, 11, 18, 19, 20, NA, 19, 17, 18))
  )

  expect_warning(
    short_median_chart("yes", data = with_gaps),
    "this series has 11"
  )
})


# A series too short to establish control limits


no_limits_data <- data.frame(
  x = 1:14,
  y = as.integer(c(10, 11, 10, 12, 11, 18, 19, 20, 19, 17, 18, 19, 18, 20))
)


no_limits_chart <- function(floating_median, ...) {
  return(suppressWarnings(autospc(no_limits_data,
    chart_type = "C",
    period_min = 21L,
    floating_median = floating_median,
    ...
  )))
}


median_value_of <- function(table) {
  return(as.numeric(unique(stats::na.omit(table$median))))
}


drawn_medians <- function(plot) {
  built <- ggplot2::ggplot_build(plot)

  return(list(
    values = lapply(built$data, function(layer) {
      as.numeric(unique(stats::na.omit(layer$y)))
    }),
    labels = unlist(lapply(built$data, function(layer) layer$label))
  ))
}


test_that("a series too short for limits still gets a floating median", {
  result <- no_limits_chart("yes", plot_chart = FALSE)

  expect_identical(sum(!is.na(result$median)), 12L)
  expect_length(median_value_of(result), 1L)
})


test_that("the floating median is drawn on a series too short for limits", {
  expected <- median_value_of(no_limits_chart("yes", plot_chart = FALSE))

  drawn <- drawn_medians(no_limits_chart("yes"))

  # one layer holds the median line, at the value the table reports
  expect_true(any(vapply(
    drawn$values,
    function(y) identical(y, expected),
    logical(1L)
  )))

  # and one holds its label
  expect_true("Median" %in% drawn$labels)
})


test_that("no floating median is drawn where none was asked for", {
  expect_false("Median" %in% drawn_medians(no_limits_chart("no"))$labels)
})


test_that("show_limits = FALSE keeps the floating median", {
  # the argument asks for no control limits, not for no median, and the series
  # is long enough for both
  longer <- data.frame(
    x = 1:30,
    y = as.integer(c(rep(c(10, 11, 10, 12, 11), 3), rep(c(18, 19, 20), 5)))
  )

  drawn <- drawn_medians(suppressWarnings(autospc(longer,
    chart_type = "C", period_min = 21L, floating_median = "yes",
    show_limits = FALSE
  )))

  expect_true("Median" %in% drawn$labels)
})


test_that("auto draws no median on a series too short for limits", {
  # auto draws one only where a point in the window is part of a shift rule
  # break, and without limits there are no rule breaks to find
  expect_true(all(is.na(no_limits_chart("auto",
    plot_chart = FALSE
  )$median)))
})


test_that("auto is not confused by a point with no value in the window", {
  # the rule 2 column holds NA at a point with no value, which is not a break
  alternating <- data.frame(x = 1:40, y = as.integer(rep(c(48L, 52L), 20)))
  alternating$y[38] <- NA

  result <- expect_no_error(suppressWarnings(autospc(alternating,
    chart_type = "C", period_min = 21L, floating_median = "auto",
    plot_chart = FALSE
  )))

  # no point in the window is part of a shift rule break, so no median
  expect_true(all(is.na(result$median)))
})


test_that("the floating median label sits at the start of the median window", {
  table <- suppressWarnings(autospc(example_series_1,
    chart_type = "C", floating_median = "yes", plot_chart = FALSE
  ))

  window_starts_at <- min(table$x[!is.na(table$median)])

  drawn <- ggplot2::ggplot_build(suppressWarnings(autospc(example_series_1,
    chart_type = "C", floating_median = "yes"
  )))$data

  label <- do.call(rbind, lapply(drawn, function(layer) {
    if (is.null(layer$label)) {
      return(NULL)
    }

    return(layer[layer$label == "Median", c("x", "y")])
  }))

  expect_equal(label$x, window_starts_at)
})


test_that("the floating median is not drawn over the extension", {
  # extend_limits_to adds rows beyond the end of the data, and they hold no
  # observation for a median to be taken over
  set.seed(5)
  extended <- data.frame(x = 1:40, y = as.integer(stats::rpois(40, 50)))

  result <- suppressWarnings(autospc(extended,
    chart_type = "C", period_min = 21L, floating_median = "yes",
    extend_limits_to = 48, plot_chart = FALSE
  ))

  expect_identical(sum(result$limit_extension), 2L)
  expect_identical(sum(!is.na(result$median)), 12L)
  expect_false(any(!is.na(result$median) & result$limit_extension))
})
