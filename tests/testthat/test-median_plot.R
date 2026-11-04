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
    max_exclusions = 3L,
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
  # the last eight points are four of 10 then four of 30, so the median of the
  # last four is 30 and of the last eight is 20
  window_data <- data.frame(
    x = 1:30,
    y = as.integer(c(
      rep(c(10, 12, 11, 13, 9, 14), 3), 11, 12, 13, 12,
      10, 10, 10, 10, 30, 30, 30, 30
    ))
  )

  label_of <- function(n) {
    plot <- suppressWarnings(autospc(window_data,
      chart_type = "C",
      period_min = 21L,
      floating_median = "yes",
      floating_median_n = n
    ))

    return(median_drawing(plot)$label$label)
  }

  expect_identical(label_of(4L), "Median\n30")
  expect_identical(label_of(8L), "Median\n20")
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


has_median_label <- function(labels) {
  return(any(startsWith(as.character(stats::na.omit(labels)), "Median\n")))
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
  expect_true(has_median_label(drawn$labels))
})


test_that("no floating median is drawn where none was asked for", {
  expect_false(has_median_label(drawn_medians(no_limits_chart("no"))$labels))
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

  expect_true(has_median_label(drawn$labels))
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


test_that("the floating median label sits just after the end of the median window", {
  table <- suppressWarnings(autospc(example_series_1,
    chart_type = "C", floating_median = "yes", plot_chart = FALSE
  ))

  window_ends_at <- max(table$x[!is.na(table$median)])

  label <- median_drawing(suppressWarnings(autospc(example_series_1,
    chart_type = "C", floating_median = "yes"
  )))$label

  expect_identical(nrow(label), 1L)
  expect_gt(label$x, window_ends_at)
  expect_identical(label$hjust, 0)
  expect_equal(label$y, median_value_of(table))
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


test_that("an XmR pair too short for limits draws its floating median", {
  # a pair without limits is drawn as the location chart alone, so the median
  # is reached through create_timeseries_plot() rather than the pair
  short_pair <- data.frame(
    x = 1:14,
    y = as.integer(c(10, 11, 10, 12, 11, 18, 19, 20, 19, 17, 18, 19, 18, 20))
  )

  drawn <- drawn_medians(suppressWarnings(autospc(short_pair,
    chart_type = "XMR", period_min = 21L, floating_median = "yes"
  )))

  expect_true(has_median_label(drawn$labels))
})


test_that("a moving range panel without limits draws its floating median", {
  # a gap costs the moving range series two of its values, so a series long
  # enough for the X chart's limits can leave the moving range chart short of
  # them. That panel is then drawn by create_timeseries_plot().
  set.seed(3)
  with_a_gap <- data.frame(x = 1:22, y = as.integer(stats::rpois(22, 50)))
  with_a_gap$y[11] <- NA

  plot <- suppressWarnings(autospc(with_a_gap,
    chart_type = "XMR", period_min = 21L, floating_median = "yes"
  ))

  halves <- autospc_plot_charts(plot)

  expect_identical(
    vapply(halves, enough_data_for_limits, logical(1L)),
    c(location = TRUE, dispersion = FALSE)
  )

  # one median label on each panel
  expect_identical(sum(startsWith(panel_texts(plot), "Median\n")), 2L)
})


# The floating median's style and its label


# a P chart long enough for limits, whose last twelve points are 40, 45, 50,
# 55, 60 and 65 per cent twice, so its floating median is 52.5%
with_limits_data <- data.frame(
  x = 1:36,
  y = as.integer(c(rep(c(48, 52), 12), rep(c(40, 45, 50, 55, 60, 65), 2))),
  n = 100L
)


with_limits_plot <- function(...) {
  return(suppressWarnings(autospc(with_limits_data,
    chart_type = "P",
    floating_median = "yes",
    ...
  )))
}


# no_limits_data, above, is too short for limits, so it is drawn by
# create_timeseries_plot(); its last twelve points have a median of 18
no_limits_plot <- function(...) {
  return(no_limits_chart("yes", ...))
}


median_plots <- list(
  with_limits = with_limits_plot,
  no_limits = no_limits_plot
)


test_that("the floating median is drawn in firebrick3, longdash, at width 0.75 by default", {
  for (name in names(median_plots)) {
    drawing <- median_drawing(median_plots[[name]]())

    expect_length(drawing$line, 1L)

    line <- drawing$line[[1]]

    expect_identical(unique(line$colour), "firebrick3", info = name)
    expect_identical(unique(line$linetype), "longdash", info = name)
    expect_identical(unique(line$linewidth), 0.75, info = name)
    expect_identical(drawing$label$colour, "firebrick3", info = name)
  }
})


test_that("floating_median_style sets the median's colour, line type and width", {
  style <- list(colour = "navy", linetype = "dashed", linewidth = 1.2)

  for (name in names(median_plots)) {
    drawing <- median_drawing(
      median_plots[[name]](floating_median_style = style)
    )

    line <- drawing$line[[1]]

    expect_identical(unique(line$colour), "navy", info = name)
    expect_identical(unique(line$linetype), "dashed", info = name)
    expect_identical(unique(line$linewidth), 1.2, info = name)
    expect_identical(drawing$label$colour, "navy", info = name)
  }
})


test_that("elements not given in floating_median_style take their defaults", {
  line <- median_drawing(
    with_limits_plot(floating_median_style = list(colour = "navy"))
  )$line[[1]]

  expect_identical(unique(line$colour), "navy")
  expect_identical(unique(line$linetype), "longdash")
  expect_identical(unique(line$linewidth), 0.75)
})


test_that("the floating median's width follows line_width_sf", {
  for (name in names(median_plots)) {
    line <- median_drawing(median_plots[[name]](line_width_sf = 2))$line[[1]]

    expect_identical(unique(line$linewidth), 1.5, info = name)
  }
})


test_that("the floating median has no legend entry", {
  for (name in names(median_plots)) {
    layer <- median_drawing(median_plots[[name]]())$layer[[1]]

    expect_false(layer$show.legend, info = name)
  }
})


test_that("the floating median label gives the median's value", {
  expect_identical(
    median_drawing(with_limits_plot())$label$label,
    "Median\n52.5%"
  )

  expect_identical(
    median_drawing(no_limits_plot())$label$label,
    "Median\n18"
  )
})


test_that("a count median halfway between two whole numbers keeps its half", {
  # C and C' labels are whole numbers, but the median of an even number of
  # counts can end in .5. Its last twelve points sort to 15, 16, 17, 17, 18,
  # 18, 19, 19, 20, 20, 21 and 22, so the median is 18.5
  halfway_data <- data.frame(
    x = 1:14,
    y = as.integer(c(10, 11, 17, 18, 16, 19, 20, 18, 21, 19, 22, 17, 20, 15))
  )

  label <- median_drawing(suppressWarnings(autospc(halfway_data,
    chart_type = "C",
    period_min = 21L,
    floating_median = "yes"
  )))$label

  expect_identical(label$label, "Median\n18.5")
})


test_that("the floating median label sits just after its line, level with it", {
  last_x <- c(with_limits = 36L, no_limits = 14L)
  value <- c(with_limits = 52.5, no_limits = 18)

  for (name in names(median_plots)) {
    label <- median_drawing(median_plots[[name]]())$label

    # one label, so it was not dropped for lying outside the axis
    expect_identical(nrow(label), 1L, info = name)
    expect_gt(label$x, last_x[[name]])
    expect_identical(label$hjust, 0, info = name)
    expect_equal(label$y, value[[name]], info = name)
  }
})


test_that("the floating median label is drawn at annotation_size", {
  for (name in names(median_plots)) {
    label <- median_drawing(median_plots[[name]](annotation_size = 5))$label

    expect_identical(label$size, 5, info = name)
  }
})


test_that("the floating median label sits at the end of the median, not of the extension", {
  set.seed(5)
  extended <- data.frame(x = 1:40, y = as.integer(stats::rpois(40, 50)))

  label <- median_drawing(suppressWarnings(autospc(extended,
    chart_type = "C", period_min = 21L, floating_median = "yes",
    extend_limits_to = 48
  )))$label

  expect_identical(nrow(label), 1L)
  expect_gt(label$x, 40)
  expect_lt(label$x, 48)
})


test_that("the plot records floating_median_style with every element filled in", {
  plot <- with_limits_plot(floating_median_style = list(colour = "navy"))

  expect_identical(
    autospc_plot_visualisation_params(plot,
      parameter = "floating_median_style"
    ),
    list(colour = "navy", linetype = "longdash", linewidth = 0.75)
  )
})


test_that("a faceted plot draws the median label only in the facet with the median", {
  # facet_stages() takes the floating median over the last points of the whole
  # series, which are in the last facet
  plot <- suppressWarnings(facet_stages(example_series_1,
    split_at = c(60L, 100L),
    chart_type = "C",
    floating_median = "yes"
  ))

  label <- median_drawing(plot)$label

  expect_identical(nrow(label), 1L)
  expect_identical(as.integer(label$PANEL), 3L)
})
