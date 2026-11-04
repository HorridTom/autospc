# display_window: the plot is zoomed to a range of the horizontal axis, and
# the annotations are placed for that range. The analysis is unchanged.


p_window <- as.Date(c("2021-01-01", "2024-07-01"))


p_chart <- function(...) {
  return(suppressWarnings(autospc(ed_attendances_monthly,
    x = month_start,
    y = within_4h,
    n = att_all,
    chart_type = "P'",
    ...
  )))
}


c_chart <- function(...) {
  return(suppressWarnings(autospc(example_series_1,
    chart_type = "C",
    ...
  )))
}


# The centre line labels a plot carries, with the x each is placed at
centre_line_labels <- function(plot) {
  return(plot$data %>%
    dplyr::filter(
      plotted_line == "cl",
      !is.na(cl_label),
      cl_label != ""
    ) %>%
    dplyr::select(x, cl_label, annotation_level))
}


coordinate_limits <- function(plot) {
  return(plot$coordinates$limits)
}


test_that("display_window zooms the plot to the window", {
  plot <- p_chart(display_window = p_window)

  expect_equal(as.numeric(coordinate_limits(plot)$x), as.numeric(p_window))
})


test_that("display_window keeps the vertical axis of the whole series", {
  expect_identical(
    coordinate_limits(p_chart(display_window = p_window))$y,
    coordinate_limits(p_chart())$y
  )
})


test_that("display_window drops no points from the plot", {
  # the window zooms, so points outside it are kept and moved out of view
  # rather than removed from the scale
  points_drawn <- function(plot) {
    built <- ggplot2::ggplot_build(plot)$data

    return(max(vapply(built, nrow, integer(1L))))
  }

  expect_identical(
    points_drawn(p_chart(display_window = p_window)),
    points_drawn(p_chart())
  )
})


test_that("a period that starts before the window is labelled at the window's first point", {
  # the 87.6% period runs from 2019-08 to 2021-04, so only its end is in view
  labels <- centre_line_labels(p_chart(display_window = p_window))

  expect_identical(labels$cl_label, c("87.6%", "70.9%"))
  expect_identical(labels$x, as.Date(c("2021-01-01", "2021-05-01")))
})


test_that("each period overlapping the window is labelled once, and none outside it", {
  # the periods starting at 26, 47 and 74 overlap 30 to 90. The display period
  # from 68 continues the one from 47, so it has no label of its own
  labels <- centre_line_labels(c_chart(display_window = c(30, 90)))

  expect_identical(labels$cl_label, c("12", "9", "11"))
  expect_equal(labels$x, c(30, 47, 74))
})


test_that("align_labels places the labels by the highest upper limit in the window", {
  # 95.8 in the window, against 97.8 over the whole series
  plot <- p_chart(display_window = p_window, align_labels = TRUE)

  in_window <- plot$data %>%
    dplyr::filter(
      plotted_line == "ucl",
      x >= p_window[1],
      x <= p_window[2]
    )

  expect_equal(
    unique(centre_line_labels(plot)$annotation_level),
    max(in_window$value, na.rm = TRUE) * 1.04
  )
})


test_that("basic labels are nudged by a share of the window, not of the series", {
  window <- c(30, 90)

  plot <- c_chart(display_window = window, basic_annotations = TRUE)

  anchors <- centre_line_labels(plot)

  drawn <- ggplot2::ggplot_build(plot)$data

  label_layer <- Filter(
    function(layer) "label" %in% names(layer) && "12" %in% layer$label,
    drawn
  )[[1]]

  drawn_at <- label_layer$x[label_layer$label == "12"]
  anchored_at <- anchors$x[anchors$cl_label == "12"]

  expect_equal(drawn_at - anchored_at, diff(window) / 25)
})


test_that("the floating median is drawn when the window reaches the end of the data", {
  plot <- p_chart(
    display_window = as.Date(c("2021-01-01", NA)),
    floating_median = "yes"
  )

  label <- median_drawing(plot)$label

  expect_identical(nrow(label), 1L)

  # and the axis reaches far enough to show it
  expect_gt(as.numeric(coordinate_limits(plot)$x[2]), as.numeric(label$x))
})


test_that("a window ending at the last point makes room for the median label", {
  # 2024-06-01 is the last point; the label sits after it
  plot <- p_chart(
    display_window = as.Date(c("2021-01-01", "2024-06-01")),
    floating_median = "yes"
  )

  label <- median_drawing(plot)$label

  expect_identical(nrow(label), 1L)
  expect_gt(as.numeric(coordinate_limits(plot)$x[2]), as.numeric(label$x))
})


test_that("no floating median is drawn when the window ends before the end of the data", {
  # the median is taken over 2023-07 to 2024-06; one window ends before it,
  # the other part-way through it
  for (end in c("2023-06-01", "2024-01-01")) {
    drawing <- median_drawing(p_chart(
      display_window = as.Date(c("2021-01-01", end)),
      floating_median = "yes"
    ))

    expect_length(drawing$line, 0L)
    expect_identical(NROW(drawing$label), 0L)
  }
})


test_that("a window end beyond the data extends the axis", {
  window <- as.Date(c("2021-01-01", "2025-06-01"))

  expect_equal(
    as.numeric(coordinate_limits(p_chart(display_window = window))$x),
    as.numeric(window)
  )
})


test_that("NA at either end of display_window leaves that end where the chart puts it", {
  built <- ggplot2::ggplot_build(p_chart())
  scale_limits <- built$layout$panel_scales_x[[1]]$limits

  from_2021 <- coordinate_limits(p_chart(
    display_window = as.Date(c("2021-01-01", NA))
  ))$x

  to_2021 <- coordinate_limits(p_chart(
    display_window = as.Date(c(NA, "2021-01-01"))
  ))$x

  window_start <- as.numeric(p_window[1])

  expect_equal(as.numeric(from_2021), c(window_start, scale_limits[2]))
  expect_equal(as.numeric(to_2021), c(scale_limits[1], window_start))
})


test_that("override_y_lim with a display_window is checked against the points in the window", {
  # the series reaches 97.3 before the window and 92.6 within it
  plot <- expect_no_error(p_chart(
    display_window = p_window,
    override_y_lim = c(55, 95)
  ))

  expect_equal(coordinate_limits(plot)$y, c(55, 95))

  # a range that hides a point in the window is still refused
  expect_error(
    p_chart(display_window = p_window, override_y_lim = c(65, 95)),
    "override_y_lim"
  )
})


test_that("display_window does not change the analysis", {
  expect_identical(
    p_chart(display_window = p_window, plot_chart = FALSE),
    p_chart(plot_chart = FALSE)
  )
})


test_that("facet_stages zooms every facet to the window", {
  plot <- suppressWarnings(facet_stages(example_series_1,
    split_at = c(60L, 100L),
    chart_type = "C",
    display_window = c(30, 90)
  ))

  expect_equal(as.numeric(coordinate_limits(plot)$x), c(30, 90))
})
