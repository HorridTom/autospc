# The class methods are tested in the class files. These check the default
# reaches the chart, which the method tests cannot: a default that is correct
# and never consulted looks the same to them.
#
# The assertions compare runs rather than reading annotation_level directly, so
# they do not depend on how add_annotation_data() turns a scale factor into a
# position. They are always within one chart type, because two chart types have
# different limits and so different label positions at the same scale factor.
#
# The second assertion in each is also the "a value the caller passes wins over
# the method" case: passing the other class's factor changes the result.

annotation_data <- data.frame(
  x = 1:30,
  y = c(
    10, 12, 11, 13, 9, 14, 10, 11, 12, 13,
    10, 12, 11, 13, 9, 14, 10, 11, 12, 13,
    10, 12, 11, 13, 9, 14, 10, 11, 12, 13
  ),
  n = rep(100L, 30)
)

run_annotation <- function(chart_type, ...) {
  suppressWarnings(
    autospc(annotation_data,
      chart_type = chart_type,
      period_min = 21L,
      plot_chart = FALSE,
      ...
    )$annotation_level
  )
}


test_that("a P chart defaults to 1.04", {
  expect_equal(
    run_annotation("P"),
    run_annotation("P", upper_annotation_sf = 1.04)
  )

  expect_false(isTRUE(all.equal(
    run_annotation("P"),
    run_annotation("P",
      upper_annotation_sf = 1.1
    )
  )))
})


test_that("a C chart defaults to 1.1", {
  expect_equal(
    run_annotation("C"),
    run_annotation("C", upper_annotation_sf = 1.1)
  )

  expect_false(isTRUE(all.equal(
    run_annotation("C"),
    run_annotation("C",
      upper_annotation_sf = 1.04
    )
  )))
})


# the label the class formats is the label in the output


label_data <- data.frame(
  x = 1:30,
  y = rep(c(
    12000L, 12500L, 11800L,
    13000L, 12200L, 12700L
  ), 5L)
)

label_p_data <- data.frame(
  x = 1:30,
  y = rep(c(40L, 45L, 38L, 50L, 42L, 47L), 5L),
  n = rep(100L, 30)
)

drawn_labels <- function(data, chart_type, ...) {
  result <- autospc(data,
    chart_type = chart_type,
    period_min = 21L,
    plot_chart = FALSE,
    ...
  )
  unique(result$cl_label[result$cl_label != ""])
}


test_that("a P chart's centre line label carries a per cent sign", {
  expect_match(drawn_labels(label_p_data, "P"), "%$")
})


test_that("a C chart's centre line label separates thousands", {
  expect_match(drawn_labels(label_data, "C"), ",")
})


# a series that steps down, so the centre line falls and flip_labels applies
stepped_data <- data.frame(
  x = 1:70,
  y = c(
    rep(c(120L, 140L, 110L, 150L, 130L), 7L),
    rep(c(20L, 24L, 18L, 26L, 22L), 7L)
  )
)

flipped_levels <- function(chart_type) {
  result <- autospc(stepped_data,
    chart_type = chart_type,
    period_min = 21L,
    flip_labels = TRUE,
    plot_chart = FALSE
  )
  labelled <- !is.na(result$cl_label) & result$cl_label != ""
  result$annotation_level[labelled] < result$ucl[labelled]
}


test_that("flip_labels puts a label below the line when the centre line falls", {
  expect_true(any(flipped_levels("C")))
})


test_that("moving range labels stay above it anyway", {
  # the same fall, the same flip_labels, and no label below the line
  expect_false(any(flipped_levels("MR")))
})
