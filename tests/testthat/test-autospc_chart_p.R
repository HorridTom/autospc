# P data can arrive in three shapes: pre-aggregated counts at subgroup level,
# counts below subgroup level, or individual binary observations. Note that for
# the binary form the n column does not exist until aggregation creates it, so
# the chart's n field names a column that is not yet present.

pre_agg_data <- data.frame(
  x = 1:5,
  y = c(3, 4, 2, 5, 3),
  n = rep(20L, 5),
  site = "a"
)

sub_level_data <- data.frame(
  x = rep(1:3, each = 2),
  y = c(1, 2, 3, 4, 5, 6),
  n = rep(10L, 6)
)

binary_data <- data.frame(
  x = rep(1:5, each = 4),
  y = rep(c(TRUE, FALSE, TRUE, TRUE), 5)
)

# one observation per subgroup, so every proportion is 0% or 100%
binary_degenerate_data <- data.frame(
  x = 1:5,
  y = c(TRUE, FALSE, TRUE, TRUE, FALSE)
)

# a calculation period as the algorithm builds it for P charts: y holds
# percentages and y_numerator holds the counts, so a method reading the wrong
# column produces a different answer rather than an error
proportion_period_data <- data.frame(
  x = 1:5,
  y_numerator = c(3, 4, 2, 5, 3),
  n = rep(20, 5),
  y = c(3, 4, 2, 5, 3) * 100 / rep(20, 5)
)

chart_p <- function(data, ...) {
  autospc_chart_p(data = data, x = "x", y = "y", n = "n", ...)
}


test_that("autospc_chart_p returns an object of the expected class", {
  expect_identical(
    class(chart_p(pre_agg_data)),
    c("autospc_chart_p", "autospc_chart")
  )
})


test_that("autospc_chart_p carries the common elements plus its own", {
  chart <- chart_p(pre_agg_data)

  expect_true(all(autospc_chart_elements() %in% names(chart)))
  expect_true(all(autospc_chart_p_elements() %in% names(chart)))
  expect_length(
    chart,
    length(autospc_chart_elements()) +
      length(autospc_chart_p_elements())
  )
})


test_that("n is required and is appended after the common elements", {
  expect_error(
    autospc_chart_p(data = pre_agg_data, x = "x", y = "y"),
    "argument \"n\" is missing"
  )

  expect_identical(
    names(chart_p(pre_agg_data))[length(chart_p(pre_agg_data))],
    "n"
  )
})


test_that("data_original is populated correctly", {
  expect_identical(chart_p(pre_agg_data)$data_original, pre_agg_data)
})


test_that("an unrecognised argument name is rejected", {
  expect_error(chart_p(pre_agg_data, period_mn = 30L), "unused argument")
})


test_that("a missing element is caught on the constructor path", {
  expect_error(
    validate_autospc_chart_p(new_autospc_chart_p(list(data = pre_agg_data))),
    "element\\(s\\) not present"
  )
})


test_that("a missing n is caught on the constructor path", {
  no_n <- new_autospc_chart_p(autospc_chart_list(
    data = pre_agg_data,
    x = "x",
    y = "y"
  ))

  expect_error(
    validate_autospc_chart_p(no_n),
    "autospc_chart_p object - element\\(s\\) not present: n"
  )
})


test_that("validate_autospc_chart_p rejects a bare autospc_chart object", {
  expect_error(
    validate_autospc_chart_p(new_autospc_chart(autospc_chart_list(
      data = pre_agg_data,
      x = "x",
      y = "y"
    ))),
    "Not an autospc_chart_p object"
  )
})


test_that("validate_autospc_chart_p rejects a sibling subclass object", {
  expect_error(
    validate_autospc_chart_p(autospc_chart_c(
      data = pre_agg_data,
      x = "x",
      y = "y"
    )),
    "Not an autospc_chart_p object"
  )
})


test_that("validate_autospc_chart_p returns a valid object unchanged", {
  chart <- chart_p(pre_agg_data)

  expect_identical(validate_autospc_chart_p(chart), chart)
})


test_that("y_axis_title returns the P chart axis title", {
  expect_identical(y_axis_title(chart_p(pre_agg_data)), "Percentage")
})


test_that("chart_type_label returns the P chart label", {
  expect_identical(chart_type_label(chart_p(pre_agg_data)), "P")
})


test_that("aggregate_data sums y and n over x", {
  chart <- aggregate_data(chart_p(sub_level_data))

  expect_identical(chart$data$x, 1:3)
  expect_identical(chart$data$y, c(3, 7, 11))
  expect_identical(chart$data$n, c(20L, 20L, 20L))
})


test_that("aggregate_data counts individual binary observations", {
  chart <- aggregate_data(chart_p(binary_data))

  expect_identical(chart$data$y, rep(3L, 5))
  expect_identical(chart$data$n, rep(4L, 5))
})


test_that("aggregate_data materialises n when there is one observation per subgroup", {
  # the case that fails end to end today: with no x value repeated, the
  # pre-refactor function returns early and n is never created, so the run dies
  # at `y = y * 100 / n`
  chart <- aggregate_data(chart_p(binary_degenerate_data))

  expect_identical(chart$data$n, rep(1L, 5))
  expect_identical(chart$data$y, c(1L, 0L, 1L, 1L, 0L))
})


test_that("aggregate_data leaves fully pre-aggregated data unchanged", {
  chart <- aggregate_data(chart_p(pre_agg_data))

  expect_identical(chart$data$x, pre_agg_data$x)
  expect_identical(chart$data$y, pre_agg_data$y)
  expect_identical(chart$data$n, pre_agg_data$n)
})


test_that("aggregate_data returns the same columns by either route", {
  expect_identical(
    names(aggregate_data(chart_p(pre_agg_data))$data),
    names(aggregate_data(chart_p(sub_level_data))$data)
  )
})


test_that("aggregate_data returns a chart and leaves data_original untouched", {
  chart <- aggregate_data(chart_p(binary_data))

  expect_identical(class(chart), c("autospc_chart_p", "autospc_chart"))
  expect_identical(chart$data_original, binary_data)
})


test_that("calculate_limits matches get_p_limits", {
  expect_identical(
    calculate_limits(chart_p(pre_agg_data),
      period = proportion_period_data,
      exclusion_points = NULL
    ),
    get_p_limits(
      y = proportion_period_data$y_numerator,
      n = proportion_period_data$n,
      exclusion_points = NULL,
      multiply = 100
    )
  )
})


test_that("calculate_limits uses y_numerator, not the percentage column y", {
  # the y column holds percentages; using it would give a centre line of 85
  # rather than the true 17%, with no error raised
  limits <- calculate_limits(chart_p(pre_agg_data),
    period = proportion_period_data,
    exclusion_points = NULL
  )

  expect_equal(
    limits$cl[1],
    sum(proportion_period_data$y_numerator) /
      sum(proportion_period_data$n) * 100
  )
})


test_that("calculate_limits passes exclusion_points through", {
  expect_identical(
    calculate_limits(chart_p(pre_agg_data),
      period = proportion_period_data,
      exclusion_points = 4L
    ),
    get_p_limits(
      y = proportion_period_data$y_numerator,
      n = proportion_period_data$n,
      exclusion_points = 4L,
      multiply = 100
    )
  )

  # excluding the highest point must lower the centre line, otherwise the
  # comparison above would pass even if the argument were ignored
  with_excl <- calculate_limits(chart_p(pre_agg_data),
    period = proportion_period_data,
    exclusion_points = 4L
  )
  without <- calculate_limits(chart_p(pre_agg_data),
    period = proportion_period_data,
    exclusion_points = NULL
  )

  expect_lt(with_excl$cl[1], without$cl[1])
})


test_that("limits_table_columns keeps n and y_numerator", {
  # y holds percentages for this class, so the counts and denominators the
  # limits were calculated from have to survive into the limits table
  expect_identical(
    limits_table_columns(chart_p(pre_agg_data)),
    c("n", "y_numerator")
  )
})


test_that("extend_display_limits recomputes the limits at each denominator", {
  # the limits of a P chart depend on n, so they cannot be carried forward.
  # The width of the last calculated period is held as a constant and reapplied
  # at each display point's own denominator - so a point with a larger n gets
  # narrower limits.
  table <- data.frame(
    x = 1:5,
    y = c(15, 15, 15, 15, 15),
    n = c(100, 100, 100, 25, 400),
    ucl = c(rep(25, 3), rep(NA_real_, 2)),
    lcl = c(rep(5, 3), rep(NA_real_, 2)),
    cl = c(rep(15, 3), rep(NA_real_, 2)),
    period_type = c(
      rep("calculation", 3),
      rep(NA_character_, 2)
    )
  )

  extended <- extend_display_limits(chart_p(pre_agg_data),
    limits_table = table,
    counter = 4
  )

  # constant = (25 - 15) * sqrt(100) = 100, so the half-width is 100/sqrt(n)
  expect_equal(extended$ucl[4], 15 + 100 / sqrt(25))
  expect_equal(extended$lcl[5], 15 - 100 / sqrt(400))

  # the centre line is carried forward, and the calculated rows are untouched
  expect_identical(extended$cl, rep(15, 5))
  expect_identical(
    extended$period_type,
    c(rep("calculation", 3), rep("display", 2))
  )
})


test_that("extend_display_limits clamps the recomputed limits to 0 and 100", {
  # percentages, so a limit outside 0-100 is meaningless
  table <- data.frame(
    x = 1:5,
    y = c(50, 50, 50, 50, 50),
    n = c(100, 100, 100, 1, 1),
    ucl = c(rep(80, 3), rep(NA_real_, 2)),
    lcl = c(rep(20, 3), rep(NA_real_, 2)),
    cl = c(rep(50, 3), rep(NA_real_, 2)),
    period_type = c(
      rep("calculation", 3),
      rep(NA_character_, 2)
    )
  )

  extended <- extend_display_limits(chart_p(pre_agg_data),
    limits_table = table,
    counter = 4
  )

  expect_true(all(extended$ucl[4:5] <= 100))
  expect_true(all(extended$lcl[4:5] >= 0))
})


test_that("extrapolate_limits recalculates from the final period", {
  # the limits of a P chart vary with n, so there is no single set to carry
  # forward. They are recalculated from the final calculation period, giving
  # one set of values for the whole extension.
  final_period <- data.frame(
    y = c(10, 15, 20, 12, 18), # percentages
    n = c(100, 100, 200, 200, 100),
    excluded = rep(FALSE, 5),
    cl = rep(99, 5), # deliberately wrong,
    lcl = rep(99, 5), # so a method that
    ucl = rep(99, 5)
  ) # echoes them fails

  limits <- extrapolate_limits(chart_p(pre_agg_data), period = final_period)

  expect_named(limits, c("cl", "ucl", "lcl"), ignore.order = TRUE)
  expect_length(limits$cl, 1L)

  # the centre line is the pooled proportion of the period, not anything taken
  # from the cl column
  pooled <- sum(final_period$y / 100 * final_period$n) /
    sum(final_period$n) * 100

  expect_equal(limits$cl, pooled)
  expect_gt(limits$ucl, limits$cl)
  expect_lt(limits$lcl, limits$cl)
})


test_that("extrapolate_limits leaves out the excluded points", {
  base_period <- data.frame(
    y = c(15, 15, 15, 60, 15),
    n = rep(100, 5),
    excluded = rep(FALSE, 5),
    cl = rep(15, 5),
    lcl = rep(5, 5),
    ucl = rep(25, 5)
  )

  excluded_period <- base_period
  excluded_period$excluded <- c(FALSE, FALSE, FALSE, TRUE, FALSE)

  with_spike <- extrapolate_limits(chart_p(pre_agg_data), period = base_period)
  without_spike <- extrapolate_limits(chart_p(pre_agg_data),
    period = excluded_period
  )

  expect_lt(without_spike$cl, with_spike$cl)
})


test_that("prepare_data turns counts into percentages and keeps the count", {
  counts <- data.frame(
    x = 1:4,
    y = c(10, 20, 30, 40),
    n = c(100, 200, 100, 200)
  )

  prepared <- prepare_data(
    autospc_chart_p(data = counts, x = "x", y = "y", n = "n")
  )

  expect_identical(prepared$data$y, c(10, 10, 30, 20))
  expect_identical(prepared$data$y_numerator, counts$y)
  expect_identical(prepared$data$n, counts$n)

  expect_identical(prepared$data_original, counts)
})


test_that("prepare_data gives NA for a zero or missing denominator", {
  # rather than NaN or Inf, which would propagate into the limits
  counts <- data.frame(
    x = 1:3,
    y = c(10, 10, 10),
    n = c(100, 0, NA_real_)
  )

  prepared <- prepare_data(
    autospc_chart_p(data = counts, x = "x", y = "y", n = "n")
  )

  expect_identical(prepared$data$y, c(10, NA_real_, NA_real_))
})


test_that("P chart labels sit closer to the upper limit than the default", {
  # the axis is a percentage scale, so the superclass's tenth is a wide gap
  expect_identical(upper_annotation_sf_default(chart_p(pre_agg_data)), 1.04)
})


test_that("P chart labels are rounded to one decimal place", {
  expect_identical(label_accuracy(chart_p(pre_agg_data), ylimhigh = 110), 0.1)
})


test_that("a P chart label is a percentage", {
  expect_identical(
    centre_line_label(chart_p(pre_agg_data),
      cl = 43.28,
      ylimhigh = 110
    ),
    "43.3%"
  )
})
