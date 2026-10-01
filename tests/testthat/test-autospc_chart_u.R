# U data is a count y and the area of opportunity n it was made over, either
# one row per subgroup or several rows per subgroup to be summed. The area need
# not be a whole number.

pre_agg_data <- data.frame(
  x = 1:5,
  y = c(3, 4, 2, 5, 3),
  n = c(1.5, 2, 2, 2.5, 1)
)

sub_level_data <- data.frame(
  x = rep(1:3, each = 2),
  y = c(1, 2, 3, 4, 5, 6),
  n = c(0.5, 1, 1, 1, 2, 0.5)
)

# a calculation period as the algorithm builds it for U charts: series holds
# rates and y holds the counts, so a method reading the wrong column produces a
# different answer rather than an error
rate_period_data <- data.frame(
  x = 1:5,
  y = c(3, 4, 2, 5, 3),
  n = c(1.5, 2, 2, 2.5, 1),
  series = c(3, 4, 2, 5, 3) / c(1.5, 2, 2, 2.5, 1)
)

chart_u <- function(data, ...) {
  autospc_chart_u(data = data, x = "x", y = "y", n = "n", ...)
}


# the object


test_that("autospc_chart_u returns an object of the expected class", {
  expect_identical(
    class(chart_u(pre_agg_data)),
    c("autospc_chart_u", "autospc_chart")
  )
})


test_that("autospc_chart_u carries the common elements plus its own", {
  chart <- chart_u(pre_agg_data)

  expect_true(all(autospc_chart_elements() %in% names(chart)))
  expect_true(all(autospc_chart_u_elements() %in% names(chart)))
  expect_length(
    chart,
    length(autospc_chart_elements()) +
      length(autospc_chart_u_elements())
  )
})


test_that("n is required and is appended after the common elements", {
  expect_error(
    autospc_chart_u(data = pre_agg_data, x = "x", y = "y"),
    "argument \"n\" is missing"
  )

  expect_identical(
    names(chart_u(pre_agg_data))[length(chart_u(pre_agg_data))],
    "n"
  )
})


test_that("data_original is populated correctly", {
  expect_identical(chart_u(pre_agg_data)$data_original, pre_agg_data)
})


test_that("an unrecognised argument name is rejected", {
  expect_error(chart_u(pre_agg_data, period_mn = 30L), "unused argument")
})


test_that("a missing n element is caught on the constructor path", {
  no_n <- new_autospc_chart_u(assemble_chart_list(
    data = pre_agg_data,
    x = "x",
    y = "y"
  ))

  expect_error(
    validate_autospc_chart_u(no_n),
    "autospc_chart_u object - element\\(s\\) not present: n"
  )
})


test_that("validate_autospc_chart_u rejects a bare autospc_chart object", {
  expect_error(
    validate_autospc_chart_u(new_autospc_chart(assemble_chart_list(
      data = pre_agg_data,
      x = "x",
      y = "y"
    ))),
    "Not an autospc_chart_u object"
  )
})


test_that("validate_autospc_chart_u rejects a sibling subclass object", {
  expect_error(
    validate_autospc_chart_u(autospc_chart_p(
      data = data.frame(x = 1:5, y = 1:5, n = rep(10L, 5)),
      x = "x",
      y = "y",
      n = "n"
    )),
    "Not an autospc_chart_u object"
  )
})


test_that("validate_autospc_chart_u returns a valid object unchanged", {
  chart <- chart_u(pre_agg_data)

  expect_identical(validate_autospc_chart_u(chart), chart)
})


# the columns


test_that("a U chart needs an n column", {
  expect_error(
    chart_u(data.frame(x = 1:5, y = c(3, 4, 2, 5, 3))),
    paste(
      "n not specified. For U and U' charts, n must be specified: it is",
      "the area of opportunity each count in y was made over."
    ),
    fixed = TRUE
  )
})


test_that("an area of opportunity that is not a whole number is kept", {
  expect_no_warning(chart <- chart_u(pre_agg_data))

  expect_identical(chart$data$n, pre_agg_data$n)
})


test_that("a count that is not a whole number is rounded, with a warning", {
  fractional <- pre_agg_data
  fractional$y[2] <- 4.4

  expect_warning(
    chart <- chart_u(fractional),
    "U and U' charts require y to be a count",
    fixed = TRUE
  )

  expect_identical(chart$data$y, c(3, 4, 2, 5, 3))
})


test_that("a negative count is refused, naming the row", {
  negative <- pre_agg_data
  negative$y[2] <- -1

  expect_error(
    chart_u(negative),
    "For U and U' charts, y cannot be negative. Negative: row 2 (y = -1).",
    fixed = TRUE
  )
})


test_that("a negative area of opportunity is refused, naming the row", {
  negative <- pre_agg_data
  negative$n[4] <- -2.5

  expect_error(
    chart_u(negative),
    "For U and U' charts, n cannot be negative. Negative: row 4 (n = -2.5).",
    fixed = TRUE
  )
})


test_that("a count over an area of opportunity of zero is refused", {
  no_opportunity <- pre_agg_data
  no_opportunity$n[3] <- 0

  expect_error(
    chart_u(no_opportunity),
    paste(
      "For U and U' charts, y must be 0 where n is 0.",
      "Not 0: row 3 (y = 2, n = 0)."
    ),
    fixed = TRUE
  )
})


test_that("a count of zero over an area of zero is accepted", {
  no_opportunity <- pre_agg_data
  no_opportunity$y[3] <- 0
  no_opportunity$n[3] <- 0

  expect_no_error(chart_u(no_opportunity))
})


# analysis methods


test_that("aggregate_data sums y and n over x", {
  chart <- aggregate_data(chart_u(sub_level_data))

  expect_identical(chart$data$x, 1:3)
  expect_identical(chart$data$y, c(3, 7, 11))
  expect_identical(chart$data$n, c(1.5, 2, 2.5))
})


test_that("aggregate_data leaves data already one row per subgroup unchanged", {
  chart <- aggregate_data(chart_u(pre_agg_data))

  expect_identical(chart$data$x, pre_agg_data$x)
  expect_identical(chart$data$y, pre_agg_data$y)
  expect_identical(chart$data$n, pre_agg_data$n)
})


test_that("prepare_data turns counts into rates and keeps the count", {
  prepared <- prepare_data(chart_u(pre_agg_data))

  expect_identical(prepared$data$series, pre_agg_data$y / pre_agg_data$n)
  expect_identical(prepared$data$y, pre_agg_data$y)
  expect_identical(prepared$data$n, pre_agg_data$n)
})


test_that("prepare_data gives NA for a zero or missing area of opportunity", {
  counts <- data.frame(
    x = 1:3,
    y = c(10, 0, 10),
    n = c(2, 0, NA_real_)
  )

  prepared <- prepare_data(chart_u(counts))

  expect_identical(prepared$data$series, c(5, NA_real_, NA_real_))
})


test_that("calculate_limits matches get_u_statistics", {
  expect_identical(
    calculate_limits(chart_u(pre_agg_data),
      period = rate_period_data,
      exclusion_points = 4L
    ),
    get_u_statistics(
      y = rate_period_data$y,
      n = rate_period_data$n,
      exclusion_points = 4L
    )
  )
})


test_that("calculate_limits uses y and n, not the rate column series", {
  limits <- calculate_limits(chart_u(pre_agg_data),
    period = rate_period_data,
    exclusion_points = NULL
  )

  expect_equal(limits$cl[1], 17 / 9)
})


test_that("calculate_limits passes exclusion_points through", {
  with_excl <- calculate_limits(chart_u(pre_agg_data),
    period = rate_period_data,
    exclusion_points = 5L
  )

  # leaving out row 5, the highest rate, leaves 14 over 8
  expect_equal(with_excl$cl[1], 14 / 8)
})


test_that("the standard error is the estimate over the square root of n", {
  expect_equal(
    standard_error_at(chart_u(pre_agg_data),
      sd_estimate = 2,
      rows = pre_agg_data
    ),
    2 / sqrt(pre_agg_data$n)
  )
})


test_that("a rate is bounded below at zero and not above", {
  expect_identical(
    limit_bounds(chart_u(pre_agg_data)),
    list(low = 0, high = Inf)
  )
})


test_that("limits_table_columns keeps y and n", {
  expect_identical(
    limits_table_columns(chart_u(pre_agg_data)),
    c("y", "n")
  )
})


test_that("display limits are recalculated at each area of opportunity", {
  # the period carries a standard deviation estimate of 2, so the limits sit
  # 3 * 2 / sqrt(n) either side of the centre line
  table <- data.frame(
    x = 1:5,
    y = c(4, 4, 4, 1, 16),
    n = c(1, 1, 1, 0.25, 4),
    ucl = c(rep(10, 3), rep(NA_real_, 2)),
    lcl = c(rep(0, 3), rep(NA_real_, 2)),
    cl = c(rep(4, 3), rep(NA_real_, 2)),
    sd_estimate = c(rep(2, 3), rep(NA_real_, 2)),
    period_type = c(
      rep("calculation", 3),
      rep(NA_character_, 2)
    )
  )

  extended <- form_display_limits(
    limits_table = table,
    counter = 4,
    chart = chart_u(pre_agg_data)
  )

  expect_equal(extended$ucl[4], 4 + 3 * 2 / sqrt(0.25))
  expect_equal(extended$ucl[5], 4 + 3 * 2 / sqrt(4))
  expect_equal(extended$lcl[5], 4 - 3 * 2 / sqrt(4))

  # below zero at the smaller area, so constrained to zero
  expect_identical(extended$lcl[4], 0)

  expect_identical(extended$cl, rep(4, 5))
})


test_that("the extension's limits sit at the final period's mean area", {
  # the mean leaves out the row with no observation and the excluded point
  final_period <- data.frame(
    series = c(4, 4, 4, 4, NA, 20),
    n = c(1, 2, 2, 3, 100, 0.1),
    excluded = c(FALSE, FALSE, FALSE, FALSE, NA, TRUE),
    cl = rep(4, 6),
    sd_estimate = rep(2, 6)
  )

  limits <- extension_limits(chart_u(pre_agg_data), final_period = final_period)

  expect_identical(limits$cl, 4)
  expect_equal(limits$ucl, 4 + 3 * 2 / sqrt(2))

  # 4 - 3 * 2 / sqrt(2) is below zero, so the lower limit is constrained to 0
  expect_identical(limits$lcl, 0)
})


test_that("the limits are Provost and Murray's, through autospc()", {
  # the rates alternate around 2 per unit, with the area of opportunity varying
  d <- data.frame(
    x = 1:24,
    y = rep(c(3, 5, 4, 8, 2, 6), 4),
    n = rep(c(1.5, 2.5, 2, 4, 1, 3), 4)
  )

  result <- autospc(d,
    chart_type = "U", x = "x", y = "y", n = "n",
    plot_chart = FALSE, period_min = 21L, max_exclusions = 0L
  )

  ubar <- sum(d$y[1:21]) / sum(d$n[1:21])

  expect_equal(result$cl, rep(ubar, 24))
  expect_equal(result$ucl, ubar + 3 * sqrt(ubar) / sqrt(d$n))
  expect_equal(result$lcl, pmax(ubar - 3 * sqrt(ubar) / sqrt(d$n), 0))
  expect_equal(unique(result$sd_estimate), sqrt(ubar))
  expect_equal(result$series, d$y / d$n)
})


# presentation methods


test_that("chart_type_label returns the U chart label", {
  expect_identical(chart_type_label(chart_u(pre_agg_data)), "U")
})


test_that("y_axis_title returns the U chart axis title", {
  expect_identical(y_axis_title(chart_u(pre_agg_data)), "Rate")
})


test_that("labels have four significant figures at the scale of the axis", {
  expect_identical(label_accuracy(chart_u(pre_agg_data), ylimhigh = 110), 0.1)
  expect_equal(label_accuracy(chart_u(pre_agg_data), ylimhigh = 0.05), 1e-5)
})


test_that("the axis runs from zero to a tenth above the highest value", {
  data <- data.frame(
    series = c(1.2, 2.5, 0.4),
    ucl = c(3, 3, 3),
    lcl = c(0, 0, 0)
  )

  expect_equal(
    y_axis_range(chart_u(pre_agg_data), data = data),
    list(low = 0, high = 3.3)
  )
})


test_that("the axis extends below zero to a limit that is below it", {
  # with autospc.constrain_limits = FALSE a lower limit can be negative
  data <- data.frame(
    series = c(1.2, 2.5, 0.4),
    ucl = c(3, 3, 3),
    lcl = c(-0.5, -0.5, -0.5)
  )

  expect_identical(y_axis_range(chart_u(pre_agg_data), data = data)$low, -0.5)
})
