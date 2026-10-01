# U' data takes the same forms as U data, a count y and the area of
# opportunity n it was made over, and is validated by the same helpers. What
# differs is the standard deviation estimate, which carries Laney's sigma_z.

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

# rates that vary far more than a Poisson count would, over areas of
# opportunity that vary
overdispersed <- data.frame(
  x = 1:24,
  y = c(
    12, 30, 8, 25, 40, 10, 18, 35, 6, 28, 15, 33,
    9, 22, 38, 11, 27, 14, 31, 7, 20, 26, 16, 29
  ),
  n = rep(c(10, 15, 8, 12, 20, 9), 4)
)

chart_up <- function(data, ...) {
  autospc_chart_up(data = data, x = "x", y = "y", n = "n", ...)
}


# the object


test_that("autospc_chart_up returns an object of the expected class", {
  expect_identical(
    class(chart_up(pre_agg_data)),
    c("autospc_chart_up", "autospc_chart")
  )
})


test_that("autospc_chart_up carries the common elements plus its own", {
  chart <- chart_up(pre_agg_data)

  expect_true(all(autospc_chart_up_elements() %in% names(chart)))
  expect_length(
    chart,
    length(autospc_chart_elements()) +
      length(autospc_chart_up_elements())
  )
})


test_that("n is required", {
  expect_error(
    autospc_chart_up(data = pre_agg_data, x = "x", y = "y"),
    "argument \"n\" is missing"
  )
})


test_that("a missing n element is caught on the constructor path", {
  no_n <- new_autospc_chart_up(assemble_chart_list(
    data = pre_agg_data,
    x = "x",
    y = "y"
  ))

  expect_error(
    validate_autospc_chart_up(no_n),
    "autospc_chart_up object - element\\(s\\) not present: n"
  )
})


test_that("validate_autospc_chart_up rejects a sibling subclass object", {
  expect_error(
    validate_autospc_chart_up(autospc_chart_u(
      data = pre_agg_data,
      x = "x",
      y = "y",
      n = "n"
    )),
    "Not an autospc_chart_up object"
  )
})


test_that("validate_autospc_chart_up returns a valid object unchanged", {
  chart <- chart_up(pre_agg_data)

  expect_identical(validate_autospc_chart_up(chart), chart)
})


# the columns, checked by the helpers shared with U


test_that("a U' chart needs an n column", {
  expect_error(
    chart_up(data.frame(x = 1:5, y = c(3, 4, 2, 5, 3))),
    "n not specified. For U and U' charts, n must be specified",
    fixed = TRUE
  )
})


test_that("a count that is not a whole number is rounded, with a warning", {
  fractional <- pre_agg_data
  fractional$y[2] <- 4.4

  expect_warning(
    chart <- chart_up(fractional),
    "U and U' charts require y to be a count",
    fixed = TRUE
  )

  expect_identical(chart$data$y, c(3, 4, 2, 5, 3))
  expect_identical(chart$data$n, pre_agg_data$n)
})


test_that("a count over an area of opportunity of zero is refused", {
  no_opportunity <- pre_agg_data
  no_opportunity$n[3] <- 0

  expect_error(
    chart_up(no_opportunity),
    "For U and U' charts, y must be 0 where n is 0.",
    fixed = TRUE
  )
})


# analysis methods


test_that("aggregate_data sums y and n over x", {
  chart <- aggregate_data(chart_up(sub_level_data))

  expect_identical(chart$data$x, 1:3)
  expect_identical(chart$data$y, c(3, 7, 11))
  expect_identical(chart$data$n, c(1.5, 2, 2.5))
})


test_that("prepare_data turns counts into rates and keeps the count", {
  prepared <- prepare_data(chart_up(pre_agg_data))

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

  prepared <- prepare_data(chart_up(counts))

  expect_identical(prepared$data$series, c(5, NA_real_, NA_real_))
})


test_that("calculate_limits matches get_up_statistics", {
  expect_identical(
    calculate_limits(chart_up(overdispersed),
      period = overdispersed,
      exclusion_points = 4L
    ),
    get_up_statistics(
      y = overdispersed$y,
      n = overdispersed$n,
      exclusion_points = 4L,
      mr_screen_max_loops = autospc_default("mr_screen_max_loops")
    )
  )
})


test_that("calculate_limits screens as many times as the chart says", {
  # the jump in the last rate gives a moving range the default screening
  # removes
  jump <- data.frame(
    x = 1:7,
    y = c(10, 11, 10, 11, 10, 11, 40),
    n = rep(10, 7)
  )

  screened <- calculate_limits(chart_up(jump),
    period = jump,
    exclusion_points = NULL
  )
  unscreened <- calculate_limits(chart_up(jump, mr_screen_max_loops = 0L),
    period = jump,
    exclusion_points = NULL
  )

  expect_identical(
    unscreened,
    get_up_statistics(
      y = jump$y,
      n = jump$n,
      mr_screen_max_loops = 0L
    )
  )
  expect_lt(screened$sd_estimate[1], unscreened$sd_estimate[1])
})


test_that("the standard error is the estimate over the square root of n", {
  expect_equal(
    standard_error_at(chart_up(pre_agg_data),
      sd_estimate = 2,
      rows = pre_agg_data
    ),
    2 / sqrt(pre_agg_data$n)
  )
})


test_that("a rate is bounded below at zero and not above", {
  expect_identical(
    limit_bounds(chart_up(pre_agg_data)),
    list(low = 0, high = Inf)
  )
})


test_that("limits_table_columns keeps y and n", {
  expect_identical(
    limits_table_columns(chart_up(pre_agg_data)),
    c("y", "n")
  )
})


test_that("the limits are Laney's, through autospc()", {
  # with no screening, as Laney (2002) calculates them; the first 21 points
  # form the period and the last three are display rows at their own n
  result <- autospc(overdispersed,
    chart_type = "U'", x = "x", y = "y", n = "n",
    plot_chart = FALSE, period_min = 21L, max_exclusions = 0L,
    mr_screen_max_loops = 0L
  )

  y <- overdispersed$y[1:21]
  n <- overdispersed$n[1:21]
  ubar <- sum(y) / sum(n)
  z <- (y / n - ubar) / sqrt(ubar / n)
  sigma_z <- mean(abs(diff(z))) / d2_constant()

  half_width <- 3 * sqrt(ubar / overdispersed$n) * sigma_z

  expect_equal(result$cl, rep(ubar, 24))
  expect_equal(result$ucl, ubar + half_width)
  expect_equal(result$lcl, pmax(ubar - half_width, 0))
  expect_equal(unique(result$sd_estimate), sqrt(ubar) * sigma_z)
})


test_that("U' limits are wider than U limits on overdispersed rates", {
  analyse <- function(chart_type) {
    autospc(overdispersed,
      chart_type = chart_type, x = "x", y = "y", n = "n",
      plot_chart = FALSE, period_min = 21L, max_exclusions = 0L
    )
  }

  u <- analyse("U")
  up <- analyse("U'")

  expect_identical(up$cl, u$cl)
  expect_true(all(up$ucl > u$ucl))
})


# presentation methods


test_that("chart_type_label returns the U' chart label", {
  expect_identical(chart_type_label(chart_up(pre_agg_data)), "U'")
})


test_that("y_axis_title returns the U' chart axis title", {
  expect_identical(y_axis_title(chart_up(pre_agg_data)), "Rate")
})


test_that("labels have four significant figures at the scale of the axis", {
  expect_equal(label_accuracy(chart_up(pre_agg_data), ylimhigh = 0.05), 1e-5)
})


test_that("the axis runs from zero, or a limit below it, to a tenth above", {
  data <- data.frame(
    series = c(1.2, 2.5, 0.4),
    ucl = c(3, 3, 3),
    lcl = c(-0.5, -0.5, -0.5)
  )

  expect_equal(
    y_axis_range(chart_up(pre_agg_data), data = data),
    list(low = -0.5, high = 3.3)
  )
})
