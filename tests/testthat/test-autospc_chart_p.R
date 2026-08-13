# P data can arrive in three shapes: pre-aggregated counts at subgroup level,
# counts below subgroup level, or individual binary observations. Note that for
# the binary form the n column does not exist until aggregation creates it, so
# the chart's n field names a column that is not yet present.

pre_agg_data <- data.frame(x = 1:5,
                           y = c(3, 4, 2, 5, 3),
                           n = rep(20L, 5),
                           site = "a")

sub_level_data <- data.frame(x = rep(1:3, each = 2),
                             y = c(1, 2, 3, 4, 5, 6),
                             n = rep(10L, 6))

binary_data <- data.frame(x = rep(1:5, each = 4),
                          y = rep(c(TRUE, FALSE, TRUE, TRUE), 5))

# one observation per subgroup, so every proportion is 0% or 100%
binary_degenerate_data <- data.frame(x = 1:5,
                                     y = c(TRUE, FALSE, TRUE, TRUE, FALSE))

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

  expect_identical(class(chart_p(pre_agg_data)),
                   c("autospc_chart_p", "autospc_chart"))

})


test_that("autospc_chart_p carries the common elements plus its own", {

  chart <- chart_p(pre_agg_data)

  expect_true(all(autospc_chart_elements() %in% names(chart)))
  expect_true(all(autospc_chart_p_elements() %in% names(chart)))
  expect_length(chart,
                length(autospc_chart_elements()) +
                  length(autospc_chart_p_elements()))

})


test_that("n is required and is appended after the common elements", {

  expect_error(autospc_chart_p(data = pre_agg_data, x = "x", y = "y"),
               "argument \"n\" is missing")

  expect_identical(names(chart_p(pre_agg_data))[length(chart_p(pre_agg_data))],
                   "n")

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

  no_n <- new_autospc_chart_p(autospc_chart_list(data = pre_agg_data,
                                                 x = "x",
                                                 y = "y"))

  expect_error(validate_autospc_chart_p(no_n),
               "autospc_chart_p object - element\\(s\\) not present: n")

})


test_that("validate_autospc_chart_p rejects a bare autospc_chart object", {

  expect_error(
    validate_autospc_chart_p(new_autospc_chart(autospc_chart_list(data = pre_agg_data,
                                           x = "x",
                                           y = "y"))),
    "Not an autospc_chart_p object"
  )

})


test_that("validate_autospc_chart_p rejects a sibling subclass object", {

  expect_error(
    validate_autospc_chart_p(autospc_chart_c(data = pre_agg_data,
                                             x = "x",
                                             y = "y")),
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

  expect_identical(names(aggregate_data(chart_p(pre_agg_data))$data),
                   names(aggregate_data(chart_p(sub_level_data))$data))

})


test_that("aggregate_data returns a chart and leaves data_original untouched", {

  chart <- aggregate_data(chart_p(binary_data))

  expect_identical(class(chart), c("autospc_chart_p", "autospc_chart"))
  expect_identical(chart$data_original, binary_data)

})


test_that("calculate_limits matches get_p_limits", {

  expect_identical(
    calculate_limits(chart_p(pre_agg_data), proportion_period_data,
                     exclusion_points = NULL),
    get_p_limits(y = proportion_period_data$y_numerator,
                 n = proportion_period_data$n,
                 exclusion_points = NULL,
                 multiply = 100)
  )

})


test_that("calculate_limits uses y_numerator, not the percentage column y", {

  # the y column holds percentages; using it would give a centre line of 85
  # rather than the true 17%, with no error raised
  limits <- calculate_limits(chart_p(pre_agg_data),
                             proportion_period_data, NULL)

  expect_equal(limits$cl[1],
               sum(proportion_period_data$y_numerator) /
                 sum(proportion_period_data$n) * 100)

})


test_that("calculate_limits passes exclusion_points through", {

  expect_identical(
    calculate_limits(chart_p(pre_agg_data), proportion_period_data,
                     exclusion_points = 4L),
    get_p_limits(y = proportion_period_data$y_numerator,
                 n = proportion_period_data$n,
                 exclusion_points = 4L,
                 multiply = 100)
  )

  # excluding the highest point must lower the centre line, otherwise the
  # comparison above would pass even if the argument were ignored
  with_excl <- calculate_limits(chart_p(pre_agg_data),
                                proportion_period_data, 4L)
  without   <- calculate_limits(chart_p(pre_agg_data),
                                proportion_period_data, NULL)

  expect_lt(with_excl$cl[1], without$cl[1])

})
