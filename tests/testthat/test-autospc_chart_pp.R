# P' takes the same input shapes as P — see test-autospc_chart_p.R — so the
# aggregation fixtures mirror those. The screening fixture is P'-specific.

pp_pre_agg_data <- data.frame(x = 1:5,
                              y = c(3, 4, 2, 5, 3),
                              n = rep(20L, 5),
                              site = "a")

pp_sub_level_data <- data.frame(x = rep(1:3, each = 2),
                                y = c(1, 2, 3, 4, 5, 6),
                                n = rep(10L, 6))

pp_binary_data <- data.frame(x = rep(1:5, each = 4),
                             y = rep(c(TRUE, FALSE, TRUE, TRUE), 5))

# one observation per subgroup, so every proportion is 0% or 100%
pp_binary_degenerate_data <- data.frame(x = 1:5,
                                        y = c(TRUE, FALSE, TRUE, TRUE, FALSE))

# a calculation period as the algorithm builds it: y holds percentages and
# y_numerator holds the counts
pp_period_data <- data.frame(
  x = 1:5,
  y_numerator = c(3, 4, 2, 5, 3),
  n = rep(20, 5),
  y = c(3, 4, 2, 5, 3) * 100 / rep(20, 5)
)

# a long stable series with a single spike, needed for the screening test: with
# a short series the spike's own two large moving ranges inflate the mean enough
# to cover themselves, so screening changes nothing and the test could not tell
# whether mr_screen_max_loops was read at all
pp_screening_numerator <- c(rep(c(3, 3, 4, 3, 3, 4, 3, 3, 4, 3), 2), 16)
pp_screening_data <- data.frame(
  x = seq_along(pp_screening_numerator),
  y_numerator = pp_screening_numerator,
  n = rep(20, length(pp_screening_numerator)),
  y = pp_screening_numerator * 100 / 20
)

chart_pp <- function(data, ...) {
  autospc_chart_pp(data = data, x = "x", y = "y", n = "n", ...)
}


test_that("autospc_chart_pp returns an object of the expected class", {

  expect_identical(class(chart_pp(pp_pre_agg_data)),
                   c("autospc_chart_pp", "autospc_chart"))

})


test_that("autospc_chart_pp carries the common elements plus its own", {

  chart <- chart_pp(pp_pre_agg_data)

  expect_true(all(autospc_chart_elements() %in% names(chart)))
  expect_true(all(autospc_chart_pp_elements() %in% names(chart)))
  expect_length(chart,
                length(autospc_chart_elements()) +
                  length(autospc_chart_pp_elements()))

})


test_that("n is required and is appended after the common elements", {

  expect_error(autospc_chart_pp(data = pp_pre_agg_data, x = "x", y = "y"),
               "argument \"n\" is missing")

  chart <- chart_pp(pp_pre_agg_data)

  expect_identical(names(chart)[length(chart)], "n")

})


test_that("data_original is populated correctly", {

  expect_identical(chart_pp(pp_pre_agg_data)$data_original, pp_pre_agg_data)

})


test_that("an unrecognised argument name is rejected", {

  expect_error(chart_pp(pp_pre_agg_data, period_mn = 30L), "unused argument")

})


test_that("a missing n is caught on the constructor path", {

  no_n <- new_autospc_chart_pp(autospc_chart_list(data = pp_pre_agg_data,
                                                  x = "x",
                                                  y = "y"))

  expect_error(validate_autospc_chart_pp(no_n),
               "autospc_chart_pp object - element\\(s\\) not present: n")

})


test_that("validate_autospc_chart_pp rejects a bare autospc_chart object", {

  expect_error(
    validate_autospc_chart_pp(autospc_chart(data = pp_pre_agg_data,
                                            x = "x",
                                            y = "y")),
    "Not an autospc_chart_pp object"
  )

})


test_that("validate_autospc_chart_pp rejects an autospc_chart_p object", {

  expect_error(
    validate_autospc_chart_pp(autospc_chart_p(data = pp_pre_agg_data,
                                              x = "x",
                                              y = "y",
                                              n = "n")),
    "Not an autospc_chart_pp object"
  )

})


test_that("validate_autospc_chart_pp returns a valid object unchanged", {

  chart <- chart_pp(pp_pre_agg_data)

  expect_identical(validate_autospc_chart_pp(chart), chart)

})


test_that("y_axis_title returns the P' chart axis title", {

  expect_identical(y_axis_title(chart_pp(pp_pre_agg_data)), "Percentage")

})


test_that("chart_type_label returns the P' chart label", {

  expect_identical(chart_type_label(chart_pp(pp_pre_agg_data)), "P'")

})


test_that("aggregate_data sums y and n over x", {

  chart <- aggregate_data(chart_pp(pp_sub_level_data))

  expect_identical(chart$data$x, 1:3)
  expect_identical(chart$data$y, c(3, 7, 11))
  expect_identical(chart$data$n, c(20L, 20L, 20L))

})


test_that("aggregate_data counts individual binary observations", {

  chart <- aggregate_data(chart_pp(pp_binary_data))

  expect_identical(chart$data$y, rep(3L, 5))
  expect_identical(chart$data$n, rep(4L, 5))

})


test_that("aggregate_data materialises n for one observation per subgroup", {

  chart <- aggregate_data(chart_pp(pp_binary_degenerate_data))

  expect_identical(chart$data$n, rep(1L, 5))
  expect_identical(chart$data$y, c(1L, 0L, 1L, 1L, 0L))

})


test_that("aggregate_data returns the same columns by either route", {

  expect_identical(names(aggregate_data(chart_pp(pp_pre_agg_data))$data),
                   names(aggregate_data(chart_pp(pp_sub_level_data))$data))

})


test_that("calculate_limits matches get_pp_limits", {

  expect_identical(
    calculate_limits(chart_pp(pp_pre_agg_data), pp_period_data,
                     exclusion_points = NULL),
    get_pp_limits(y = pp_period_data$y_numerator,
                  n = pp_period_data$n,
                  exclusion_points = NULL,
                  multiply = 100,
                  mr_screen_max_loops = 1L)
  )

})


test_that("calculate_limits uses y_numerator, not the percentage column y", {

  limits <- calculate_limits(chart_pp(pp_pre_agg_data), pp_period_data, NULL)

  expect_equal(limits$cl[1],
               sum(pp_period_data$y_numerator) /
                 sum(pp_period_data$n) * 100)

})


test_that("calculate_limits passes exclusion_points through", {

  expect_identical(
    calculate_limits(chart_pp(pp_pre_agg_data), pp_period_data,
                     exclusion_points = 4L),
    get_pp_limits(y = pp_period_data$y_numerator,
                  n = pp_period_data$n,
                  exclusion_points = 4L,
                  multiply = 100,
                  mr_screen_max_loops = 1L)
  )

  with_excl <- calculate_limits(chart_pp(pp_pre_agg_data), pp_period_data, 4L)
  without   <- calculate_limits(chart_pp(pp_pre_agg_data), pp_period_data, NULL)

  expect_lt(with_excl$cl[1], without$cl[1])

})


test_that("calculate_limits takes mr_screen_max_loops from the chart", {

  chart <- chart_pp(pp_pre_agg_data, mr_screen_max_loops = 0L)

  expect_identical(
    calculate_limits(chart, pp_screening_data, exclusion_points = NULL),
    get_pp_limits(y = pp_screening_data$y_numerator,
                  n = pp_screening_data$n,
                  exclusion_points = NULL,
                  multiply = 100,
                  mr_screen_max_loops = 0L)
  )

  # screening must actually change the limits on this data, otherwise the
  # comparison above would pass even if the field were ignored
  unscreened <- calculate_limits(chart_pp(pp_pre_agg_data,
                                          mr_screen_max_loops = 0L),
                                 pp_screening_data, NULL)
  screened   <- calculate_limits(chart_pp(pp_pre_agg_data,
                                          mr_screen_max_loops = 1L),
                                 pp_screening_data, NULL)

  expect_false(identical(unscreened, screened))

})
