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
    validate_autospc_chart_pp(new_autospc_chart(autospc_chart_list(data = pp_pre_agg_data,
                                            x = "x",
                                            y = "y"))),
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


test_that("limits_table_columns keeps n and y_numerator", {

  expect_identical(limits_table_columns(chart_pp(pp_pre_agg_data)),
                   c("n", "y_numerator"))

})


test_that("extend_display_limits recomputes the limits at each denominator", {

  # the limits of a P' chart depend on n, so they cannot be carried forward.
  # The width of the last calculated period is held as a constant and reapplied
  # at each display point's own denominator - so a point with a larger n gets
  # narrower limits.
  table <- data.frame(x = 1:5,
                      y = c(15, 15, 15, 15, 15),
                      n = c(100, 100, 100, 25, 400),
                      ucl = c(rep(25, 3), rep(NA_real_, 2)),
                      lcl = c(rep(5, 3), rep(NA_real_, 2)),
                      cl = c(rep(15, 3), rep(NA_real_, 2)),
                      periodType = c(rep("calculation", 3),
                                     rep(NA_character_, 2)))

  extended <- extend_display_limits(chart_pp(pp_pre_agg_data), table, counter = 4)

  # constant = (25 - 15) * sqrt(100) = 100, so the half-width is 100/sqrt(n)
  expect_equal(extended$ucl[4], 15 + 100 / sqrt(25))
  expect_equal(extended$lcl[5], 15 - 100 / sqrt(400))

  # the centre line is carried forward, and the calculated rows are untouched
  expect_identical(extended$cl, rep(15, 5))
  expect_identical(extended$periodType,
                   c(rep("calculation", 3), rep("display", 2)))

})


test_that("extend_display_limits clamps the recomputed limits to 0 and 100", {

  # percentages, so a limit outside 0-100 is meaningless
  table <- data.frame(x = 1:5,
                      y = c(50, 50, 50, 50, 50),
                      n = c(100, 100, 100, 1, 1),
                      ucl = c(rep(80, 3), rep(NA_real_, 2)),
                      lcl = c(rep(20, 3), rep(NA_real_, 2)),
                      cl = c(rep(50, 3), rep(NA_real_, 2)),
                      periodType = c(rep("calculation", 3),
                                     rep(NA_character_, 2)))

  extended <- extend_display_limits(chart_pp(pp_pre_agg_data), table, counter = 4)

  expect_true(all(extended$ucl[4:5] <= 100))
  expect_true(all(extended$lcl[4:5] >= 0))

})


test_that("extrapolate_limits recalculates from the final period", {

  # the limits of a P' chart vary with n, so there is no single set to carry
  # forward. They are recalculated from the final calculation period, giving
  # one set of values for the whole extension.
  final_period <- data.frame(y = c(10, 15, 20, 12, 18),   # percentages
                             n = c(100, 100, 200, 200, 100),
                             excluded = rep(FALSE, 5),
                             cl = rep(99, 5),             # deliberately wrong,
                             lcl = rep(99, 5),            # so a method that
                             ucl = rep(99, 5))            # echoes them fails

  limits <- extrapolate_limits(chart_pp(pp_pre_agg_data), final_period)

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

  base_period <- data.frame(y = c(15, 15, 15, 60, 15),
                            n = rep(100, 5),
                            excluded = rep(FALSE, 5),
                            cl = rep(15, 5),
                            lcl = rep(5, 5),
                            ucl = rep(25, 5))

  excluded_period <- base_period
  excluded_period$excluded <- c(FALSE, FALSE, FALSE, TRUE, FALSE)

  with_spike <- extrapolate_limits(chart_pp(pp_pre_agg_data), base_period)
  without_spike <- extrapolate_limits(chart_pp(pp_pre_agg_data), excluded_period)

  expect_lt(without_spike$cl, with_spike$cl)

})


test_that("prepare_data turns counts into percentages and keeps the count", {

  counts <- data.frame(x = 1:4,
                       y = c(10, 20, 30, 40),
                       n = c(100, 200, 100, 200))

  prepared <- prepare_data(autospc_chart_pp(data = counts, x = "x", y = "y", n = "n"))

  expect_identical(prepared$data$y, c(10, 10, 30, 20))
  expect_identical(prepared$data$y_numerator, counts$y)
  expect_identical(prepared$data$n, counts$n)

  expect_identical(prepared$data_original, counts)

})


test_that("prepare_data gives NA for a zero or missing denominator", {

  # rather than NaN or Inf, which would propagate into the limits
  counts <- data.frame(x = 1:3,
                       y = c(10, 10, 10),
                       n = c(100, 0, NA_real_))

  prepared <- prepare_data(autospc_chart_pp(data = counts, x = "x", y = "y", n = "n"))

  expect_identical(prepared$data$y, c(10, NA_real_, NA_real_))

})
