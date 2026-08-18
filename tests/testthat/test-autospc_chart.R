# autospc_chart() and chart_type_for_object() each list the chart types
# separately, so this mapping is written out once and used by both sets of
# tests below
factory_classes <- c("C"  = "autospc_chart_c",
                     "C'" = "autospc_chart_cp",
                     "P"  = "autospc_chart_p",
                     "P'" = "autospc_chart_pp",
                     "X"  = "autospc_chart_x",
                     "MR" = "autospc_chart_mr")

factory_data <- data.frame(x = 1:5,
                           y = c(3, 4, 2, 5, 3),
                           n = rep(20L, 5))


test_that("new_autospc_chart rejects an object that is not a list", {

  expect_error(new_autospc_chart("not a list"), "is\\.list")

})


test_that("validate_autospc_chart rejects a list that is not a chart", {

  bare_list <- autospc_chart_list(data = test_data, x = "x", y = "y")

  expect_error(validate_autospc_chart(bare_list),
               "Not an autospc_chart object")

})


test_that("y_axis_title has no method for a bare autospc_chart", {

  bare_chart <- new_autospc_chart(autospc_chart_list(data = test_data,
                                                    x = "x",
                                                    y = "y"))

  expect_error(y_axis_title(bare_chart), "no applicable method")

})


test_that("aggregate_data returns a bare autospc_chart unchanged", {

  bare_chart <- new_autospc_chart(autospc_chart_list(data = test_data,
                                                    x = "x",
                                                    y = "y"))

  expect_identical(aggregate_data(bare_chart), bare_chart)

})


test_that("chart_type_label has no method for a bare autospc_chart", {

  bare_chart <- new_autospc_chart(autospc_chart_list(data = test_data,
                                                    x = "x",
                                                    y = "y"))

  expect_error(chart_type_label(bare_chart), "no applicable method")

})


test_that("calculate_limits has no method for a bare autospc_chart", {

  bare_chart <- new_autospc_chart(autospc_chart_list(data = test_data,
                                                    x = "x",
                                                    y = "y"))

  expect_error(
    calculate_limits(bare_chart, test_data, exclusion_points = NULL),
    "no applicable method"
  )

})


# autospc_chart()

test_that("autospc_chart returns the right class for every type it handles", {

  for(chart_type in names(factory_classes)) {

    chart <- autospc_chart(chart_type = chart_type,
                           data = factory_data,
                           x = "x",
                           y = "y",
                           n = "n")

    expect_identical(class(chart),
                     c(factory_classes[[chart_type]], "autospc_chart"),
                     info = chart_type)

  }

})


test_that("n is not required by the types that do not use it", {

  # only the P and P' branches use n, and R does not evaluate an argument that
  # nothing looks at, so the other four must build with no n supplied
  for(chart_type in c("C", "C'", "X", "MR")) {

    expect_no_error(autospc_chart(chart_type = chart_type,
                                  data = factory_data,
                                  x = "x",
                                  y = "y"))

  }

})


test_that("n is required by the proportion charts", {

  for(chart_type in c("P", "P'")) {

    expect_error(autospc_chart(chart_type = chart_type,
                               data = factory_data,
                               x = "x",
                               y = "y"),
                 "argument \"n\" is missing")

  }

})


test_that("an unknown chart type errors rather than returning NULL", {

  # a switch() with no matching branch and no default returns NULL without
  # printing anything, so a mistyped chart type would produce a NULL chart that
  # went on to fail somewhere else entirely
  expect_error(autospc_chart(chart_type = "Q",
                             data = factory_data,
                             x = "x",
                             y = "y"),
               "No autospc_chart class for chart_type")

})


test_that("XMR is an accepted chart type but autospc_chart cannot build it", {

  # XMR asks for a pair of charts - an X and an MR - so no single object serves
  # it, and there is no autospc_chart_xmr class waiting to be written. Unlike
  # X, this does not change when the chart types are widened.

  expect_true("XMR" %in% autospc_chart_types())

  expect_error(autospc_chart(chart_type = "XMR",
                             data = factory_data,
                             x = "x",
                             y = "y"),
               "No autospc_chart class for chart_type")

})


test_that("arguments passed through ... reach the object", {

  chart <- autospc_chart(chart_type = "C",
                         data = factory_data,
                         x = "x",
                         y = "y",
                         period_min = 30L,
                         no_regrets = FALSE)

  expect_identical(chart$period_min, 30L)
  expect_identical(chart$no_regrets, FALSE)

})


test_that("autospc_chart keeps data_original as passed", {

  chart <- autospc_chart(chart_type = "C",
                         data = factory_data,
                         x = "x",
                         y = "y")

  expect_identical(chart$data_original, factory_data)

})


# prepare_data()

test_that("prepare_data returns the chart unchanged by default", {

  for(chart_type in c("C", "C'", "X")) {

    chart <- autospc_chart(chart_type = chart_type,
                           data = factory_data,
                           x = "x",
                           y = "y")

    expect_identical(prepare_data(chart), chart, info = chart_type)

  }

})


# n_effective_points()

test_that("n_effective_points counts the non-missing values of y", {

  chart <- autospc_chart(chart_type = "C",
                         data = factory_data,
                         x = "x",
                         y = "y")

  expect_identical(n_effective_points(chart, data.frame(y = c(1, NA, 3, 4))),
                   3L)

})


test_that("the classes other than MR count the rows as they are", {

  counted <- data.frame(y = c(1, NA, 3, 4))

  for(chart_type in c("C", "C'", "X")) {

    chart <- autospc_chart(chart_type = chart_type,
                           data = factory_data,
                           x = "x",
                           y = "y")

    expect_identical(n_effective_points(chart, counted), 3L, info = chart_type)

  }

})


# extend_display_limits()

# a limits table as the algorithm builds it: three calculated rows, then three
# rows waiting for display limits
display_table <- data.frame(x = 1:6,
                            y = c(10, 12, 11, 14, 9, 13),
                            n = rep(20, 6),
                            ucl = c(rep(18, 3), rep(NA_real_, 3)),
                            lcl = c(rep(4, 3), rep(NA_real_, 3)),
                            cl = c(rep(11, 3), rep(NA_real_, 3)),
                            periodType = c(rep("calculation", 3),
                                           rep(NA_character_, 3)))


test_that("the default carries the last calculated limits forward", {

  chart <- autospc_chart(chart_type = "C",
                         data = factory_data,
                         x = "x",
                         y = "y")

  extended <- extend_display_limits(chart, display_table, counter = 4)

  expect_identical(extended$ucl, rep(18, 6))
  expect_identical(extended$lcl, rep(4, 6))
  expect_identical(extended$cl, rep(11, 6))
  expect_identical(extended$periodType,
                   c(rep("calculation", 3), rep("display", 3)))

})


test_that("the calculated rows are left alone", {

  chart <- autospc_chart(chart_type = "C",
                         data = factory_data,
                         x = "x",
                         y = "y")

  extended <- extend_display_limits(chart, display_table, counter = 4)

  expect_identical(extended[1:3, ], display_table[1:3, ])

})


test_that("the classes with no override inherit the carry-forward default", {

  for(chart_type in c("C", "C'", "X", "MR")) {

    chart <- autospc_chart(chart_type = chart_type,
                           data = factory_data,
                           x = "x",
                           y = "y")

    extended <- extend_display_limits(chart, display_table, counter = 4)

    expect_identical(extended$ucl, rep(18, 6), info = chart_type)

  }

})


# extrapolate_limits()

test_that("the default averages the final period's limits", {

  # for the classes with constant limits within a period the mean is just that
  # constant value, but a mean is what the original code took, so this pins it
  final_period <- data.frame(cl = c(11, 11, 11),
                             lcl = c(4, 4, 4),
                             ucl = c(18, 18, 18))

  chart <- autospc_chart(chart_type = "C",
                         data = factory_data,
                         x = "x",
                         y = "y")

  expect_identical(extrapolate_limits(chart, final_period),
                   list(cl = 11, lcl = 4, ucl = 18))

})


test_that("the default ignores missing limit values", {

  final_period <- data.frame(cl = c(11, NA, 11),
                             lcl = c(4, NA, 4),
                             ucl = c(18, NA, 18))

  chart <- autospc_chart(chart_type = "C",
                         data = factory_data,
                         x = "x",
                         y = "y")

  expect_identical(extrapolate_limits(chart, final_period),
                   list(cl = 11, lcl = 4, ucl = 18))

})


test_that("the default averages, rather than taking a single row", {

  # cannot arise today - the limits of these classes are constant within a
  # calculation period, so mean, first and last all agree. This pins the choice
  # so that it stays deliberate if that ever changes.
  final_period <- data.frame(cl = c(10, 20),
                             lcl = c(2, 4),
                             ucl = c(18, 36))

  chart <- autospc_chart(chart_type = "C",
                         data = factory_data,
                         x = "x",
                         y = "y")

  expect_identical(extrapolate_limits(chart, final_period),
                   list(cl = 15, lcl = 3, ucl = 27))

})


test_that("the classes with no override inherit the averaging default", {

  final_period <- data.frame(cl = c(11, 11), lcl = c(4, 4), ucl = c(18, 18))

  for(chart_type in c("C", "C'", "X", "MR")) {

    chart <- autospc_chart(chart_type = chart_type,
                           data = factory_data,
                           x = "x",
                           y = "y")

    expect_identical(extrapolate_limits(chart, final_period),
                     list(cl = 11, lcl = 4, ucl = 18),
                     info = chart_type)

  }

})


# limits_table_columns()

test_that("limits_table_columns is empty by default", {

  bare_chart <- new_autospc_chart(autospc_chart_list(data = test_data,
                                                     x = "x",
                                                     y = "y"))

  expect_identical(limits_table_columns(bare_chart), character(0))

})


test_that("the classes with no override inherit the empty default", {

  # only P and P' calculate limits from a column other than the plotted y, so
  # only they need extra columns kept
  for(chart_type in c("C", "C'", "X", "MR")) {

    chart <- autospc_chart(chart_type = chart_type,
                           data = factory_data,
                           x = "x",
                           y = "y")

    expect_identical(limits_table_columns(chart), character(0),
                     info = chart_type)

  }

})


# chart_type_for_object()

test_that("every chart type except XMR maps to itself", {

  for(chart_type in setdiff(autospc_chart_types(), "XMR")) {

    expect_identical(chart_type_for_object(chart_type), chart_type,
                     info = chart_type)

  }

})


test_that("XMR maps to X", {

  # XMR asks for a pair of charts. The MR half is created by the
  # chart_type = "MR" re-invocation in autospc(), so the X half is all that is
  # needed here.
  expect_identical(chart_type_for_object("XMR"), "X")

})


test_that("X is not yet a chart type a user can pass", {

  # X has a class and autospc_chart() will build one, but nothing later in
  # autospc() can chart it - form_limits.R has no branch for X. This is
  # temporary: X becomes user-passable later, which is what allows show_mr to
  # be dropped. Update this test then.
  expect_false("X" %in% autospc_chart_types())

})


test_that("chart_type_for_object returns NULL for bad input, not an error", {

  # autospc() calls this before chart_type has been checked, so whatever the
  # user passed has to come back NULL and leave validate_chart_type() to
  # produce the error message. The case that matters is a chart_type holding
  # two values: test-autospc-chart-type.R passes c("XMR", "MR") on purpose, and
  # in R 4.3 and later `&&` errors if either side is longer than one value.
  expect_null(chart_type_for_object(NULL))
  expect_null(chart_type_for_object(c("XMR", "MR")))
  expect_null(chart_type_for_object(c("C", "P")))
  expect_null(chart_type_for_object(character(0)))
  expect_null(chart_type_for_object(5))
  expect_null(chart_type_for_object(NA))
  expect_null(chart_type_for_object("Q"))

})


test_that("every chart type a user can pass can be built", {

  # chart_type_for_object() takes its list from autospc_chart_types(), while
  # autospc_chart() names each chart type in a separate branch. Nothing else
  # checks that the two agree. Every accepted chart type must now map to
  # something buildable - there is no longer one that maps to NULL.
  for(chart_type in autospc_chart_types()) {

    object_chart_type <- chart_type_for_object(chart_type)

    expect_false(is.null(object_chart_type), info = chart_type)

    expect_no_error(autospc_chart(chart_type = object_chart_type,
                                  data = factory_data,
                                  x = "x",
                                  y = "y",
                                  n = "n"))

  }

})


# first_label_row(), label_accuracy(), y_axis_range()

test_that("the first centre line label goes on row one by default", {

  chart <- autospc_chart(chart_type = "C", data = test_data, x = "x", y = "y")

  expect_identical(first_label_row(chart), 1L)

})


test_that("centre line labels are rounded to whole numbers by default", {

  chart <- autospc_chart(chart_type = "C", data = test_data, x = "x", y = "y")

  expect_identical(label_accuracy(chart, ylimhigh = 1000), 1)

})


test_that("the default y axis range is a percentage scale", {

  bare_chart <- new_autospc_chart(autospc_chart_list(data = test_data,
                                                     x = "x",
                                                     y = "y"))

  expect_identical(y_axis_range(bare_chart, limits_data),
                   list(low = 0, high = 110))

})


# the fit slots


test_that("a new chart has empty result and history slots", {

  chart <- autospc_chart(chart_type = "C", data = test_data, x = "x", y = "y")

  expect_true(all(c("result", "history") %in% autospc_chart_elements()))
  expect_identical(chart$result, list())
  expect_identical(chart$history, list())

})


test_that("validate_autospc_chart requires the fit slots", {

  chart <- autospc_chart(chart_type = "C", data = test_data, x = "x", y = "y")
  chart$result <- NULL

  expect_error(validate_autospc_chart(chart),
               "element\\(s\\) not present: result")

})
