# autospc_chart() and has_autospc_chart_class() each list the chart types
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


test_that("XMR is an accepted chart type but has no class yet", {

  # so autospc_chart() cannot build one

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


# has_autospc_chart_class()

test_that("has_autospc_chart_class is TRUE for accepted types with a class", {

  # X is excluded here and tested separately below - it has a class, but is not
  # an accepted chart_type
  for(chart_type in setdiff(names(factory_classes), "X")) {

    expect_true(has_autospc_chart_class(chart_type), info = chart_type)

  }

})


test_that("has_autospc_chart_class is FALSE for the two types at either edge", {

  # XMR and X both return FALSE, for opposite reasons, and both must go on
  # doing so:
  #
  # XMR is an accepted chart_type that has no class - it needs two SSA runs and
  # the plot object.
  expect_true("XMR" %in% autospc_chart_types())
  expect_false(has_autospc_chart_class("XMR"))

  # X is the other way round: it has a class, and autospc_chart() will build
  # one, but it is not an accepted chart_type because nothing later in
  # autospc() can chart it - form_limits.R has no branch for X. So autospc()
  # must not build one either.
  expect_false("X" %in% autospc_chart_types())
  expect_false(has_autospc_chart_class("X"))

})


test_that("has_autospc_chart_class is FALSE for bad input, not an error", {

  # autospc() calls this before chart_type has been checked, so whatever the
  # user passed has to come back FALSE and leave validate_chart_type() to
  # produce the error message. The case that matters is a chart_type holding
  # two values: test-autospc-chart-type.R passes c("XMR", "MR") on purpose, and
  # in R 4.3 and later `&&` errors if either side is longer than one value.
  expect_false(has_autospc_chart_class(NULL))
  expect_false(has_autospc_chart_class(c("XMR", "MR")))
  expect_false(has_autospc_chart_class(c("C", "P")))
  expect_false(has_autospc_chart_class(character(0)))
  expect_false(has_autospc_chart_class(5))
  expect_false(has_autospc_chart_class(NA))

})


test_that("every chart type has_autospc_chart_class accepts can be built", {

  # has_autospc_chart_class() takes its list from autospc_chart_types(), while
  # autospc_chart() names each chart type in a separate branch. Nothing else
  # checks that the two agree.
  for(chart_type in autospc_chart_types()) {

    if(has_autospc_chart_class(chart_type)) {

      expect_no_error(autospc_chart(chart_type = chart_type,
                                    data = factory_data,
                                    x = "x",
                                    y = "y",
                                    n = "n"))

    }

  }

})


test_that("autospc() builds a chart object during a real run", {

  # TEMPORARY - delete when autospc() starts using the chart object. See
  # CLEAN UP #16 in the worklist.
  #
  # Nothing reads the chart object yet, so if has_autospc_chart_class() started
  # returning FALSE for everything, no object would ever be built and every
  # other test would still pass. This is the only test that checks autospc()
  # really does call autospc_chart(). Once the object is used, the end to end
  # tests cover that and this one should go.
  built <- autospc_chart

  called <- NULL

  testthat::local_mocked_bindings(
    autospc_chart = function(chart_type, ...) {
      called <<- chart_type
      built(chart_type = chart_type, ...)
    }
  )

  invisible(autospc(autospc::ed_attendances_monthly,
                    chart_type = "C'",
                    x = month_start,
                    y = att_all,
                    plot_chart = FALSE))

  expect_identical(called, "C'")

})
