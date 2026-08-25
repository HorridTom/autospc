# The compatibility contract - that the class and slots survive `+`, that
# ggsave() works, that print() draws - is stage 3 and lives in
# test-autospc_plot_ggplot_contract.R. This file tests construction and the
# class contract.

plot_data <- data.frame(x = 1:5,
                        y = c(3, 4, 2, 5, 3))

built_plot <- function() {

  ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point()

}

plot_chart_c <- function(...) {

  autospc_chart_c(data = plot_data, x = "x", y = "y", ...)

}

# autospc_plot() draws the plot it constructs, so these tests go a level below
# it: new_autospc_plot() writes the class and the slots, and
# validate_autospc_plot() checks them.
test_plot <- function(charts = list(plot_chart_c()),
                      passed = list(show_limits = TRUE),
                      derived = list()) {

  validate_autospc_plot(
    new_autospc_plot(plot = built_plot(),
                     charts = charts,
                     presentation = list(passed = passed,
                                         derived = derived))
  )

}


test_that("an autospc_plot is still a ggplot", {

  expect_s3_class(test_plot(), "ggplot")

  expect_s3_class(test_plot(), "gg")

})


test_that("autospc_plot comes first in the class vector", {

  # methods have to be found before ggplot2's
  expect_identical(class(test_plot()),
                   c("autospc_plot", "gg", "ggplot"))

})


test_that("the plot carries the charts it was drawn from", {

  chart <- plot_chart_c()

  expect_identical(autospc_plot_charts(test_plot(charts = list(chart))),
                   list(chart))

})


test_that("charts is a list even when it holds one", {

  expect_type(autospc_plot_charts(test_plot()), "list")

  expect_length(autospc_plot_charts(test_plot()), 1L)

})


test_that("a plot can hold two charts, as an XmR pair will", {

  pair <- list(plot_chart_c(), plot_chart_c())

  expect_length(autospc_plot_charts(test_plot(charts = pair)), 2L)

})


test_that("the plot carries the presentation parameters", {

  settings <- list(show_limits = FALSE, point_size = 4)

  expect_identical(autospc_plot_passed(test_plot(passed = settings)),
                   settings)

})


test_that("presentation returns both halves", {

  presentation <- autospc_plot_presentation(
    test_plot(passed = list(point_size = 4),
              derived = list(ylimhigh = 110))
  )

  expect_identical(presentation,
                   list(passed = list(point_size = 4),
                        derived = list(ylimhigh = 110)))

})


test_that("a single presentation parameter can be read by name", {

  plot <- test_plot(passed = list(show_limits = FALSE, point_size = 4))

  expect_identical(autospc_plot_passed(plot, "point_size"), 4)

})


test_that("a presentation parameter that was not supplied reads as NULL", {

  # the plot records what it was drawn with, not the renderer's defaults
  expect_null(autospc_plot_passed(test_plot(), "point_size"))

})


test_that("both halves can be empty", {

  plot <- test_plot(passed = list(), derived = list())

  expect_identical(autospc_plot_passed(plot), list())

  expect_identical(autospc_plot_derived(plot), list())

})


# validate_autospc_plot()

test_that("validate rejects an object that is not an autospc_plot", {

  expect_error(validate_autospc_plot(built_plot()),
               "Not an autospc_plot object")

})


test_that("validate rejects a plot that is not a ggplot", {

  not_a_plot <- structure(list(charts = list(plot_chart_c()),
                               presentation = list(passed = list(),
                                                   derived = list())),
                          class = "autospc_plot")

  expect_error(validate_autospc_plot(not_a_plot), "it is not a ggplot")

})


test_that("validate rejects a class vector in the wrong order", {

  wrong_order <- built_plot()
  wrong_order$charts <- list(plot_chart_c())
  wrong_order$presentation <- list(passed = list(), derived = list())
  class(wrong_order) <- c(class(wrong_order), "autospc_plot")

  expect_error(validate_autospc_plot(wrong_order), "must come first")

})


test_that("validate rejects a plot constructed twice", {

  # prepending the class a second time would leave methods dispatching normally
  # while the slots were silently overwritten
  twice <- new_autospc_plot(plot = test_plot(),
                            charts = list(plot_chart_c()),
                            presentation = list(passed = list(),
                                                derived = list()))

  expect_error(validate_autospc_plot(twice), "more than")

})


test_that("validate rejects missing elements", {

  no_charts <- built_plot()
  no_charts$presentation <- list(passed = list(), derived = list())
  class(no_charts) <- c("autospc_plot", class(no_charts))

  expect_error(validate_autospc_plot(no_charts),
               "element\\(s\\) not present: charts")

})


test_that("validate rejects an empty charts list", {

  expect_error(test_plot(charts = list()), "at least one")

})


test_that("validate rejects charts that are not autospc_chart objects", {

  expect_error(test_plot(charts = list(plot_data)),
               "every element of charts must be an autospc_chart")

})


test_that("a single chart is rejected with a message saying so", {

  # the natural mistake, and "must be a list of at least one" would not explain
  # it, because a chart is a list
  expect_error(autospc_plot(charts = plot_chart_c(),
                            passed = list()),
               "not a single chart")

})


test_that("validate rejects a half that is not a named list", {

  expect_error(test_plot(passed = c(show_limits = TRUE)),
               "presentation\\$passed must be a list")

  expect_error(test_plot(passed = list(TRUE)),
               "presentation\\$passed must be named")

  expect_error(test_plot(derived = list(110)),
               "presentation\\$derived must be named")

})


test_that("validate rejects a presentation missing a half", {

  half_only <- built_plot()
  half_only$charts <- list(plot_chart_c())
  half_only$presentation <- list(passed = list())
  class(half_only) <- c("autospc_plot", class(half_only))

  expect_error(validate_autospc_plot(half_only),
               "presentation element\\(s\\) not present: derived")

})


# derived

test_that("the plot carries the values worked out for the drawing", {

  values <- list(ylimhigh = 110, start_x = 1L)

  plot <- test_plot(derived = values)

  expect_identical(autospc_plot_derived(plot), values)

  expect_identical(autospc_plot_derived(plot, "ylimhigh"), 110)

})


test_that("a derived value that was not worked out reads as NULL", {

  expect_null(autospc_plot_derived(test_plot(), "ylimhigh"))

})
