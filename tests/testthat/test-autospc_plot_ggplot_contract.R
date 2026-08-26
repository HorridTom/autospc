# An autospc_plot is a ggplot subclass, so ggplot2 keeps working on it. These
# tests are the tripwire for that stopping being true.
#
# Two of them - that the class survives `+`, and that the slots survive `+` -
# hold because ggplot2 copies the object's list wholesale rather than rebuilding
# it. Nothing documents that it must. If ggplot2 ever rebuilds instead, a user
# adding a layer would get back something still classed autospc_plot with no
# charts on it, and nothing would error. These tests fail instead.

contract_data <- data.frame(x = 1:5,
                            y = c(3, 4, 2, 5, 3))

contract_plot <- function(
    plot = ggplot2::ggplot(contract_data, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(),
    charts = list(autospc_chart_c(data = contract_data, x = "x", y = "y")),
    visualisation_params = list(point_size = 4)) {

  validate_autospc_plot(
    new_autospc_plot(plot = plot,
                     charts = charts,
                     presentation = list(
                       visualisation_params = visualisation_params,
                       derived = list()
                     ))
  )

}

test_that("adding a theme keeps the class and the slots", {

  themed <- contract_plot() + ggplot2::theme_minimal()

  expect_identical(class(themed), c("autospc_plot", "gg", "ggplot"))

  expect_length(autospc_plot_charts(themed), 1L)

  expect_identical(autospc_plot_visualisation_params(themed, "point_size"), 4)

})


test_that("adding a layer keeps the class and the slots", {

  layered <- contract_plot() + ggplot2::geom_line()

  expect_identical(class(layered), c("autospc_plot", "gg", "ggplot"))

  expect_length(autospc_plot_charts(layered), 1L)

  expect_identical(autospc_plot_visualisation_params(layered, "point_size"), 4)

})


test_that("adding a scale keeps the class and the slots", {

  # scales are cloned rather than copied, which is the one part of the object
  # ggplot2 handles differently on `+`
  scaled <- contract_plot() + ggplot2::scale_y_continuous(limits = c(0, 10))

  expect_identical(class(scaled), c("autospc_plot", "gg", "ggplot"))

  expect_length(autospc_plot_charts(scaled), 1L)

})


test_that("ggsave writes a file", {

  path <- tempfile(fileext = ".png")
  on.exit(unlink(path))

  ggplot2::ggsave(path, plot = contract_plot(), width = 3, height = 2)

  expect_true(file.exists(path))

})


test_that("printing draws without error", {

  expect_no_error(drawn(contract_plot()))

})


test_that("ggplot_build accepts it", {

  # what ggsave, print and every renderer go through
  expect_no_error(ggplot2::ggplot_build(contract_plot()))

})


test_that("a cowplot composite can be subclassed the same way", {

  # the XmR pair is drawn by cowplot::plot_grid(), so the object handed to
  # autospc_plot() is a composite rather than a single plot
  composite <- cowplot::plot_grid(
    ggplot2::ggplot(contract_data, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(),
    ggplot2::ggplot(contract_data, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_line()
  )

  paired <- contract_plot(
    plot = composite,
    charts = list(autospc_chart_c(data = contract_data, x = "x", y = "y"),
                  autospc_chart_mr(data = contract_data, x = "x", y = "y"))
  )

  expect_identical(class(paired), c("autospc_plot", "gg", "ggplot"))

  expect_length(autospc_plot_charts(paired), 2L)

  expect_no_error(drawn(paired))

})


test_that("attaching the slots to a ggplot warns about nothing", {

  # ggplot2 3.5.2 treats a ggplot as a list, so writing new elements into one is
  # ordinary assignment. This is the assertion that says so, in place of the
  # blanket suppressWarnings() that used to wrap the construction.
  expect_no_warning(
    contract_plot(plot = ggplot2::ggplot(contract_data,
                                         ggplot2::aes(x = x, y = y)),
                  charts = autospc_plot_charts(contract_plot()))
  )

})
