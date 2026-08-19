x_type_data <- data.frame(
  x = 1:30,
  y = c(50, 48, 49, 50, 52, 52, 48, 53, 51, 51,
        47, 52, 51, 47, 48, 49, 51, 51, 45, 49,
        49, 50, 48, 53, 49, 48, 51, 46, 48, 49)
)


test_that("X is accepted by autospc", {

  expect_no_error(autospc(x_type_data,
                          chart_type = "X",
                          period_min = 21L,
                          plot_chart = FALSE))

})


test_that("an X run gives an autospc_chart_x", {

  plot <- autospc(x_type_data, chart_type = "X", period_min = 21L)

  expect_s3_class(autospc_plot_charts(plot)[[1]], "autospc_chart_x")

})


test_that("X gives the same result as XMR without the moving range chart", {

  # this equivalence is what let show_mr be deprecated
  expect_equal(
    autospc(x_type_data, chart_type = "X", period_min = 21L,
            plot_chart = FALSE),
    suppressWarnings(
      autospc(x_type_data, chart_type = "XMR", show_mr = FALSE,
              period_min = 21L, plot_chart = FALSE)
    )
  )

})


test_that("an X chart draws", {

  expect_no_error(
    drawn(autospc(x_type_data, chart_type = "X", period_min = 21L))
  )

})


test_that("X requires y, and says so in the same terms as MR and XMR", {

  expect_error(autospc(x_type_data[, "x", drop = FALSE],
                       chart_type = "X",
                       period_min = 21L,
                       plot_chart = FALSE),
               "For X, MR and XMR charts, y must be specified",
               fixed = TRUE)

})
