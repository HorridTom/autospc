
test_that("Integer x breaks are as specified by x_break", {
  
  plot_10 <- autospc(example_series_1,
                     chart_type = "C",
                     x_break = 10)
  
  plot_20 <- autospc(example_series_1,
                     chart_type = "C",
                     x_break = 20)
  
  plot_5<- autospc(example_series_1,
                   chart_type = "C",
                   x_break = 5)
  
  n_x_breaks_10 <- length(as.numeric(na.omit(
    ggplot2::layer_scales(plot_10)$x$break_positions())))
  
  n_x_breaks_20 <- length(as.numeric(na.omit(
    ggplot2::layer_scales(plot_20)$x$break_positions())))
  
  n_x_breaks_5 <- length(as.numeric(na.omit(
    ggplot2::layer_scales(plot_5)$x$break_positions())))
  
  expect_equal(n_x_breaks_10,
               13L)
  
  expect_equal(n_x_breaks_20,
               7L)
  
  expect_equal(n_x_breaks_5,
               25L)
  
})
