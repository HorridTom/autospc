# The "Period Type" legend's entries.

legend_series <- data.frame(
  x = example_series_1$x,
  y = round(example_series_1$y)
)

# The labels of the scale named "Period Type"
period_type_labels <- function(plot) {
  scales <- ggplot2::ggplot_build(plot)$plot$scales$scales

  is_period_type <- vapply(
    scales,
    function(scale) identical(scale$name, "Period Type"),
    logical(1L)
  )

  return(scales[is_period_type][[1]]$get_labels())
}


test_that("the legend shows both period types", {
  plot <- autospc(legend_series, chart_type = "C", max_exclusions = 0L)

  expect_identical(period_type_labels(plot), c("Calculation", "Display"))
})


test_that("the legend shows both period types after leading missing values", {
  leading_na <- legend_series
  leading_na$y[1:2] <- NA

  plot <- autospc(leading_na, chart_type = "C", max_exclusions = 0L)

  expect_identical(period_type_labels(plot), c("Calculation", "Display"))
})


test_that("the legend shows both period types after a segment without limits", {
  plot <- autospc(legend_series,
    chart_type = "C", max_exclusions = 0L, force_restart_at = 10
  )

  expect_identical(period_type_labels(plot), c("Calculation", "Display"))
})
