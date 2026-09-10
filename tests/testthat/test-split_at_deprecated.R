# facet_stages(split_rows) is deprecated in favour of facet_stages(split_at),
# which counts points in the analysed series rather than rows of the data as
# supplied. The old name takes the new meaning.

split_at_data <- data.frame(
  x = 1:30,
  y = rep(c(10L, 12L, 11L, 13L, 9L, 14L), 5L)
)


facet_at <- function(...) {
  return(facet_stages(split_at_data,
    chart_type = "C",
    period_min = 5L,
    plot_chart = FALSE,
    ...
  ))
}


test_that("supplying split_rows warns that it is deprecated", {
  lifecycle::expect_deprecated(facet_at(split_rows = 12L), "split_rows")
})


test_that("split_rows is taken as split_at", {
  expect_equal(
    suppressWarnings(facet_at(split_rows = 12L)),
    facet_at(split_at = 12L)
  )
})


test_that("split_at wins where both are given", {
  both <- suppressWarnings(facet_at(split_at = 12L, split_rows = 20L))

  expect_identical(as.integer(table(both$stage)), c(12L, 30L))
})


test_that("not supplying split_rows is silent", {
  expect_no_warning(facet_at(split_at = 12L))
})
