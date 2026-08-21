pair_data <- data.frame(
  x = 1:40,
  y = c(10, 12, 11, 13, 9, 14, 10, 11, 12, 13,
        10, 12, 11, 13, 9, 14, 10, 11, 12, 13,
        30, 32, 31, 33, 29, 34, 30, 31, 32, 33,
        30, 32, 31, 33, 29, 34, 30, 31, 32, 33)
)

pair_titled <- pair_data
pair_titled$title <- "From the data"
pair_titled$subtitle <- "Also from the data"

run_pair <- function(data = pair_data, ...) {

  suppressWarnings(
    autospc(data, chart_type = "XMR", period_min = 21L, ...)
  )

}


test_that("an XMR chart draws", {

  expect_no_error(drawn(run_pair()))

})


test_that("both panels are drawn", {

  expect_setequal(intersect(panel_texts(run_pair()), c("X", "MR")),
                  c("X", "MR"))

})


# the title belongs to the pair, not to either panel


test_that("a title from the data is rendered once, not on both panels", {

  texts <- panel_texts(run_pair(pair_titled))

  expect_identical(sum(texts == "From the data"), 1L)

  expect_identical(sum(texts == "Also from the data"), 1L)

})


test_that("a title passed as an argument is rendered once", {

  texts <- panel_texts(run_pair(title = "Passed in", subtitle = "And this"))

  expect_identical(sum(texts == "Passed in"), 1L)

  expect_identical(sum(texts == "And this"), 1L)

})


test_that("a title passed as an argument wins over one in the data", {

  texts <- panel_texts(run_pair(pair_titled,
                                title = "Passed in",
                                subtitle = "And this"))

  expect_identical(sum(texts == "Passed in"), 1L)

  expect_false(any(texts == "From the data"))

  expect_false(any(texts == "Also from the data"))

})


# the moving range analysis


test_that("the MR chart of the pair is the one a standalone MR run gives", {

  pair_mr <- autospc_plot_charts(run_pair())[[2]]

  alone <- suppressWarnings(
    autospc(pair_data, chart_type = "MR", period_min = 21L)
  )

  expect_equal(pair_mr$result$table,
               autospc_plot_charts(alone)[[1]]$result$table)

})


test_that("the moving range columns are joined onto the table output", {

  wide <- run_pair(plot_chart = FALSE)

  expect_true(all(c("mr", "amr", "url", "lrl") %in% colnames(wide)))

  expect_identical(nrow(wide), 40L)

})


test_that("XMR survives being called from a wrapper that forwards ...", {

  wrapper <- function(...) autospc(..., plot_chart = FALSE)

  expect_no_error(
    suppressWarnings(
      wrapper(pair_data, chart_type = "XMR", x = x, y = y, period_min = 21L)
    )
  )

})


test_that("the caption names the pair, not the X chart", {

  # the pair is a cowplot grid, so the caption is inside it rather than on it
  texts <- panel_texts(run_pair())

  expect_true(any(grepl("XMR Shewhart Chart", texts, fixed = TRUE)))

})
