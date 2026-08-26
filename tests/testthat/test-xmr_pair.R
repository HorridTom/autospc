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


test_that("the charts of a pair are named by the part they play", {

  expect_named(autospc_plot_charts(run_pair()), c("location", "dispersion"))

})


test_that("an XMR chart draws", {

  expect_no_error(drawn(run_pair()))

})


test_that("a pair with too few points for limits carries the X chart alone", {

  one_short <- data.frame(x = 1:20, y = rep(c(4, 7, 5, 6), 5))

  plot <- suppressWarnings(autospc(one_short, chart_type = "XMR",
                                   period_min = 21L))

  expect_length(autospc_plot_charts(plot), 1L)

  expect_s3_class(autospc_plot_charts(plot)[[1]], "autospc_chart_x")

})


test_that("both halves of a pair are short of points together", {

  # the two halves need the same number of points to form a period, which is
  # what makes one "fewer than the minimum" warning for the call right
  just_enough <- data.frame(x = 1:21, y = rep(c(4, 7, 5), 7))
  one_short   <- data.frame(x = 1:20, y = rep(c(4, 7, 5, 6), 5))

  limits_of <- function(data) {

    charts <- autospc_plot_charts(
      suppressWarnings(autospc(data, chart_type = "XMR", period_min = 21L))
    )

    vapply(charts,
           function(chart) centre_line_present(chart$result$table),
           logical(1L))

  }

  expect_identical(limits_of(just_enough), c(location = TRUE,
                                             dispersion = TRUE))

  # a pair with no limits carries the X chart alone
  expect_identical(limits_of(one_short), c(location = FALSE))

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
