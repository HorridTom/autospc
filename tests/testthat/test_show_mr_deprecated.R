show_mr_data <- data.frame(
  x = 1:30,
  y = c(50, 48, 49, 50, 52, 52, 48, 53, 51, 51,
        47, 52, 51, 47, 48, 49, 51, 51, 45, 49,
        49, 50, 48, 53, 49, 48, 51, 46, 48, 49)
)

run_show_mr <- function(...) {

  autospc(show_mr_data,
          chart_type = "XMR",
          period_min = 21L,
          ...)

}


test_that("show_mr = FALSE warns that it is deprecated", {

  lifecycle::expect_deprecated(run_show_mr(show_mr = FALSE, plot_chart = FALSE))

})


test_that("show_mr = TRUE warns too - the argument is deprecated, not a value", {

  lifecycle::expect_deprecated(run_show_mr(show_mr = TRUE, plot_chart = FALSE))

})


test_that("the warning names the argument that replaces it", {

  expect_warning(run_show_mr(show_mr = FALSE, plot_chart = FALSE),
                 "chart_type")

})


test_that("not supplying it does not warn", {

  # deprecated() is a sentinel, not a value, so is_present() has to distinguish
  # "argument absent" from "argument supplied"
  expect_no_warning(run_show_mr(plot_chart = FALSE))

})


test_that("not supplying it still draws the pair", {

  charts <- autospc_plot_charts(run_show_mr())

  expect_length(charts, 2L)

})


# what the deprecated argument still does, until CLEAN UP #12 removes it


test_that("show_mr = FALSE carries the X chart alone", {

  charts <- suppressWarnings(
    autospc_plot_charts(run_show_mr(show_mr = FALSE))
  )

  expect_length(charts, 1L)

  expect_s3_class(charts[[1]], "autospc_chart_x")

})


test_that("show_mr = TRUE carries the pair, as leaving it out does", {

  charts <- suppressWarnings(
    autospc_plot_charts(run_show_mr(show_mr = TRUE))
  )

  expect_length(charts, 2L)

})


test_that("show_mr = FALSE does not change a chart type that is not XMR", {

  # show_mr only ever chose whether the moving range chart of a requested pair
  # was drawn, so it says nothing about any other chart type
  charts <- suppressWarnings(
    autospc_plot_charts(autospc(show_mr_data,
                                chart_type = "C",
                                period_min = 21L,
                                show_mr = FALSE))
  )

  expect_length(charts, 1L)

  expect_s3_class(charts[[1]], "autospc_chart_c")

})


test_that("show_mr = FALSE draws one panel, not a pair", {

  plot <- suppressWarnings(run_show_mr(show_mr = FALSE))

  expect_error(panel_texts(plot), "only one panel")

  expect_identical(plot$labels$y, "X")

})


test_that("show_mr = FALSE leaves the moving range columns off the table", {

  wide <- suppressWarnings(run_show_mr(show_mr = FALSE, plot_chart = FALSE))

  expect_false(any(c("mr", "amr", "url", "lrl") %in% colnames(wide)))

})


# facet_stages() has never drawn the moving range chart


facet_data <- data.frame(x = 1:60,
                         y = rep(c(50, 48, 49, 51, 52, 47), 10L))

facet_show_mr <- function(...) {

  facet_stages(facet_data,
               split_rows = c(30L, 60L),
               chart_type = "XMR",
               period_min = 21L,
               ...)

}


test_that("facet_stages warns that show_mr is deprecated", {

  lifecycle::expect_deprecated(facet_show_mr(show_mr = FALSE))

})


test_that("facet_stages still refuses show_mr = TRUE in its own words", {

  expect_warning(
    suppressWarnings(facet_show_mr(show_mr = TRUE),
                     classes = "lifecycle_warning_deprecated"),
    "does not support"
  )

})


test_that("facet_stages warns about show_mr once, not once per stage", {

  # facet_stages() drops show_mr before calling autospc(), which would
  # otherwise warn again for every stage it runs
  count <- 0L

  suppressWarnings(
    withCallingHandlers(
      facet_show_mr(show_mr = FALSE),
      lifecycle_warning_deprecated = function(w) count <<- count + 1L
    )
  )

  expect_identical(count, 1L)

})


test_that("facet_stages does not warn when show_mr is left out", {

  expect_no_warning(facet_show_mr())

})


test_that("faceting an XMR request draws the same chart as asking for X", {

  expect_equal(suppressWarnings(facet_show_mr(show_mr = FALSE))$data,
               facet_stages(facet_data,
                            split_rows = c(30L, 60L),
                            chart_type = "X",
                            period_min = 21L)$data)

})
