# autospc(write_table) is deprecated. The results are returned by
# autospc(plot_chart = FALSE) and by as.data.frame() on a chart, and writing
# them is left to the caller.

write_table_data <- data.frame(x = 1:30,
                               y = rep(c(10L, 12L, 11L, 13L, 9L, 14L), 5L))


test_that("supplying write_table warns that it is deprecated", {

  lifecycle::expect_deprecated(
    autospc(write_table_data, chart_type = "C", period_min = 21L,
            plot_chart = FALSE, write_table = TRUE),
    "write_table"
  )

})


test_that("write_table = FALSE warns as well, because the argument is going", {

  lifecycle::expect_deprecated(
    autospc(write_table_data, chart_type = "C", period_min = 21L,
            plot_chart = FALSE, write_table = FALSE),
    "write_table"
  )

})


test_that("not supplying write_table is silent", {

  expect_no_warning(
    autospc(write_table_data, chart_type = "C", period_min = 21L,
            plot_chart = FALSE)
  )

})


test_that("the results are returned rather than written to a file", {

  result <- suppressWarnings(
    autospc(write_table_data, chart_type = "C", period_min = 21L,
            plot_chart = FALSE, write_table = TRUE)
  )

  expect_s3_class(result, "data.frame")

  expect_identical(nrow(result), 30L)

})


test_that("no file is written", {

  written_before <- list.files(recursive = TRUE)

  suppressWarnings(
    autospc(write_table_data, chart_type = "C", period_min = 21L,
            plot_chart = FALSE, write_table = TRUE)
  )

  expect_identical(list.files(recursive = TRUE), written_before)

})
