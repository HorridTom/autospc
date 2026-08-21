# Every data frame the package produces is a plain data frame, whatever the
# caller passed and whatever the chart type. data_original is the exception: it
# is the data as the caller passed it, class included.

return_class_data <- data.frame(x = 1:30,
                                y = rep(c(10L, 12L, 11L, 13L, 9L, 14L), 5L),
                                n = rep(100L, 30))

return_class_tibble <- tibble::as_tibble(return_class_data)

analysed_frame <- function(data, chart_type) {

  suppressWarnings(
    autospc(data, chart_type = chart_type, period_min = 21L,
            plot_chart = FALSE)
  )

}


test_that("autospc returns a plain data frame for every chart type", {

  for(chart_type in c("C", "C'", "P", "P'", "X", "MR", "XMR")) {

    expect_identical(class(analysed_frame(return_class_data, chart_type)),
                     "data.frame",
                     info = chart_type)

  }

})


test_that("a tibble in does not make a tibble out", {

  for(chart_type in c("C", "C'", "P", "P'", "X", "MR", "XMR")) {

    expect_identical(class(analysed_frame(return_class_tibble, chart_type)),
                     "data.frame",
                     info = chart_type)

  }

})


test_that("the rows are numbered from one", {

  result <- analysed_frame(return_class_tibble, "C")

  expect_identical(rownames(result),
                   as.character(seq_len(nrow(result))))

})


test_that("facet_stages returns a plain data frame", {

  result <- suppressWarnings(
    facet_stages(return_class_tibble, split_rows = c(15L, 30L),
                 chart_type = "C", period_min = 21L, plot_chart = FALSE)
  )

  expect_identical(class(result), "data.frame")

})


test_that("the frames carried by a chart are plain data frames", {

  chart <- autospc_plot_charts(suppressWarnings(
    autospc(return_class_tibble, chart_type = "C'", period_min = 21L,
            keep_candidate_tables = TRUE)
  ))[[1]]

  expect_identical(class(chart$data), "data.frame")

  expect_identical(class(chart$result$table), "data.frame")

  expect_identical(class(chart$history$counter_path), "data.frame")

})


test_that("the tables recorded for each candidate are plain data frames", {

  chart <- autospc_chart(chart_type = "C'",
                         data = tibble::as_tibble(example_series_2a),
                         x = "x",
                         y = "y",
                         keep_candidate_tables = TRUE)

  chart <- run_limit_algorithm(prepare_data(order_series(aggregate_data(chart))))

  expect_gt(length(chart$history$candidates), 0L)

  expect_identical(class(chart$history$breaks), "data.frame")

  for(candidate in chart$history$candidates) {

    expect_identical(class(candidate$table), "data.frame")

  }

})


test_that("data_original keeps the class the caller passed", {

  from_tibble <- autospc_chart(chart_type = "C", data = return_class_tibble,
                               x = "x", y = "y")

  expect_s3_class(from_tibble$data_original, "tbl_df")

  from_data_frame <- autospc_chart(chart_type = "C", data = return_class_data,
                                   x = "x", y = "y")

  expect_identical(class(from_data_frame$data_original), "data.frame")

})
