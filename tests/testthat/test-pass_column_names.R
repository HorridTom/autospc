test_data <- readRDS("testdata/test_data.rds")

test_that("Renaming columns doesn't change the result", {
  test_data1 <- test_data %>%
    dplyr::select(x, y)

  test_data2 <- test_data %>%
    dplyr::select(month = x, count = y)

  result1 <- autospc(test_data1,
    chart_type = "C'",
    plot_chart = FALSE
  )

  result2 <- autospc(test_data2,
    x = month,
    y = count,
    chart_type = "C'",
    plot_chart = FALSE
  )

  testthat::expect_equal(result1, result2)
})


# a column already named x, y or n, and a different one named as that field


clashing_data <- data.frame(
  x = rep("not the x column", 30L),
  month = 1:30,
  count = rep(c(10L, 12L, 11L, 13L, 9L, 14L), 5L)
)


test_that("a column already named x does not stop another being named as x", {
  result <- suppressWarnings(
    autospc(clashing_data,
      chart_type = "C", x = month, y = count,
      period_min = 21L, plot_chart = FALSE
    )
  )

  expect_identical(result$x, clashing_data$month)
})


test_that("facet_stages settles a clash the same way", {
  result <- suppressWarnings(
    facet_stages(clashing_data,
      split_rows = c(15L, 30L), chart_type = "C",
      x = month, y = count, period_min = 21L, plot_chart = FALSE
    )
  )

  expect_identical(unique(result$x), clashing_data$month)
})
