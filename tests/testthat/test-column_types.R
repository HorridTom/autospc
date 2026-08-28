# The analysed columns keep the type they were supplied with. Nothing in the
# analysis coerces them: the limits are doubles because they are means and
# standard-deviation-based bounds, whatever the counts they were computed from.

integer_counts <- data.frame(
  x = 1:30,
  y = rep(c(10L, 12L, 11L, 13L, 9L, 14L), 5L),
  n = rep(100L, 30)
)

double_counts <- data.frame(
  x = 1:30,
  y = as.double(rep(c(10, 12, 11, 13, 9, 14), 5L)),
  n = as.double(rep(100, 30))
)

analysed <- function(data, chart_type) {
  suppressWarnings(
    autospc(data,
      chart_type = chart_type, period_min = 21L,
      plot_chart = FALSE
    )
  )
}


test_that("an integer y comes back as an integer", {
  for (chart_type in c("C", "C'", "X")) {
    expect_identical(typeof(analysed(integer_counts, chart_type)$y),
      "integer",
      info = chart_type
    )
  }
})


test_that("a double y comes back as a double", {
  for (chart_type in c("C", "C'", "X")) {
    expect_identical(typeof(analysed(double_counts, chart_type)$y),
      "double",
      info = chart_type
    )
  }
})


test_that("a proportion chart keeps the type of its denominator", {
  for (chart_type in c("P", "P'")) {
    expect_identical(typeof(analysed(integer_counts, chart_type)$n),
      "integer",
      info = chart_type
    )

    expect_identical(typeof(analysed(double_counts, chart_type)$n),
      "double",
      info = chart_type
    )
  }
})


test_that("the limits are doubles whatever the counts were", {
  for (chart_type in c("C", "C'", "X", "MR", "P", "P'")) {
    result <- analysed(integer_counts, chart_type)

    expect_identical(typeof(result$cl), "double", info = chart_type)
    expect_identical(typeof(result$ucl), "double", info = chart_type)
    expect_identical(typeof(result$lcl), "double", info = chart_type)
  }
})


test_that("counts too large to hold in an integer sum are analysed correctly", {
  # sum() over integers has returned a double rather than overflowing since
  # R 3.5.0, so nothing needs to coerce the counts first
  large <- data.frame(
    x = 1:30,
    y = rep(200000000L, 30),
    n = rep(500000000L, 30)
  )

  result <- analysed(large, "P")

  expect_false(any(is.na(result$cl)))

  expect_equal(result$cl[1], 40)
})
