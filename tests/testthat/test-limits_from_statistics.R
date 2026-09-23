# The standard deviation estimate on the analysis table, and the limits formed
# from it.

statistics_series <- function() {
  set.seed(4)

  return(data.frame(x = 1:40, y = as.integer(stats::rpois(40, 12))))
}


analysed <- function(chart_type, ...) {
  return(suppressWarnings(autospc(statistics_series(),
    chart_type = chart_type, x = x, y = y, period_min = 21L,
    plot_chart = FALSE, ...
  )))
}


test_that("every chart type returns an estimate on every row", {
  for (chart_type in c("C", "C'", "X", "MR")) {
    table <- analysed(chart_type)

    expect_true("sd_estimate" %in% colnames(table), label = chart_type)
    expect_false(anyNA(table$sd_estimate), label = chart_type)
  }
})


test_that("the display rows take the estimate of the period they follow", {
  table <- analysed("C")

  calculated <- table$period_type == "calculation"
  displayed <- table$period_type == "display"

  # the classes whose limits do not vary with a denominator carry the estimate
  # forward, so one period holds one value
  expect_gt(sum(displayed), 0)
  expect_identical(
    unique(table$sd_estimate[displayed]),
    unique(table$sd_estimate[calculated])
  )
})


test_that("the limits sit three standard errors either side of the centre", {
  for (chart_type in c("C", "C'", "X")) {
    table <- analysed(chart_type)

    expect_equal(table$ucl, table$cl + 3 * table$sd_estimate,
      label = chart_type
    )
    expect_equal(table$lcl, table$cl - 3 * table$sd_estimate,
      label = chart_type
    )
  }
})


test_that("a proportion chart's estimate is free of the denominator", {
  set.seed(6)

  denominators <- as.integer(sample(c(20L, 60L, 200L), 40L, replace = TRUE))
  data <- data.frame(
    x = 1:40,
    n = denominators,
    y = as.integer(stats::rbinom(40L, denominators, 0.4))
  )

  table <- suppressWarnings(autospc(data,
    chart_type = "P", x = x, y = y, n = n, period_min = 21L,
    plot_chart = FALSE
  ))

  # one estimate for the period, and the limits placed at each row's own
  # denominator
  expect_length(unique(table$sd_estimate), 1L)
  expect_gt(length(unique(table$n)), 1L)
  expect_equal(table$ucl, table$cl + 3 * table$sd_estimate / sqrt(table$n))
})


test_that("extension rows are formed from the estimate the table carries", {
  set.seed(6)

  denominators <- as.integer(sample(c(20L, 60L, 200L), 40L, replace = TRUE))
  data <- data.frame(
    x = 1:40,
    n = denominators,
    y = as.integer(stats::rbinom(40L, denominators, 0.4))
  )
  data$y[9] <- data$n[9]

  for (chart_type in c("P", "P'")) {
    table <- suppressWarnings(autospc(data,
      chart_type = chart_type, x = x, y = y, n = n, period_min = 21L,
      extend_limits_to = 50L, plot_chart = FALSE
    ))

    calculated <- table[table$period_type == "calculation", ]
    final <- calculated[
      calculated$plot_period == calculated$plot_period[nrow(calculated)],
    ]
    extension <- table[table$limit_extension, ]

    # the extension carries one pair of limits, placed at the mean denominator
    # of the observations of the period it is carried from, leaving out the
    # excluded points
    counted <- !is.na(final$series) & final$excluded %in% FALSE
    implied <- (extension$ucl[1] - extension$cl[1]) *
      sqrt(mean(final$n[counted])) / 3

    expect_equal(implied, extension$sd_estimate[1], label = chart_type)
    expect_equal(extension$cl[1], final$cl[1], label = chart_type)
  }
})
