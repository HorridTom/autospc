# The Xbar chart on its own. The analysis is checked against Provost and
# Murray's formulae, pp. 193-194, worked out in the test from the observations.

xbar_sizes <- c(
  5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 8, 3, 1, 6, 5, 7, 2, 5, 5, 5, 5,
  4, 9, 5, 1, 6, 3, 5, 7, 5
)

xbar_data <- function(seed = 42) {
  set.seed(seed)

  data.frame(
    x = rep(seq_along(xbar_sizes), xbar_sizes),
    y = stats::rnorm(sum(xbar_sizes), 50, 5)
  )
}

# the chart analysed as one baseline of 21 subgroups and nine display rows
analysed_xbar <- function(data = xbar_data(), ...) {
  chart <- autospc_chart_xbar(
    data = data, x = "x", y = "y",
    period_min = 21L, baseline_only = TRUE, ...
  )

  return(analyse_charts(list(chart))[[1]]$result$table)
}

# Provost and Murray's figures for the first 21 subgroups, from the observations
by_hand <- function(data, excluded = integer(0)) {
  means <- tapply(data$y, data$x, mean)
  sds <- tapply(data$y, data$x, stats::sd)
  sizes <- as.numeric(table(data$x))

  counted <- setdiff(1:21, excluded)
  with_s <- counted[sizes[counted] >= 2]

  list(
    cl = sum(sizes[counted] * means[counted]) / sum(sizes[counted]),
    sbar = sum(sizes[with_s] * sds[with_s]) / sum(sizes[with_s]),
    sizes = sizes
  )
}

a3 <- function(n) {
  c4 <- sqrt(2 / (n - 1)) * exp(lgamma(n / 2) - lgamma((n - 1) / 2))

  return(3 / (c4 * sqrt(n)))
}


test_that("autospc_chart_xbar returns an object of the expected class", {
  chart <- autospc_chart_xbar(data = xbar_data(), x = "x", y = "y")

  expect_identical(class(chart), c("autospc_chart_xbar", "autospc_chart"))
  expect_true(all(c(autospc_chart_elements(), "n", "s") %in% names(chart)))
})


test_that("an Xbar chart accepts a repeated x", {
  expect_no_error(autospc_chart_xbar(data = xbar_data(), x = "x", y = "y"))
})


test_that("one row per subgroup needs both n and s", {
  summarised <- data.frame(x = 1:3, y = c(10, 11, 12), n = c(4, 5, 6))

  expect_error(
    autospc_chart_xbar(data = summarised, x = "x", y = "y", n = "n"),
    "n and s must both be specified"
  )
})


test_that("a negative subgroup size or standard deviation is refused", {
  summarised <- data.frame(
    x = 1:3, y = c(10, 11, 12), n = c(4, -5, 6), s = c(1, 2, 3)
  )

  expect_error(
    autospc_chart_xbar(data = summarised, x = "x", y = "y"),
    "cannot be negative"
  )
})


test_that("y must be numeric", {
  expect_error(
    autospc_chart_xbar(
      data = data.frame(x = 1:3, y = c("a", "b", "c")),
      x = "x", y = "y"
    ),
    "y must be of type integer or double"
  )
})


test_that("the analysis table carries n, s and sbar", {
  expect_identical(
    names(analysed_xbar()),
    analysis_table_columns(autospc_chart_xbar(
      data = xbar_data(), x = "x", y = "y"
    ))
  )

  expect_true(all(c("y", "n", "s", "sbar") %in% names(analysed_xbar())))
})


test_that("the centre line and limits are Provost and Murray's", {
  table <- analysed_xbar()
  expected <- by_hand(xbar_data(), excluded = which(table$excluded))

  expect_equal(table$cl, rep(expected$cl, 30))
  expect_equal(table$sbar, rep(expected$sbar, 30))

  with_limits <- expected$sizes >= 2

  expect_equal(
    table$ucl[with_limits],
    (expected$cl + a3(expected$sizes) * expected$sbar)[with_limits]
  )
  expect_equal(
    table$lcl[with_limits],
    (expected$cl - a3(expected$sizes) * expected$sbar)[with_limits]
  )
})


test_that("a subgroup of one is plotted with a gap in its limits", {
  table <- analysed_xbar()

  # subgroup 13 in the calculation period and 25 in the display rows
  expect_false(is.na(table$series[13]))
  expect_false(is.na(table$series[25]))
  expect_true(all(is.na(c(table$ucl[c(13, 25)], table$lcl[c(13, 25)]))))
  expect_false(anyNA(table$cl))
})


test_that("the display rows' limits are formed at each row's own size", {
  table <- analysed_xbar()
  display <- which(table$period_type == "display" & xbar_sizes >= 2)

  expect_equal(
    table$ucl[display],
    table$cl[display] + a3(xbar_sizes[display]) * table$sbar[display]
  )
})


test_that("the extension's limits sit at the final period's mean size", {
  data <- xbar_data()
  chart <- autospc_chart_xbar(
    data = data, x = "x", y = "y",
    period_min = 21L, baseline_only = TRUE, extend_limits_to = 35L
  )
  table <- analyse_charts(list(chart))[[1]]$result$table

  extension <- table[table$limit_extension, ]
  final <- table[table$period_type %in% "calculation" &
    !table$limit_extension, ]
  mean_n <- mean(final$n[!final$excluded])

  expect_equal(
    extension$ucl,
    rep(final$cl[1] + a3(mean_n) * final$sbar[1], nrow(extension))
  )
  expect_identical(extension$sbar, rep(final$sbar[1], nrow(extension)))
})


test_that("a subgroup with no observation is a gap with limits at its size", {
  data <- xbar_data()
  data$y[data$x == 23] <- NA

  table <- analysed_xbar(data)

  expect_true(is.na(table$series[23]))
  expect_identical(table$n[23], 9L)
  expect_equal(table$ucl[23], table$cl[23] + a3(9) * table$sbar[23])
})


test_that("the same data one row per subgroup or split across rows agrees", {
  data <- xbar_data()

  summarise_rows <- function(rows) {
    data.frame(
      x = rows$x[1],
      y = mean(rows$y),
      n = nrow(rows),
      s = if (nrow(rows) > 1) stats::sd(rows$y) else NA_real_
    )
  }

  one_row <- do.call(rbind, lapply(split(data, data$x), summarise_rows))

  # each subgroup split into chunks of up to three observations
  data$chunk <- stats::ave(data$y, data$x, FUN = function(v) {
    (seq_along(v) - 1) %/% 3
  })
  split_rows <- do.call(rbind, lapply(
    split(data, list(data$x, data$chunk), drop = TRUE),
    summarise_rows
  ))

  from_observations <- analysed_xbar(xbar_data())
  from_one_row <- analysed_xbar(one_row, n = "n", s = "s")
  from_split_rows <- analysed_xbar(split_rows, n = "n", s = "s")

  compared <- c("series", "n", "s", "cl", "ucl", "lcl", "sbar")

  expect_equal(from_one_row[compared], from_observations[compared])
  expect_equal(from_split_rows[compared], from_observations[compared])
  expect_gt(nrow(split_rows), nrow(one_row))
})


test_that("the published A3 is used where the option asks for it", {
  previous <- options(autospc.rounded_constants = TRUE)
  on.exit(options(previous))

  table <- analysed_xbar()
  fives <- which(xbar_sizes == 5)

  expect_equal(
    table$ucl[fives],
    table$cl[fives] + 1.427 * table$sbar[fives]
  )
})


test_that("the Xbar chart names and titles itself", {
  chart <- autospc_chart_xbar(data = xbar_data(), x = "x", y = "y")

  expect_identical(chart_type_label(chart), "Xbar")
  expect_identical(y_axis_title(chart), "Xbar")
  expect_identical(period_statistics_columns(chart), c("sd_estimate", "sbar"))
})
