# The S chart on its own, checked against Provost and Murray's formulae,
# pp. 193-194, worked out in the test from the observations.

s_sizes <- c(
  5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 8, 3, 1, 6, 5, 7, 2, 5, 5, 5, 5, 5,
  4, 9, 5, 1, 6, 3, 5, 7, 5
)

s_data <- function(seed = 7) {
  set.seed(seed)

  data.frame(
    x = rep(seq_along(s_sizes), s_sizes),
    y = stats::rnorm(sum(s_sizes), 50, 5)
  )
}

# 21 subgroups with a standard deviation make the baseline, which is the first
# 22 subgroups because subgroup 13 has none
analysed_s <- function(data = s_data(), ...) {
  chart <- autospc_chart_s(
    data = data, x = "x", y = "y",
    period_min = 21L, baseline_only = TRUE, ...
  )

  return(analyse_charts(list(chart))[[1]]$result$table)
}

b_constants <- function(n) {
  c4 <- sqrt(2 / (n - 1)) * exp(lgamma(n / 2) - lgamma((n - 1) / 2))
  k <- 3 * sqrt(1 - c4^2) / c4

  return(list(b3 = pmax(0, 1 - k), b4 = 1 + k))
}


test_that("autospc_chart_s returns an object of the expected class", {
  chart <- autospc_chart_s(data = s_data(), x = "x", y = "y")

  expect_identical(class(chart), c("autospc_chart_s", "autospc_chart"))
  expect_true(all(c(autospc_chart_elements(), "n", "s") %in% names(chart)))
})


test_that("the S chart analyses the subgroup standard deviations", {
  table <- analysed_s()

  expect_equal(
    table$series,
    as.numeric(tapply(s_data()$y, s_data()$x, stats::sd))
  )
})


test_that("the centre line is sbar and the limits B3 and B4 times it", {
  table <- analysed_s()
  data <- s_data()

  sds <- tapply(data$y, data$x, stats::sd)
  counted <- setdiff(which(s_sizes[1:22] >= 2), which(table$excluded))
  sbar <- sum(s_sizes[counted] * sds[counted]) / sum(s_sizes[counted])

  with_s <- s_sizes >= 2
  b <- b_constants(s_sizes)

  expect_equal(table$cl, rep(sbar, 31))
  expect_equal(table$ucl[with_s], (b$b4 * sbar)[with_s])
  expect_equal(table$lcl[with_s], (b$b3 * sbar)[with_s])
})


test_that("the lower limit is zero below six and above zero from six", {
  table <- analysed_s()

  expect_true(all(table$lcl[s_sizes >= 2 & s_sizes < 6] == 0))
  expect_true(all(table$lcl[s_sizes >= 6] > 0))
})


test_that("the lower limit is zero below six even with limits unconstrained", {
  previous <- options(autospc.constrain_limits = FALSE)
  on.exit(options(previous))

  table <- analysed_s()

  expect_true(all(table$lcl[s_sizes >= 2 & s_sizes < 6] == 0))
})


test_that("a subgroup of one is a gap on the S chart", {
  table <- analysed_s()

  expect_true(all(is.na(table$series[c(13, 26)])))
})


test_that("the display rows' limits are formed at each row's own size", {
  table <- analysed_s()
  display <- which(table$period_type == "display" & s_sizes >= 2)

  expect_equal(
    table$ucl[display],
    b_constants(s_sizes[display])$b4 * table$cl[display]
  )
})


test_that("the published B4 is used where the option asks for it", {
  previous <- options(autospc.rounded_constants = TRUE)
  on.exit(options(previous))

  table <- analysed_s()
  fives <- which(s_sizes == 5)

  expect_equal(table$ucl[fives], 2.089 * table$cl[fives])
})


test_that("the S chart names the columns it adds to its pair", {
  chart <- autospc_chart_s(data = s_data(), x = "x", y = "y")

  expect_identical(
    paired_columns(chart),
    c(subgroup_s = "series", scl = "cl", s_ucl = "ucl", s_lcl = "lcl")
  )
})


test_that("the S chart names and titles itself", {
  chart <- autospc_chart_s(data = s_data(), x = "x", y = "y")

  expect_identical(chart_type_label(chart), "S")
  expect_identical(y_axis_title(chart), "S")
  expect_true(labels_stay_above(chart))
})
