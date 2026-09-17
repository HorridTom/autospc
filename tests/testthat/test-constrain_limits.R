# Constraining control limits to the range the plotted statistic can take, the
# option that turns the constraining off, and the axis that follows either way.

# a percentage series high enough that three standard errors reach above 100
high_p_data <- function() {
  set.seed(11)

  n <- 40L

  return(data.frame(
    x = 1:n,
    n = rep(8L, n),
    y = as.integer(stats::rbinom(n, 8, 0.95))
  ))
}


# a count series low enough that three standard deviations reach below zero
low_count_data <- function() {
  set.seed(3)

  return(data.frame(x = 1:40, y = as.integer(stats::rpois(40, 1))))
}


# the shape the proportion class constructors take
proportion_data <- data.frame(
  x = 1:5,
  y = c(3, 4, 2, 5, 3),
  n = rep(20L, 5)
)


p_chart <- function(...) {
  return(suppressWarnings(autospc(high_p_data(),
    chart_type = "P", x = x, y = y, n = n, period_min = 21L, ...
  )))
}


count_chart <- function(chart_type) {
  return(suppressWarnings(autospc(low_count_data(),
    chart_type = chart_type, x = x, y = y, period_min = 21L,
    plot_chart = FALSE
  )))
}


test_that("limits are constrained unless the option says otherwise", {
  expect_true(limits_constrained())

  previous <- options(autospc.constrain_limits = FALSE)
  on.exit(options(previous))

  expect_false(limits_constrained())
})


test_that("a value other than FALSE leaves the constraining in place", {
  previous <- options(autospc.constrain_limits = "banana")
  on.exit(options(previous))

  expect_true(limits_constrained())
})


test_that("every chart class states the range its statistic can take", {
  bounds <- function(chart) {
    return(unlist(limit_bounds(chart)))
  }

  count <- c(low = 0, high = Inf)
  percentage <- c(low = 0, high = 100)

  expect_identical(
    bounds(autospc_chart_c(data = test_data, x = "x", y = "y")), count
  )
  expect_identical(
    bounds(autospc_chart_cp(data = test_data, x = "x", y = "y")), count
  )
  expect_identical(
    bounds(autospc_chart_mr(data = test_data, x = "x", y = "y")), count
  )
  expect_identical(
    bounds(autospc_chart_p(
      data = proportion_data, x = "x", y = "y", n = "n"
    )),
    percentage
  )
  expect_identical(
    bounds(autospc_chart_pp(
      data = proportion_data, x = "x", y = "y", n = "n"
    )),
    percentage
  )
  expect_identical(
    bounds(autospc_chart_x(data = unique_x_data, x = "x", y = "y")),
    c(low = -Inf, high = Inf)
  )
})


test_that("an infinite or missing limit is left alone", {
  # get_p_limits() returns infinite limits for a denominator of zero, and an
  # infinite limit means the limit is unknown rather than at the bound
  constrained <- constrain_limits(
    limits = list(ucl = c(Inf, 120, NA), lcl = c(-Inf, -20, NA)),
    bounds = list(low = 0, high = 100)
  )

  expect_identical(constrained$ucl, c(Inf, 100, NA))
  expect_identical(constrained$lcl, c(-Inf, 0, NA))
})


test_that("a percentage chart's upper limit is constrained to 100 on every row", {
  table <- p_chart(extend_limits_to = 50L, plot_chart = FALSE)

  # the three kinds of row reach their limits by three different routes, so all
  # three have to be present for this to mean anything
  expect_setequal(table$period_type, c("calculation", "display"))
  expect_true(any(table$limit_extension))

  expect_equal(max(table$ucl, na.rm = TRUE), 100)
})


test_that("unconstrained, the same upper limit appears on every row", {
  previous <- options(autospc.constrain_limits = FALSE)
  on.exit(options(previous))

  table <- p_chart(extend_limits_to = 50L, plot_chart = FALSE)

  by_row_kind <- c(
    max(table$ucl[table$period_type == "calculation"], na.rm = TRUE),
    max(table$ucl[table$period_type == "display" & !table$limit_extension],
      na.rm = TRUE
    ),
    max(table$ucl[table$limit_extension], na.rm = TRUE)
  )

  expect_gt(by_row_kind[1], 100)
  expect_equal(by_row_kind, rep(by_row_kind[1], 3))
})


test_that("a count chart's lower limit is constrained to zero", {
  expect_equal(min(count_chart("C")$lcl, na.rm = TRUE), 0)

  previous <- options(autospc.constrain_limits = FALSE)
  on.exit(options(previous))

  expect_lt(min(count_chart("C")$lcl, na.rm = TRUE), 0)
})


test_that("a moving range chart's lower limit is zero either way", {
  mr_lower <- function() {
    table <- count_chart("MR")

    return(unique(table$lcl[!is.na(table$lcl)]))
  }

  expect_identical(mr_lower(), 0)

  previous <- options(autospc.constrain_limits = FALSE)
  on.exit(options(previous))

  expect_identical(mr_lower(), 0)
})


test_that("an individuals chart's limits are the same either way", {
  limits <- count_chart("X")[c("ucl", "lcl")]

  # the series is a count, so an individuals chart of it has a lower limit
  # below zero that nothing constrains
  expect_lt(min(limits$lcl, na.rm = TRUE), 0)

  previous <- options(autospc.constrain_limits = FALSE)
  on.exit(options(previous))

  expect_identical(count_chart("X")[c("ucl", "lcl")], limits)
})


test_that("the y axis follows limits that reach outside 0 to 100", {
  axis_of <- function(plot) {
    return(ggplot2::layer_scales(plot)$y$get_limits())
  }

  expect_identical(axis_of(p_chart()), c(0, 110))

  previous <- options(autospc.constrain_limits = FALSE)
  on.exit(options(previous))

  table <- p_chart(plot_chart = FALSE)

  # a tenth above the highest of the limits and the points
  expect_equal(
    axis_of(p_chart())[2],
    max(table$ucl, table$series, na.rm = TRUE) * 1.1
  )
})


test_that("nothing the chart draws falls outside the axis", {
  dropped <- function(plot) {
    layers <- suppressWarnings(ggplot2::ggplot_build(plot))$data

    return(sum(vapply(layers, function(layer) {
      if (!"y" %in% names(layer)) {
        return(0L)
      }

      return(sum(is.na(layer$y)))
    }, integer(1))))
  }

  expect_identical(dropped(p_chart()), 0L)

  previous <- options(autospc.constrain_limits = FALSE)
  on.exit(options(previous))

  expect_identical(dropped(p_chart()), 0L)
})
