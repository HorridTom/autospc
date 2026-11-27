# Points as variables_for_rules() gives them, built by hand: z is the distance
# from the centre line in standard errors, side is 1 above, -1 below and 0 on
# it, and stretch numbers the stretches of consecutive points within which
# patterns are looked for
points_of <- function(z = NULL,
                      series = NULL,
                      side = NULL,
                      stretch = NULL) {
  n <- max(length(z), length(series))
  if (is.null(z)) z <- rep(0, n)
  if (is.null(series)) series <- z
  if (is.null(side)) side <- as.integer(sign(z))
  if (is.null(stretch)) stretch <- rep(0L, n)

  return(data.frame(
    series = series,
    z = z,
    side = as.integer(side),
    stretch = as.integer(stretch)
  ))
}

breaks_of <- function(rule, ...) {
  return(rule_breaks(rule, points = points_of(...)))
}

# a C chart, whose standard error is its sd_estimate at every row
c_chart <- function() {
  return(autospc_chart(
    chart_type = "C",
    data = data.frame(x = 1:3, y = 1:3),
    x = "x", y = "y"
  ))
}


# zone rules


test_that("a zone rule marks the points in the zone of each window with k of m in it", {
  rule <- zone_rule(k = 2, m = 3, beyond = 2, side = "either")

  expect_identical(
    breaks_of(rule, z = c(0, 2.5, 0, -2.1, 0, 0, 2.5)),
    c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE)
  )
})


test_that("a same-side zone rule does not combine points on opposite sides", {
  z <- c(0.5, 2.5, 0.5, -2.1, -0.5, 2.2, 2.9)

  expect_identical(
    breaks_of(zone_rule(k = 2, m = 3, beyond = 2, side = "same"), z = z),
    c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
  )
  expect_identical(
    breaks_of(zone_rule(k = 2, m = 3, beyond = 2, side = "either"), z = z),
    c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE)
  )
})


test_that("a point exactly on the edge is within the zone's outer end and not beyond its inner end", {
  expect_identical(
    breaks_of(zone_rule(k = 1, m = 1, beyond = 2), z = c(2, 2.01)),
    c(FALSE, TRUE)
  )
  expect_identical(
    breaks_of(zone_rule(k = 1, m = 1, within = 1), z = c(1, 1.01)),
    c(TRUE, FALSE)
  )
})


test_that("a zone with no outer end includes points outside the limits", {
  expect_identical(
    breaks_of(zone_rule(k = 2, m = 2, beyond = 2, side = "same"),
      z = c(2.5, 4)
    ),
    c(TRUE, TRUE)
  )
})


test_that("a zone between two distances excludes points beyond the outer one", {
  expect_identical(
    breaks_of(zone_rule(k = 1, m = 1, beyond = 1, within = 2),
      z = c(0.5, 1.5, 2.5)
    ),
    c(FALSE, TRUE, FALSE)
  )
})


test_that("fifteen within one standard error marks all fifteen, including points on the centre line", {
  rule <- zone_rule(k = 15, m = 15, within = 1, side = "either")
  z <- c(rep(c(0.5, -0.5, 0), 5), 2)

  expect_identical(breaks_of(rule, z = z), c(rep(TRUE, 15), FALSE))
  expect_identical(breaks_of(rule, z = z[-1]), rep(FALSE, 15))
})


test_that("a window needs m points", {
  expect_identical(
    breaks_of(zone_rule(k = 2, m = 3, beyond = 2), z = c(2.5, 2.5)),
    c(FALSE, FALSE)
  )
})


test_that("a zone pattern does not continue from one stretch into the next", {
  rule <- zone_rule(k = 2, m = 3, beyond = 2)
  z <- c(0, 2.5, 2.5, 0)

  expect_identical(breaks_of(rule, z = z), c(FALSE, TRUE, TRUE, FALSE))
  expect_identical(
    breaks_of(rule, z = z, stretch = c(0, 0, 1, 1)),
    rep(FALSE, 4)
  )
})


test_that("a point outside the zone takes a place in a window without counting", {
  rule <- zone_rule(k = 2, m = 3, beyond = 0.25, side = "same")

  expect_identical(
    breaks_of(rule, z = c(0.5, 0, 0.5, 0, 0, 0.5)),
    c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE)
  )
})


test_that("a point on the zone's inner boundary takes no place in a window", {
  rule <- zone_rule(k = 2, m = 3, beyond = 2, side = "same")

  # the point at 2 is skipped, so the window is 2.5, 0.5 and 2.5
  expect_identical(
    breaks_of(rule, z = c(2.5, 2, 0.5, 2.5)),
    c(TRUE, FALSE, FALSE, TRUE)
  )
  # (0.1 + 0.2) / 0.15 is 2 but for rounding, and is on the boundary too
  expect_identical(
    breaks_of(rule, z = c(2.5, (0.1 + 0.2) / 0.15, 0.5, 2.5)),
    c(TRUE, FALSE, FALSE, TRUE)
  )
})


test_that("where the zone starts at the centre line, a point with no side takes no place in a window", {
  rule <- zone_rule(k = 8, m = 8, beyond = 0, side = "same")
  z <- c(rep(0.5, 4), 0, rep(0.5, 4))

  expect_identical(breaks_of(rule, z = z), c(rep(TRUE, 4), FALSE, rep(TRUE, 4)))
  expect_identical(breaks_of(rule, z = z[-1]), rep(FALSE, 8))
  # a point within centre_line_tolerance of the centre line has no side
  expect_identical(
    breaks_of(rule,
      z = c(rep(0.5, 4), 0.1, rep(0.5, 4)),
      side = c(rep(1, 4), 0, rep(1, 4))
    ),
    c(rep(TRUE, 4), FALSE, rep(TRUE, 4))
  )
})


test_that("a zone rule is not applied to a point with no limits, which ends a pattern", {
  rule <- zone_rule(k = 2, m = 3, beyond = 2)

  expect_identical(
    breaks_of(rule, z = c(2.5, NA, 2.5, 2.5, 0)),
    c(FALSE, NA, TRUE, TRUE, FALSE)
  )
})


test_that("a point with no limits takes no place in a window", {
  expect_identical(
    breaks_of(zone_rule(k = 2, m = 3, beyond = 2), z = c(NA, 2.5, 2.5)),
    c(NA, FALSE, FALSE)
  )
})


test_that("with no standard error a point on the centre line is 0 away and a point off it infinitely far", {
  expect_identical(
    distance_in_standard_errors(
      series = c(5, 6, 4, 5),
      cl = c(5, 5, 5, NA),
      standard_error = c(0, 0, 0, 1)
    ),
    c(0, Inf, -Inf, NA)
  )
})


test_that("a run rule as a zone rule marks what the shift column marks", {
  # 600 points with ties to the centre line, three tolerances, and gaps that
  # end a run at random
  set.seed(160)
  for (tolerance in c(0, 1, 2)) {
    table <- data.frame(
      series = round(stats::rnorm(600, mean = 10, sd = 2)),
      cl = 10,
      run_break = stats::runif(600) < 0.02
    )
    table$series[c(50, 51, 300)] <- NA
    table$cl[400:420] <- NA

    shifts <- add_shift_columns(table,
      centre_line_tolerance = tolerance,
      shift_rule_threshold = 4L
    )

    points <- data.frame(
      series = table$series,
      z = table$series - table$cl,
      side = shifts$above_or_below_cl,
      stretch = cumsum(table$run_break | is.na(table$series))
    )
    run_rule <- zone_rule(k = 4, m = 4, beyond = 0, side = "same")

    expect_identical(
      dplyr::coalesce(rule_breaks(run_rule, points = points), FALSE),
      dplyr::coalesce(shifts$shift, FALSE),
      info = tolerance
    )
    expect_gt(sum(shifts$shift, na.rm = TRUE), 10L)
  }
})


# trend rules


test_that("a trend rule marks k points each higher, or each lower, than the one before", {
  rule <- trend_rule(k = 6)

  expect_identical(breaks_of(rule, series = c(1:6, 3)), c(rep(TRUE, 6), FALSE))
  expect_identical(breaks_of(rule, series = c(6:1, 3)), c(rep(TRUE, 6), FALSE))
  expect_identical(breaks_of(rule, series = c(1:5, 3)), rep(FALSE, 6))
})


test_that("a point equal to the one before neither continues nor ends a trend", {
  rule <- trend_rule(k = 6)
  series <- c(1, 2, 3, 3, 4, 5, 6)

  expect_identical(breaks_of(rule, series = series), rep(TRUE, 7))
  expect_identical(
    breaks_of(trend_rule(k = 7), series = series),
    rep(FALSE, 7)
  )
})


test_that("a point equal to the last point of a trend is not marked", {
  expect_identical(
    breaks_of(trend_rule(k = 6), series = c(1:6, 6)),
    c(rep(TRUE, 6), FALSE)
  )
})


test_that("a trend up and the trend down after it share the point where they turn", {
  expect_identical(
    breaks_of(trend_rule(k = 6), series = c(1:6, 5:1, 3)),
    c(rep(TRUE, 11), FALSE)
  )
})


test_that("a trend does not continue from one stretch into the next", {
  expect_identical(
    breaks_of(trend_rule(k = 6), series = 1:6, stretch = c(0, 0, 0, 1, 1, 1)),
    rep(FALSE, 6)
  )
})


test_that("a point with no value is not part of a trend", {
  expect_identical(
    breaks_of(trend_rule(k = 3),
      series = c(1, 2, NA, 3, 4, 5),
      stretch = c(0, 0, 1, 1, 1, 1)
    ),
    c(FALSE, FALSE, NA, TRUE, TRUE, TRUE)
  )
})


# alternating rules


test_that("an alternating rule marks k points going up and down in turn", {
  series <- c(rep(c(1, 2), 7), 2)

  expect_identical(
    breaks_of(alternating_rule(k = 14), series = series),
    c(rep(TRUE, 14), FALSE)
  )
  expect_identical(
    breaks_of(alternating_rule(k = 14), series = series[-1]),
    rep(FALSE, 14)
  )
})


test_that("a point equal to the one before ends an alternation", {
  series <- c(1, 2, 1, 2, 2, 1, 2, 1)

  expect_identical(
    breaks_of(alternating_rule(k = 4), series = series),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  )
  expect_identical(
    breaks_of(alternating_rule(k = 5), series = series),
    rep(FALSE, 8)
  )
})


# applying a set to a period


test_that("the trend rule is applied to charts whose limits do not vary within a period", {
  expect_true(trend_rule_applies(c_chart(), table = NULL))

  for (chart_type in c("P", "P'", "U", "U'")) {
    chart <- autospc_chart(
      chart_type = chart_type,
      data = data.frame(x = 1:3, y = 1:3, n = 10),
      x = "x", y = "y", n = "n"
    )

    expect_false(trend_rule_applies(chart, table = NULL), info = chart_type)
  }
})


test_that("the trend rule is applied to Xbar and S charts only where every subgroup is the same size", {
  for (chart_type in c("Xbar", "S")) {
    chart <- autospc_chart(
      chart_type = chart_type,
      data = data.frame(x = 1:3, y = 1:3, n = 5, s = 1),
      x = "x", y = "y", n = "n", s = "s"
    )

    expect_true(
      trend_rule_applies(chart, table = data.frame(series = 1:3, n = 5)),
      info = chart_type
    )
    expect_true(
      trend_rule_applies(chart,
        table = data.frame(series = c(1, 2, NA), n = c(5, 5, 3))
      ),
      info = chart_type
    )
    expect_false(
      trend_rule_applies(chart,
        table = data.frame(series = 1:3, n = c(5, 5, 3))
      ),
      info = chart_type
    )
  }
})


test_that("a set adds a column for each of its rules, named by id", {
  chart <- c_chart()
  period <- data.frame(
    series = c(10, 11, 12, 13, 14, 15, 24, 25, 10),
    cl = 10,
    sd_estimate = sqrt(10),
    above_or_below_cl = c(0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L)
  )

  with_rules <- add_rule_set_columns(period,
    rules = rule_set("provost_murray")$rules,
    chart = chart,
    trend_applies = TRUE
  )

  # 10 + 2 * sqrt(10) is 16.3, so 24 and 25 are beyond 2 standard errors
  expect_identical(with_rules$rule3, c(rep(TRUE, 8), FALSE))
  expect_identical(with_rules$rule4, c(rep(FALSE, 6), TRUE, TRUE, FALSE))
  expect_identical(with_rules$rule5, rep(FALSE, 9))
})


test_that("the trend column is NA where the trend rule is not applied", {
  period <- data.frame(
    series = 1:7,
    cl = 4,
    sd_estimate = 1,
    above_or_below_cl = as.integer(sign(1:7 - 4))
  )

  with_rules <- add_rule_set_columns(period,
    rules = rule_set("provost_murray")$rules,
    chart = c_chart(),
    trend_applies = FALSE
  )

  expect_identical(with_rules$rule3, rep(NA, 7))
  expect_false(anyNA(with_rules$rule4))
})


test_that("the distance from the centre line is in the chart's standard errors", {
  chart <- autospc_chart(
    chart_type = "P",
    data = data.frame(x = 1:2, y = 1:2, n = 10),
    x = "x", y = "y", n = "n"
  )
  table <- data.frame(
    series = c(30, 30),
    cl = 20,
    sd_estimate = 40,
    n = c(16, 100),
    above_or_below_cl = 1L
  )

  # the standard error is 40 / 4 = 10 at the first row and 40 / 10 = 4 at the
  # second, so the same point is 1 and 2.5 standard errors from the centre line
  expect_identical(variables_for_rules(table, chart = chart)$z, c(1, 2.5))
})
