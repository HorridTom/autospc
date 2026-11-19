# dispersion_chart_establishment = "forced": the dispersion chart of a pair
# takes the location chart's calculation periods.

gapped_series <- example_series_1
gapped_series$y[c(5L, 40L, 41L, 92L)] <- NA

# subgroups of three, except subgroups of one at x = 5, 12, 46 and 60. The
# subgroup means alternate 0.3 either side of 10 to x = 45, and of 14 after,
# where the spread within a subgroup is twice as wide
subgroup_series <- data.frame(
  x = rep(1:90, each = 3L),
  y = rep(ifelse(1:90 <= 45L, 10, 14) + rep(c(-0.3, 0.3), 45L), each = 3L) +
    rep(c(-1, 0, 1), 90L) * rep(ifelse(1:90 <= 45L, 1, 2), each = 3L)
)
subgroup_series <- subgroup_series[
  !(subgroup_series$x %in% c(5L, 12L, 46L, 60L) &
    duplicated(subgroup_series$x)),
]

# subgroups of four at x = 1 to 30 and 71 to 90, of one at x = 31 to 70
set.seed(1)
middle_of_ones <- rbind(
  data.frame(x = rep(1:30, each = 4L), y = stats::rnorm(120L, mean = 10)),
  data.frame(x = 31:70, y = stats::rnorm(40L, mean = 20)),
  data.frame(x = rep(71:90, each = 4L), y = stats::rnorm(80L, mean = 20))
)

# subgroups of one at x = 1 to 30, of four at x = 31 to 70
set.seed(2)
first_of_ones <- rbind(
  data.frame(x = 1:30, y = stats::rnorm(30L, mean = 10)),
  data.frame(x = rep(31:70, each = 4L), y = stats::rnorm(160L, mean = 20))
)

# max_exclusions is set unless a test sets it, so that the announcement of its
# change of default is not given
pair <- function(data, chart_type, ...) {
  arguments <- list(...)

  if (!"max_exclusions" %in% names(arguments)) {
    arguments$max_exclusions <- 3L
  }

  plot <- rlang::exec(autospc, data, chart_type = chart_type, !!!arguments)

  return(autospc_plot_charts(plot))
}

# the S chart's centre line at given values of x
s_cl_at <- function(charts, x) {
  table <- charts$dispersion$result$table

  return(table$cl[match(x, table$x)])
}


test_that("the MR chart takes the X chart's calculation periods", {
  charts <- pair(gapped_series,
    chart_type = "XMR",
    dispersion_chart_establishment = "forced"
  )

  expect_identical(
    calculation_periods(charts$dispersion$result$table),
    calculation_periods(charts$location$result$table)
  )
  expect_identical(
    nrow(calculation_periods(charts$location$result$table)),
    3L
  )
})


test_that("the S chart takes the Xbar chart's calculation periods, a subgroup of one at a period start included", {
  charts <- pair(subgroup_series,
    chart_type = "XbarS",
    dispersion_chart_establishment = "forced"
  )
  location_periods <- calculation_periods(charts$location$result$table)

  expect_identical(
    calculation_periods(charts$dispersion$result$table),
    location_periods
  )
  # x = 46 is a subgroup of one, and the first x of the second period
  expect_identical(location_periods$first_x[[2L]], 46L)
  expect_identical(
    s_cl_at(charts, 46L),
    s_cl_at(charts, 47L)
  )
  expect_false(identical(s_cl_at(charts, 45L), s_cl_at(charts, 46L)))
})


test_that("the location chart is the same as with independent", {
  for (case in list(
    list(gapped_series, chart_type = "XMR"),
    list(subgroup_series, chart_type = "XbarS")
  )) {
    independent <- rlang::exec(pair, !!!case)
    forced <- rlang::exec(pair, !!!case,
      dispersion_chart_establishment = "forced"
    )

    expect_identical(
      forced$location$result$table,
      independent$location$result$table
    )
  }
})


test_that("the S chart's centre line in each calculation period is the Xbar chart's sbar", {
  charts <- pair(subgroup_series,
    chart_type = "XbarS",
    dispersion_chart_establishment = "forced",
    max_exclusions = 0L
  )
  location <- charts$location$result$table
  s <- charts$dispersion$result$table
  calculation <- location$period_type %in% "calculation" & !is.na(s$cl)

  expect_gt(sum(calculation), 0L)
  expect_equal(s$cl[calculation], location$sbar[calculation])
})


test_that("the moving range at each re-establishment is excluded, and left out of the calculation", {
  charts <- pair(gapped_series,
    chart_type = "XMR",
    dispersion_chart_establishment = "forced",
    max_exclusions = 0L
  )
  mr <- charts$dispersion$result$table

  # the X chart re-establishes limits at x = 53 and 74
  expect_identical(which(mr$excluded %in% TRUE), c(53L, 74L))
  expect_identical(which(mr$break_point %in% TRUE), c(53L, 74L))
  expect_equal(mr$cl[53L], mean(mr$series[54:73]))
  expect_false(is.na(mr$series[53L]))
})


test_that("a point excluded first does not count towards max_exclusions", {
  chart <- autospc_chart_c(
    data = data.frame(x = 1:21, y = 1:21),
    x = "x",
    y = "y",
    max_exclusions = 1L
  )
  data <- data.frame(x = 1:21, series = c(rep(10, 9), 60, rep(10, 11)))

  expect_identical(
    find_extremes(data,
      chart = chart,
      counter = 1L,
      period_length = 21L,
      excluded_first = 1L
    ),
    c(1L, 10L)
  )

  chart$max_exclusions <- 0L
  expect_identical(
    find_extremes(data,
      chart = chart,
      counter = 1L,
      period_length = 21L,
      excluded_first = 1L
    ),
    1L
  )
})


test_that("a period with no points on the S chart has no centre line or limits there", {
  charts <- pair(middle_of_ones,
    chart_type = "XbarS",
    dispersion_chart_establishment = "forced"
  )

  expect_identical(
    calculation_periods(charts$location$result$table)$first_x,
    c(1L, 31L)
  )
  expect_false(anyNA(s_cl_at(charts, 1:30)))
  expect_true(all(is.na(s_cl_at(charts, 31:90))))
  expect_true(all(is.na(charts$dispersion$result$table$ucl[31:90])))
})


test_that("where the first period has no points on the S chart, it has no limits before the next", {
  charts <- pair(first_of_ones,
    chart_type = "XbarS",
    dispersion_chart_establishment = "forced"
  )
  location_periods <- calculation_periods(charts$location$result$table)

  # the second period starts among the subgroups of one, before the first
  # point on the S chart at x = 31
  expect_identical(nrow(location_periods), 2L)
  expect_lt(location_periods$first_x[[2L]], 31L)
  expect_true(all(is.na(s_cl_at(charts, 1:30))))
  expect_false(anyNA(s_cl_at(charts, 31:70)))
  expect_identical(
    calculation_periods(charts$dispersion$result$table),
    data.frame(first_x = 31L, last_x = location_periods$last_x[[2L]])
  )
})


test_that("the S chart has limits where the location chart has a period, with fewer than period_min points of its own", {
  # 15 subgroups of one, then 15 of three: 30 points on the Xbar chart, 15 on
  # the S chart
  set.seed(3)
  data <- rbind(
    data.frame(x = 1:15, y = stats::rnorm(15L, mean = 10)),
    data.frame(x = rep(16:30, each = 3L), y = stats::rnorm(45L, mean = 10))
  )

  # the S chart alone has too few points, so warns and is drawn without limits
  independent <- suppressWarnings(pair(data, chart_type = "XbarS"))
  forced <- pair(data,
    chart_type = "XbarS",
    dispersion_chart_establishment = "forced"
  )

  expect_false(enough_data_for_limits(independent$dispersion))
  expect_true(enough_data_for_limits(forced$dispersion))
  expect_false(anyNA(forced$dispersion$result$table$cl[16:30]))
})


test_that("the log records each period taken from the location chart", {
  charts <- pair(gapped_series,
    chart_type = "XMR",
    dispersion_chart_establishment = "forced"
  )
  log <- charts$dispersion$result$table$log

  expect_identical(which(!is.na(log)), c(1L, 53L, 74L))
  expect_identical(log[c(1L, 53L, 74L)], c("0800", "0810", "0810"))
  expect_identical(
    interpret_log_entry("0800", verbosity = 2L),
    "Limits established - forced by location chart."
  )
  expect_identical(
    interpret_log_entry("0810", verbosity = 2L),
    "Limits re-established - forced by location chart."
  )
})


test_that("the log gives the restart code before the first period after a restart", {
  charts <- pair(subgroup_series,
    chart_type = "XbarS",
    dispersion_chart_establishment = "forced",
    force_restart_at = 40L
  )
  log <- charts$dispersion$result$table$log
  starts <- calculation_periods(charts$location$result$table)$first_x

  expect_identical(starts, c(1L, 40L, 61L))
  expect_identical(which(!is.na(log)), c(1L, 40L, 61L))
  expect_identical(log[c(1L, 40L, 61L)], c("0800", "0000;0800", "0810"))
})


test_that("the history of a chart that takes the location chart's periods records no candidates", {
  charts <- pair(gapped_series,
    chart_type = "XMR",
    dispersion_chart_establishment = "forced"
  )

  expect_null(charts$dispersion$history$counter_path)
  expect_null(charts$dispersion$history$breaks)
  expect_null(charts$dispersion$history$candidates)
})


test_that("only the dispersion chart of a mirrored pair holds the location chart's periods", {
  independent <- pair(gapped_series, chart_type = "XMR")
  forced <- pair(gapped_series,
    chart_type = "XMR",
    dispersion_chart_establishment = "forced"
  )

  expect_null(independent$dispersion$location_periods)
  expect_null(forced$location$location_periods)
  expect_identical(
    forced$dispersion$location_periods,
    calculation_periods(forced$location$result$table)
  )
})


test_that("an MR chart drawn alone ignores dispersion_chart_establishment", {
  alone <- autospc(gapped_series,
    chart_type = "MR",
    max_exclusions = 3L,
    plot_chart = FALSE
  )
  forced <- suppressWarnings(
    autospc(gapped_series,
      chart_type = "MR",
      max_exclusions = 3L,
      dispersion_chart_establishment = "forced",
      plot_chart = FALSE
    ),
    classes = "autospc_dispersion_chart_establishment_ignored_warning"
  )

  expect_identical(forced, alone)
})


test_that("printing a chart that takes the location chart's periods says so", {
  charts <- pair(gapped_series,
    chart_type = "XMR",
    dispersion_chart_establishment = "forced"
  )

  expect_output(
    print(charts$dispersion),
    "Calculation periods mirror the X chart"
  )

  s_chart <- pair(subgroup_series,
    chart_type = "XbarS",
    dispersion_chart_establishment = "forced"
  )$dispersion

  expect_output(print(s_chart), "Calculation periods mirror the Xbar chart")
  expect_output(
    print(charts$dispersion),
    "Limits re-established at 53, 74"
  )
})


test_that("a forced pair draws", {
  plot <- autospc(subgroup_series,
    chart_type = "XbarS",
    max_exclusions = 3L,
    dispersion_chart_establishment = "forced"
  )

  expect_no_error(drawn(plot))

  plot <- autospc(first_of_ones,
    chart_type = "XbarS",
    max_exclusions = 3L,
    dispersion_chart_establishment = "forced"
  )

  expect_no_error(drawn(plot))
})


test_that("rows with no S value in a period with no S points have no limits, where a later period follows", {
  # subgroups of four, then of one, then of four
  set.seed(4)
  data <- rbind(
    data.frame(x = rep(1:30, each = 4L), y = stats::rnorm(120L, mean = 10)),
    data.frame(x = 31:60, y = stats::rnorm(30L, mean = 20)),
    data.frame(x = rep(61:100, each = 4L), y = stats::rnorm(160L, mean = 30))
  )

  charts <- pair(data,
    chart_type = "XbarS",
    dispersion_chart_establishment = "forced"
  )

  expect_identical(
    calculation_periods(charts$location$result$table)$first_x,
    c(1L, 31L, 60L)
  )
  expect_true(all(is.na(s_cl_at(charts, 31:59))))
  # x = 60 is a subgroup of one at the start of the third period
  expect_identical(s_cl_at(charts, 60L), s_cl_at(charts, 61L))
  expect_false(anyNA(s_cl_at(charts, c(1:30, 61:100))))
})


test_that("S points in an earlier period with no S points belong to no period", {
  # subgroups of one at x = 1 to 21, then of four
  set.seed(5)
  data <- rbind(
    data.frame(x = 1:21, y = stats::rnorm(21L, mean = 10)),
    data.frame(x = rep(22:28, each = 4L), y = stats::rnorm(28L, mean = 10)),
    data.frame(x = rep(29:70, each = 4L), y = stats::rnorm(168L, mean = 20))
  )

  charts <- pair(data,
    chart_type = "XbarS",
    dispersion_chart_establishment = "forced"
  )
  s <- charts$dispersion$result$table

  expect_identical(
    calculation_periods(charts$location$result$table)$first_x,
    c(1L, 29L)
  )
  expect_false(anyNA(s$series[22:28]))
  expect_true(all(is.na(s$cl[22:28])))
  expect_true(all(is.na(s$plot_period[22:28])))
  expect_true(all(is.na(s$period_type[22:28])))
  expect_identical(
    calculation_periods(s),
    data.frame(first_x = 29L, last_x = 49L)
  )
})


test_that("an S point at the start of a period is not excluded", {
  charts <- pair(subgroup_series,
    chart_type = "XbarS",
    dispersion_chart_establishment = "forced",
    force_restart_at = 40L,
    max_exclusions = 0L
  )
  s <- charts$dispersion$result$table

  # x = 61 is a subgroup of three at the start of the third period
  expect_identical(
    calculation_periods(charts$location$result$table)$first_x,
    c(1L, 40L, 61L)
  )
  expect_false(is.na(s$series[61L]))
  expect_false(any(s$excluded %in% TRUE))
})


test_that("S points only in display rows give the S chart no limits", {
  # subgroups of one at x = 1 to 21, the first calculation period, then of four
  set.seed(6)
  data <- rbind(
    data.frame(x = 1:21, y = stats::rnorm(21L, mean = 10)),
    data.frame(x = rep(22:30, each = 4L), y = stats::rnorm(36L, mean = 10))
  )

  # the S chart has no limits, so warns and is drawn without them
  charts <- suppressWarnings(pair(data,
    chart_type = "XbarS",
    dispersion_chart_establishment = "forced"
  ))

  expect_identical(
    calculation_periods(charts$location$result$table),
    data.frame(first_x = 1L, last_x = 21L)
  )
  expect_false(enough_data_for_limits(charts$dispersion))
  expect_true(all(is.na(charts$dispersion$result$table$cl)))
})


test_that("the S chart has limits where only a later segment has S points in a period", {
  # subgroups of one to x = 30, then of four, with a forced restart at x = 31
  set.seed(7)
  data <- rbind(
    data.frame(x = 1:30, y = stats::rnorm(30L, mean = 10)),
    data.frame(x = rep(31:60, each = 4L), y = stats::rnorm(120L, mean = 10))
  )

  charts <- pair(data,
    chart_type = "XbarS",
    dispersion_chart_establishment = "forced",
    force_restart_at = 31L
  )

  expect_true(enough_data_for_limits(charts$dispersion))
  expect_true(all(is.na(s_cl_at(charts, 1:30))))
  expect_false(anyNA(s_cl_at(charts, 31:60)))
})


# dispersion_chart_establishment = "shared": each calculation period is extended
# until both charts have period_min points in it, and both charts use it.

per_periods <- function(chart) {
  return(calculation_periods(chart$result$table))
}


test_that("shared extends the X chart's periods until the MR chart has period_min points", {
  charts <- pair(gapped_series,
    chart_type = "XMR",
    dispersion_chart_establishment = "shared"
  )

  # x = 5 and 92 are missing, so the periods containing them hold 19 moving
  # ranges in 22 rows, and are extended by one row
  expect_identical(
    per_periods(charts$location),
    data.frame(first_x = c(1L, 53L, 74L), last_x = c(23L, 73L, 96L))
  )
  expect_identical(per_periods(charts$dispersion), per_periods(charts$location))
  expect_identical(charts$location$history$baseline$length, 22L)

  # the counter moves to the first row after each extended period
  moves <- charts$location$history$counter_path
  moves <- moves[moves$reason %in% c(
    "first period established", "limits re-established"
  ), ]
  expect_identical(moves$to, c(24L, 74L, 97L))
})


test_that("shared extends the Xbar chart's periods until the S chart has period_min points", {
  charts <- pair(subgroup_series,
    chart_type = "XbarS",
    dispersion_chart_establishment = "shared"
  )

  # subgroups of one at x = 5 and 12 in the first period, and 46 and 60 in the
  # second
  expect_identical(
    per_periods(charts$location),
    data.frame(first_x = c(1L, 46L), last_x = c(23L, 68L))
  )
  expect_identical(per_periods(charts$dispersion), per_periods(charts$location))
})


test_that("shared gives the same charts as forced where the dispersion chart has a point wherever the location chart does", {
  complete <- subgroup_series[!subgroup_series$x %in% c(5L, 12L, 46L, 60L), ]

  for (case in list(
    list(example_series_1, chart_type = "XMR"),
    list(complete, chart_type = "XbarS")
  )) {
    forced <- rlang::exec(pair, !!!case,
      dispersion_chart_establishment = "forced"
    )
    shared <- rlang::exec(pair, !!!case,
      dispersion_chart_establishment = "shared"
    )

    expect_identical(
      shared$location$result$table,
      forced$location$result$table
    )
    expect_identical(
      shared$dispersion$result$table,
      forced$dispersion$result$table
    )
  }
})


test_that("shared does not re-establish where the dispersion chart has too few points after the shift", {
  # subgroups of four to x = 30, then around 20 with every other subgroup a
  # subgroup of one
  set.seed(8)
  data <- rbind(
    data.frame(x = rep(1:30, each = 4L), y = stats::rnorm(120L, mean = 10)),
    do.call(rbind, lapply(31:60, function(x) {
      data.frame(x = x, y = stats::rnorm(if (x %% 2L == 0L) 1L else 4L, 20))
    }))
  )

  forced <- pair(data,
    chart_type = "XbarS",
    dispersion_chart_establishment = "forced"
  )
  shared <- pair(data,
    chart_type = "XbarS",
    dispersion_chart_establishment = "shared"
  )

  expect_identical(per_periods(forced$location)$first_x, c(1L, 27L))
  expect_identical(per_periods(shared$location)$first_x, 1L)
  expect_identical(
    shared$location$history$stopped$reason,
    "too few points after the shift rule break"
  )
})


test_that("a first period whose dispersion chart never has baseline_length points runs to the end of the data", {
  chart <- autospc_chart_x(
    data = data.frame(x = 1:50, y = 1:50),
    x = "x",
    y = "y",
    baseline_length = 40L,
    dispersion_chart_establishment = "shared"
  )
  # 30 moving ranges in 50 rows: the MR chart has 31 points, fewer than 40
  data <- data.frame(
    x = 1:50,
    dispersion_observed = c(FALSE, rep(TRUE, 30), rep(FALSE, 19))
  )

  expect_identical(calculation_period_length(chart, data, counter = 1L), 50L)
})


test_that("an X chart drawn alone ignores shared", {
  alone <- autospc(gapped_series,
    chart_type = "X",
    max_exclusions = 3L,
    plot_chart = FALSE
  )
  shared <- suppressWarnings(
    autospc(gapped_series,
      chart_type = "X",
      max_exclusions = 3L,
      dispersion_chart_establishment = "shared",
      plot_chart = FALSE
    ),
    classes = "autospc_dispersion_chart_establishment_ignored_warning"
  )

  expect_identical(shared, alone)
})


test_that("facet_stages analyses each facet as the pair under shared", {
  facets <- function(mode) {
    table <- facet_stages(gapped_series,
      split_at = 60L,
      chart_type = "XMR",
      dispersion_chart_establishment = mode,
      max_exclusions = 3L,
      plot_chart = FALSE
    )

    return(
      table %>%
        dplyr::filter(period_type == "calculation") %>%
        dplyr::group_by(stage, plot_period) %>%
        dplyr::summarise(
          first_x = min(x),
          last_x = max(x),
          .groups = "drop"
        ) %>%
        dplyr::arrange(stage, first_x) %>%
        dplyr::select(stage, first_x, last_x) %>%
        as.data.frame()
    )
  }

  expect_identical(
    facets("independent")$last_x,
    c(22L, 22L, 73L, 95L)
  )
  expect_identical(
    facets("shared")$last_x,
    c(23L, 23L, 73L, 96L)
  )
})


test_that("under shared, the next candidate starts after the extended period", {
  # a shift up at x = 22, with x = 26 missing, so that its period is extended
  # by one row to x = 44, and a further shift up starting at x = 44
  y <- c(
    rep(c(9, 11), length.out = 21L),
    rep(c(19, 21), length.out = 22L),
    rep(c(29, 31), length.out = 26L)
  )
  y[26L] <- NA
  data <- data.frame(x = seq_along(y), y = y)

  charts <- pair(data,
    chart_type = "XMR",
    dispersion_chart_establishment = "shared",
    no_regrets = FALSE,
    na_ends_run = FALSE,
    max_exclusions = 0L
  )

  expect_identical(
    per_periods(charts$location),
    data.frame(first_x = c(1L, 22L, 45L), last_x = c(21L, 44L, 65L))
  )
})


test_that("under shared, an opposite rule break in the extension rows is found", {
  # a shift up at x = 22, with x = 24 and 30 missing, so that its period is
  # extended by two rows to x = 46, and a run of eight below its centre line
  # at x = 39 to 46, which ends in the extension rows
  y <- c(
    rep(c(9, 11), length.out = 21L),
    rep(c(21, 23), length.out = 17L),
    rep(c(15, 17), length.out = 8L)
  )
  y[c(24L, 30L)] <- NA
  data <- data.frame(x = seq_along(y), y = y)

  table <- autospc(data,
    chart_type = "XMR",
    dispersion_chart_establishment = "shared",
    overhanging_reversions = FALSE,
    no_regrets = FALSE,
    na_ends_run = FALSE,
    max_exclusions = 0L,
    plot_chart = FALSE
  )

  expect_identical(which(table$break_point %in% TRUE), integer(0))
})

