# Ranges given by prevent_establishment_in: a candidate that starts in one is
# not formed, and the counter moves to the first row after the range.

range_series <- example_series_1

charts_with <- function(data, ..., chart_type = "C") {
  autospc_plot_charts(suppressWarnings(
    autospc(data,
      chart_type = chart_type, x = x, y = y,
      max_exclusions = 0L, ...
    )
  ))
}

prevented_counters <- function(chart) {
  candidates <- Filter(
    function(candidate) isTRUE(candidate$prevented),
    chart$history$candidates
  )

  return(vapply(candidates, function(candidate) candidate$counter, integer(1L)))
}

# 21 points alternating 9 and 11, then a shift up that lasts to the end of the
# series, with nothing to oppose it
lasting_shift <- data.frame(
  x = 1:70,
  y = c(rep(c(9, 11), length.out = 21), rep(c(19, 21), length.out = 49))
)


test_that("a re-establishment inside a range does not happen", {
  # by default limits are re-established at rows 26, 47 and 74
  expect_identical(
    charts_with(range_series)[[1]]$result$re_establish_rows,
    c(26L, 47L, 74L)
  )

  chart <- charts_with(range_series, prevent_establishment_in = c(20, 30))[[1]]

  expect_false(26L %in% chart$result$re_establish_rows)
  expect_identical(prevented_counters(chart), 26L)
})


test_that("the counter moves to the first row after the range", {
  chart <- charts_with(range_series, prevent_establishment_in = c(20, 30))[[1]]
  path <- chart$history$counter_path

  expect_identical(
    path[path$reason == "in a range where establishment is prevented", ],
    data.frame(
      from = 26L,
      to = 31L,
      reason = "in a range where establishment is prevented"
    ),
    ignore_attr = TRUE
  )
})


test_that("a shift that starts in a range can re-establish after it", {
  # without a range, limits are re-established where the shift starts
  expect_identical(
    charts_with(lasting_shift)[[1]]$result$re_establish_rows,
    22L
  )

  # the shift starts inside the range and continues well past its end
  chart <- charts_with(lasting_shift, prevent_establishment_in = c(20, 25))[[1]]

  expect_identical(chart$result$re_establish_rows, 26L)
})


test_that("a range with an open end prevents every re-establishment after its start", {
  chart <- charts_with(range_series, prevent_establishment_in = c(40, NA))[[1]]

  expect_identical(chart$result$re_establish_rows, 26L)
  expect_identical(prevented_counters(chart), 47L)
})


test_that("a range with an open start prevents re-establishment up to its end", {
  chart <- charts_with(range_series, prevent_establishment_in = c(NA, 30))[[1]]

  expect_identical(prevented_counters(chart), 26L)
  expect_false(26L %in% chart$result$re_establish_rows)
})


test_that("a range can be a single x", {
  chart <- charts_with(range_series, prevent_establishment_in = c(26, 26))[[1]]

  expect_identical(prevented_counters(chart), 26L)

  # the next row is considered as a candidate
  counters <- vapply(
    chart$history$candidates,
    function(candidate) candidate$counter,
    integer(1L)
  )
  expect_true(27L %in% counters)
})


test_that("each of several ranges prevents re-establishment", {
  chart <- charts_with(range_series,
    prevent_establishment_in = list(c(20, 30), c(70, 80))
  )[[1]]

  expect_identical(prevented_counters(chart), c(26L, 74L))
})


test_that("where ranges overlap, the counter moves past the one that ends last", {
  chart <- charts_with(range_series,
    prevent_establishment_in = list(c(20, 30), c(25, 40))
  )[[1]]
  path <- chart$history$counter_path

  expect_identical(
    path$to[path$reason == "in a range where establishment is prevented"],
    41L
  )
})


test_that("the first calculation period is not affected", {
  plain <- charts_with(range_series)[[1]]$result$table
  ranged <- charts_with(range_series,
    prevent_establishment_in = c(NA, 10)
  )[[1]]$result$table

  expect_identical(ranged, plain)
})


test_that("ranges outside the data change nothing", {
  plain <- charts_with(range_series)[[1]]$result$table
  ranged <- charts_with(range_series,
    prevent_establishment_in = list(c(-10, -5), c(200, 300))
  )[[1]]$result$table

  expect_identical(ranged, plain)
})


test_that("a forced restart inside a range is respected", {
  chart <- charts_with(range_series,
    prevent_establishment_in = c(50, 70),
    force_restart_at = 60
  )[[1]]

  expect_true(chart$result$table$restart[60])
  expect_true(chart$result$table$break_point[60])
})


test_that("a prevented candidate is logged as 0730, with no checks before it", {
  chart <- charts_with(range_series, prevent_establishment_in = c(20, 30))[[1]]

  expect_identical(chart$result$table$log[26], "050010;0730")

  expect_identical(
    interpret_log_entry("0730", verbosity = 1L),
    "Not considered: in a range given by prevent_establishment_in."
  )
})


test_that("the history records a prevented candidate as not evaluated", {
  chart <- charts_with(range_series, prevent_establishment_in = c(20, 30))[[1]]
  prevented <- Filter(
    function(candidate) isTRUE(candidate$prevented),
    chart$history$candidates
  )[[1]]

  expect_false(prevented$accepted)
  expect_true(is.na(prevented$opposite_break))
  expect_true(is.na(prevented$final_run_prevents))
  expect_length(prevented$period_rows, 0L)
  expect_null(prevented$table)

  # the limits in force are those of the row before the candidate
  table <- chart$result$table
  expect_identical(
    prevented$prevailing,
    list(
      last_row = 25L,
      cl = table$cl[25],
      ucl = table$ucl[25],
      lcl = table$lcl[25]
    )
  )

  # a candidate that was formed carries no prevented element
  formed <- Filter(
    function(candidate) !isTRUE(candidate$prevented),
    chart$history$candidates
  )
  expect_null(formed[[1]]$prevented)
})


test_that("both charts of an XMR pair are prevented from re-establishing", {
  plain <- charts_with(range_series, chart_type = "XMR")
  ranged <- charts_with(range_series,
    chart_type = "XMR",
    prevent_establishment_in = c(20, 40)
  )

  expect_identical(plain$location$result$re_establish_rows, c(31L, 53L, 74L))
  expect_identical(ranged$location$result$re_establish_rows, c(53L, 74L))

  expect_identical(plain$dispersion$result$re_establish_rows, c(32L, 75L))
  expect_identical(ranged$dispersion$result$re_establish_rows, 75L)
})


test_that("a facet is not affected by a range it has not reached", {
  facets <- autospc_plot_charts(suppressWarnings(
    facet_stages(range_series,
      split_at = c(50L, 125L), chart_type = "C",
      max_exclusions = 0L, prevent_establishment_in = c(70, 80)
    )
  ))

  expect_length(prevented_counters(facets[[1]]), 0L)
  expect_identical(prevented_counters(facets[[2]]), 74L)
})


test_that("ranges of dates prevent re-establishment on a chart of dates", {
  chart <- autospc_plot_charts(suppressWarnings(
    autospc(ed_attendances_monthly,
      chart_type = "C", x = month_start, y = att_all,
      max_exclusions = 0L,
      prevent_establishment_in = as.Date(c("2016-01-01", "2018-12-01"))
    )
  ))[[1]]

  starts <- chart$result$table$x[chart$result$re_establish_rows]

  expect_false(any(
    starts >= as.Date("2016-01-01") & starts <= as.Date("2018-12-01")
  ))
  expect_gt(length(prevented_counters(chart)), 0L)
})


test_that("printing a chart says how many candidates were not formed", {
  chart <- charts_with(range_series, prevent_establishment_in = c(20, 30))[[1]]

  expect_output(
    print(chart),
    "1 candidate period not formed, in ranges given by prevent_establishment_in"
  )
})
