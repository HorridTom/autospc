# On an X or MR chart each point is one row, so a repeated x has no place to be
# plotted and is refused. The subgroup charts sum the rows that share an x, so
# a repeated x is ordinary there.

repeated <- function(times = 2L, subgroups = 12L) {
  return(data.frame(
    x = rep(seq_len(subgroups), each = times),
    y = as.numeric(seq_len(subgroups * times))
  ))
}

analyse <- function(d, chart_type) {
  return(autospc(d,
    chart_type = chart_type, x = "x", y = "y",
    plot_chart = FALSE, period_min = 10L
  ))
}


# the charts that refuse it


test_that("an X chart refuses a repeated x", {
  expect_error(analyse(repeated(), "X"), "x must be unique")
})


test_that("an MR chart refuses a repeated x", {
  expect_error(analyse(repeated(), "MR"), "x must be unique")
})


test_that("an XMR chart refuses a repeated x", {
  expect_error(analyse(repeated(), "XMR"), "x must be unique")
})


test_that("facet_stages refuses a repeated x on an X chart", {
  expect_error(
    facet_stages(repeated(),
      chart_type = "X", x = "x", y = "y",
      split_at = 12L, period_min = 10L
    ),
    "x must be unique"
  )
})


# the charts that do not


test_that("the subgroup charts still sum the rows that share an x", {
  for (chart_type in c("C", "C\'", "P", "P\'")) {
    result <- autospc(
      data.frame(
        x = rep(1:12, each = 2),
        y = as.numeric(rep(c(2, 3), 12)),
        n = rep(10L, 24)
      ),
      chart_type = chart_type, x = "x", y = "y", n = "n",
      plot_chart = FALSE, period_min = 10L
    )

    expect_identical(nrow(result), 12L)
  }
})


# what the error says


test_that("the error names the values that are repeated", {
  expect_error(
    analyse(repeated(subgroups = 3L), "X"),
    "Repeated x: 1, 2, 3\\."
  )
})


test_that("a value repeated three times is named once", {
  expect_error(
    analyse(repeated(times = 3L, subgroups = 3L), "X"),
    "Repeated x: 1, 2, 3\\."
  )
})


test_that("the error names five repeated values and counts the rest", {
  expect_error(
    analyse(repeated(subgroups = 7L), "X"),
    "Repeated x: 1, 2, 3, 4, 5, and 2 more\\."
  )
})


test_that("a repeated date is named as a date", {
  d <- data.frame(
    x = rep(as.Date("2026-01-01") + 0:11, each = 2L),
    y = as.numeric(1:24)
  )

  expect_error(analyse(d, "X"), "Repeated x: 2026-01-01, 2026-01-02")
})


# a series with no repeats is untouched


test_that("a unique x is accepted", {
  result <- analyse(repeated(times = 1L), "X")

  expect_identical(nrow(result), 12L)
})
