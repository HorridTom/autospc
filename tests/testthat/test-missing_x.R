# Rows with no x are excluded before any chart is built, so nothing downstream
# sees a series with a missing x. The warning is controlled by a package option
# rather than an argument, so that a caller drawing many charts sets it once.

missing_x_data <- function(rows_without_x = 0L) {
  d <- data.frame(
    x = 1:25,
    y = as.numeric(rep(c(10, 14, 11, 16, 12), 5))
  )

  if (rows_without_x > 0L) {
    d$x[seq_len(rows_without_x)] <- NA
  }

  return(d)
}

analyse_quietly <- function(d, ...) {
  return(suppressWarnings(
    autospc(d,
      chart_type = "C\'",
      x = "x",
      y = "y",
      plot_chart = FALSE,
      period_min = 21L,
      ...
    )
  ))
}


test_that("a row with no x is excluded", {
  expect_identical(nrow(analyse_quietly(missing_x_data(0L))), 25L)
  expect_identical(nrow(analyse_quietly(missing_x_data(1L))), 24L)
  expect_identical(nrow(analyse_quietly(missing_x_data(3L))), 22L)
})


test_that("excluding a row warns, and says how many", {
  expect_warning(
    autospc(missing_x_data(3L), chart_type = "C\'", x = "x", y = "y",
      plot_chart = FALSE, period_min = 21L
    ),
    "3 rows were excluded"
  )
})


test_that("the warning is worded for a single row", {
  expect_warning(
    autospc(missing_x_data(1L), chart_type = "C\'", x = "x", y = "y",
      plot_chart = FALSE, period_min = 21L
    ),
    "1 row was excluded"
  )
})


test_that("the warning names the column the caller used as x", {
  d <- missing_x_data(1L)
  names(d)[1] <- "month_start"

  expect_warning(
    autospc(d, chart_type = "C\'", x = "month_start", y = "y",
      plot_chart = FALSE, period_min = 21L
    ),
    "no month_start value"
  )
})


test_that("naming a column the data does not hold is still reported as that", {
  # the x filter runs before the class validator, and must not get in first
  # with an error of its own
  expect_error(
    autospc(missing_x_data(0L), chart_type = "C\'", x = "not_a_column",
      y = "y", plot_chart = FALSE, period_min = 21L
    ),
    "not_a_column"
  )
})


test_that("no warning is given when nothing is excluded", {
  expect_no_warning(
    autospc(missing_x_data(0L), chart_type = "C\'", x = "x", y = "y",
      plot_chart = FALSE, period_min = 21L
    )
  )
})


test_that("the option turns the warning off", {
  previous <- options(autospc.warn_missing_x = FALSE)
  on.exit(options(previous), add = TRUE)

  expect_no_warning(
    autospc(missing_x_data(3L), chart_type = "C\'", x = "x", y = "y",
      plot_chart = FALSE, period_min = 21L
    )
  )
})


test_that("a value other than FALSE leaves the warning on", {
  previous <- options(autospc.warn_missing_x = "banana")
  on.exit(options(previous), add = TRUE)

  expect_warning(
    autospc(missing_x_data(3L), chart_type = "C\'", x = "x", y = "y",
      plot_chart = FALSE, period_min = 21L
    ),
    "3 rows were excluded"
  )
})


test_that("the warning can be handled on its own by its class", {
  caught <- tryCatch(
    autospc(missing_x_data(1L), chart_type = "C\'", x = "x", y = "y",
      plot_chart = FALSE, period_min = 21L
    ),
    autospc_missing_x_warning = function(w) "caught"
  )

  expect_identical(caught, "caught")
})


test_that("rows with no x do not count towards the minimum for limits", {
  # 20 subgroups is one short of period_min = 21L
  previous <- options(autospc.warn_missing_x = FALSE)
  on.exit(options(previous), add = TRUE)

  twenty <- data.frame(
    x = 1:20,
    y = as.numeric(rep(c(10, 14, 11, 16, 12), 4))
  )
  with_phantom <- rbind(twenty, data.frame(x = NA_integer_, y = 12))

  expect_warning(
    result <- autospc(with_phantom, chart_type = "C\'", x = "x", y = "y",
      plot_chart = FALSE, period_min = 21L
    ),
    "fewer than the minimum number of points"
  )

  expect_false("cl" %in% colnames(result))
})


test_that("facet_stages excludes rows with no x as well", {
  # caught by class rather than by expect_warning(), because stage 1 is 12 rows
  # and warns about being too short for limits as well
  caught <- tryCatch(
    facet_stages(missing_x_data(3L), split_rows = 12, chart_type = "C\'",
      x = "x", y = "y", plot_chart = FALSE, period_min = 21L
    ),
    autospc_missing_x_warning = function(w) conditionMessage(w)
  )

  expect_match(caught, "3 rows were excluded")
})


test_that("split_rows and the last stage count rows that have an x", {
  # facet_stages() is cumulative: each stage is the series up to its split
  # point, so with 3 of 25 rows dropped the stages hold 12 and 22 rows rather
  # than 12 and 25
  previous <- options(autospc.warn_missing_x = FALSE)
  on.exit(options(previous), add = TRUE)

  result <- suppressWarnings(
    facet_stages(missing_x_data(3L), split_rows = 12, chart_type = "C\'",
      x = "x", y = "y", plot_chart = FALSE, period_min = 21L
    )
  )

  expect_identical(as.integer(table(result$stage)), c(12L, 22L))
})
