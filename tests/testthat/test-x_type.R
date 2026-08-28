# The axis scales in visualisation.R handle x columns of type Date, POSIXct,
# numeric and integer. Any other type gives a warning rather than an error,
# because the analysis itself only uses x to order the rows.

x_type_message <- paste(
  "Please make sure that your x column is a",
  "'Date', 'POSIXct', 'numeric' or 'integer' type."
)

x_type_data <- function(x) {
  data.frame(
    x = x,
    y = rep(c(10L, 12L, 11L, 13L, 9L, 14L), 5L)
  )
}

count_x_type_warnings <- function(result) {
  warnings_given <- character()

  withCallingHandlers(
    force(result),
    warning = function(w) {
      warnings_given <<- c(warnings_given, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  sum(grepl(x_type_message, warnings_given, fixed = TRUE))
}


test_that("a character x column is warned about", {
  expect_warning(
    autospc(x_type_data(as.character(1:30)),
      chart_type = "C",
      period_min = 21L, plot_chart = FALSE
    ),
    x_type_message,
    fixed = TRUE
  )
})


test_that("an integer x column is not warned about", {
  expect_no_warning(
    autospc(x_type_data(1:30),
      chart_type = "C", period_min = 21L,
      plot_chart = FALSE
    )
  )
})


test_that("a Date x column is not warned about", {
  dates <- as.Date("2020-01-01") + seq(0, by = 30, length.out = 30)

  expect_no_warning(
    autospc(x_type_data(dates),
      chart_type = "C", period_min = 21L,
      plot_chart = FALSE
    )
  )
})


test_that("a POSIXct x column is not warned about", {
  times <- as.POSIXct("2020-01-01", tz = "UTC") +
    seq(0, by = 86400, length.out = 30)

  expect_no_warning(
    autospc(x_type_data(times),
      chart_type = "C", period_min = 21L,
      plot_chart = FALSE
    )
  )
})


test_that("an XMR request is warned about once, not once per chart of the pair", {
  count <- count_x_type_warnings(
    autospc(x_type_data(as.character(1:30)),
      chart_type = "XMR",
      period_min = 21L, plot_chart = FALSE
    )
  )

  expect_identical(count, 1L)
})


test_that("a faceted chart is warned about once, not once per facet", {
  count <- count_x_type_warnings(
    facet_stages(x_type_data(as.character(1:30)),
      split_rows = c(10L, 20L, 30L), chart_type = "C",
      period_min = 21L, plot_chart = FALSE
    )
  )

  expect_identical(count, 1L)
})


test_that("a column named as x that the data does not hold is left to the validator", {
  # check_x_type() has nothing to check, and the missing column is reported by
  # the class validator rather than warned about here
  missing_column <- data.frame(
    month = 1:30,
    y = rep(c(10L, 12L, 11L, 13L, 9L, 14L), 5L)
  )

  warnings_given <- character()

  withCallingHandlers(
    tryCatch(
      autospc(missing_column,
        chart_type = "C", x = not_a_column, y = y,
        period_min = 21L, plot_chart = FALSE
      ),
      error = function(e) invisible(NULL)
    ),
    warning = function(w) {
      warnings_given <<- c(warnings_given, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_false(any(grepl(x_type_message, warnings_given, fixed = TRUE)))
})


test_that("a series with an x of another type is still analysed", {
  # the warning is a warning, not an error
  result <- suppressWarnings(
    autospc(x_type_data(as.character(1:30)),
      chart_type = "C",
      period_min = 21L, plot_chart = FALSE
    )
  )

  expect_false(any(is.na(result$cl)))
})


test_that("a character x is sorted as characters are, which reorders the series", {
  # 10 sorts before 2, so the series the algorithm walks is not the series the
  # caller wrote down - which is what the warning is for
  result <- suppressWarnings(
    autospc(x_type_data(as.character(1:30)),
      chart_type = "C",
      period_min = 21L, plot_chart = FALSE
    )
  )

  expect_identical(result$x[1:3], c("1", "10", "11"))
})
