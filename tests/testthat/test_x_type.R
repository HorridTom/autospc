# The axis scales in visualisation.R handle x columns of type Date, POSIXct,
# numeric and integer. Any other type gives a warning rather than an error,
# because the analysis itself only uses x to order the rows.

x_type_message <- paste("Please make sure that your x column is a",
                        "'Date', 'POSIXct', 'numeric' or 'integer' type.")

x_type_data <- function(x) {

  data.frame(x = x,
             y = rep(c(10L, 12L, 11L, 13L, 9L, 14L), 5L))

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
    autospc(x_type_data(as.character(1:30)), chart_type = "C",
            period_min = 21L, plot_chart = FALSE),
    x_type_message,
    fixed = TRUE
  )

})


test_that("an integer x column is not warned about", {

  expect_no_warning(
    autospc(x_type_data(1:30), chart_type = "C", period_min = 21L,
            plot_chart = FALSE)
  )

})


test_that("a Date x column is not warned about", {

  dates <- as.Date("2020-01-01") + seq(0, by = 30, length.out = 30)

  expect_no_warning(
    autospc(x_type_data(dates), chart_type = "C", period_min = 21L,
            plot_chart = FALSE)
  )

})


test_that("a POSIXct x column is not warned about", {

  times <- as.POSIXct("2020-01-01", tz = "UTC") +
    seq(0, by = 86400, length.out = 30)

  expect_no_warning(
    autospc(x_type_data(times), chart_type = "C", period_min = 21L,
            plot_chart = FALSE)
  )

})


test_that("an XMR request is warned about once, not once per chart of the pair", {

  count <- count_x_type_warnings(
    autospc(x_type_data(as.character(1:30)), chart_type = "XMR",
            period_min = 21L, plot_chart = FALSE)
  )

  expect_identical(count, 1L)

})


test_that("a faceted chart repeats the warning, once per facet and once more", {

  # Three facets give four warnings: facet_stages() checks the data it was
  # passed, and each facet is checked again when it is analysed. The count was
  # measured on the code before check_x_type() was written and is unchanged by
  # it. CLEAN UP #32.
  count <- count_x_type_warnings(
    facet_stages(x_type_data(as.character(1:30)),
                 split_rows = c(10L, 20L, 30L), chart_type = "C",
                 period_min = 21L, plot_chart = FALSE)
  )

  expect_identical(count, 4L)

})
