# The axis scales in visualisation.R are built for Date, POSIXct, numeric and
# integer. Anything else is a warning rather than an error, because the analysis
# only needs x to order the rows.

x_type_message <- paste("Please make sure that your x column is a",
                        "'Date', 'POSIXct', 'numeric' or 'integer' type.")

x_type_data <- function(x) {

  data.frame(x = x,
             y = rep(c(10L, 12L, 11L, 13L, 9L, 14L), 5L))

}

rounds_of_x_type_warning <- function(result) {

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

  count <- rounds_of_x_type_warning(
    autospc(x_type_data(as.character(1:30)), chart_type = "XMR",
            period_min = 21L, plot_chart = FALSE)
  )

  expect_identical(count, 1L)

})


test_that("a faceted chart repeats the warning, once per facet and once more", {

  # Four for three facets, measured on the code before check_x_type() existed
  # and unchanged by it: facet_stages() checks the series it was given, and each
  # facet is checked again as it is analysed. CLEAN UP #32.
  count <- rounds_of_x_type_warning(
    facet_stages(x_type_data(as.character(1:30)),
                 split_rows = c(10L, 20L, 30L), chart_type = "C",
                 period_min = 21L, plot_chart = FALSE)
  )

  expect_identical(count, 4L)

})
