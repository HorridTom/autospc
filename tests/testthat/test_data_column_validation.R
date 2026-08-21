data_column_validation_data_yn <- readRDS(
  file.path("testdata",
            "data_column_validation_data_yn.rds"))

data_column_validation_expected_conditions_yn <- readRDS(
  file.path("testdata",
            "data_column_validation_expected_conditions_yn.rds"))

data_column_validation_data_y <- readRDS(
  file.path("testdata",
            "data_column_validation_data_y.rds"))

data_column_validation_expected_conditions_y <- readRDS(
  file.path("testdata",
            "data_column_validation_expected_conditions_y.rds"))

chart_types_yn <- c("P", "P'")
chart_types_y <- c("XMR", "X", "MR", "C", "C'")

# The column requirements are checked in the class validators, which run when a
# chart is constructed, so each case is tested by constructing a chart.
# build_charts() is used rather than autospc_chart() because it accepts
# chart_type = "XMR", constructing an X chart and an MR chart.
build_from_columns <- function(df,
                               chart_type) {

  build_charts(chart_type = chart_type,
               data = df,
               x = "x",
               y = "y",
               n = "n")

}


# A case expects an error or a warning, never both, because the class validator
# runs before round_counts(): if the data fails a requirement, construction
# stops with an error and no rounding takes place.
expect_conditions_of_case <- function(df,
                                      chart_type,
                                      expected_err,
                                      expected_warn) {

  if(!is.na(expected_err)) {

    expect_error(
      build_from_columns(df, chart_type = chart_type),
      regexp = expected_err,
      fixed  = TRUE
    )

    return(invisible(NULL))

  }

  if(!is.na(expected_warn)) {

    warned <- FALSE

    charts <- withCallingHandlers(
      build_from_columns(df, chart_type = chart_type),
      warning = function(w) {
        if(grepl(expected_warn, conditionMessage(w), fixed = TRUE)) {
          warned <<- TRUE
        }
        invokeRestart("muffleWarning")
      }
    )

    expect_true(warned,
                label = paste("Expected warning not found:", expected_warn))

    expect_s3_class(charts[[1]], "autospc_chart")

    return(invisible(NULL))

  }

  charts <- expect_no_error(
    expect_no_warning(
      build_from_columns(df, chart_type = chart_type)
    )
  )

  expect_s3_class(charts[[1]], "autospc_chart")

  return(invisible(NULL))

}


for (chart_type in chart_types_yn) {
  for (i in seq_along(data_column_validation_data_yn)) {
    test_that(paste("column requirements: chart_type =", chart_type, "| case i =", i), {
      expect_conditions_of_case(
        df            = data_column_validation_data_yn[[i]],
        chart_type    = chart_type,
        expected_err  = data_column_validation_expected_conditions_yn[[paste0(chart_type, "_err")]][i],
        expected_warn = data_column_validation_expected_conditions_yn[[paste0(chart_type, "_warn")]][i]
      )
    })
  }
}


for (chart_type in chart_types_y) {
  for (i in seq_along(data_column_validation_data_y)) {
    test_that(paste("column requirements: chart_type =", chart_type, "| case i =", i), {
      expect_conditions_of_case(
        df            = data_column_validation_data_y[[i]],
        chart_type    = chart_type,
        expected_err  = data_column_validation_expected_conditions_y[[paste0(chart_type, "_err")]][i],
        expected_warn = data_column_validation_expected_conditions_y[[paste0(chart_type, "_warn")]][i]
      )
    })
  }
}


# no rounding takes place when the data fails a requirement


test_that("a P chart with an invalid n column errors without warning first", {

  bad_denominator <- data.frame(x = 1:30,
                                y = rep(c(10.4, 12.6, 11.5), 10L),
                                n = rep(c(TRUE, FALSE, TRUE), 10L))

  expect_no_warning(
    expect_error(
      autospc_chart(chart_type = "P", data = bad_denominator,
                    x = "x", y = "y", n = "n"),
      "n must be of type integer or double",
      fixed = TRUE
    )
  )

})


# the rounding reaches the analysis, not just the warning


rounding_data <- data.frame(x = 1:30,
                            y = rep(c(10.4, 12.6, 11.5, 13.4, 9.6, 14.4), 5L),
                            n = rep(100L, 30))


test_that("a C chart analyses the rounded y, not the y as passed", {

  result <- suppressWarnings(
    autospc(rounding_data, chart_type = "C", period_min = 21L,
            plot_chart = FALSE)
  )

  expect_identical(result$y, round(rounding_data$y))

})


test_that("a P chart analyses the rounded numerator", {

  result <- suppressWarnings(
    autospc(rounding_data, chart_type = "P", period_min = 21L,
            plot_chart = FALSE)
  )

  expect_identical(result$y_numerator, round(rounding_data$y))

})


test_that("counts are rounded before they are aggregated, not after", {

  # two rows per subgroup, each 1.4: rounding first gives 1 + 1 = 2, and
  # aggregating first gives round(2.8) = 3
  repeated <- data.frame(x = rep(1:30, each = 2L),
                         y = rep(1.4, 60L),
                         n = rep(50L, 60))

  result <- suppressWarnings(
    autospc(repeated, chart_type = "P", period_min = 21L, plot_chart = FALSE)
  )

  expect_true(all(result$y_numerator == 2))

})


test_that("a chart built from whole numbers keeps the type it was given", {

  whole <- data.frame(x = 1:30,
                      y = as.double(rep(c(10, 12, 11), 10L)))

  chart <- expect_no_warning(
    autospc_chart(chart_type = "C", data = whole, x = "x", y = "y")
  )

  expect_identical(chart$data$y, whole$y)

})
