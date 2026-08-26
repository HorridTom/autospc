# Every combination of y and n column type, against the requirements each chart
# type places on them.
#
# The data frames are fixtures. The conditions expected of them are written out
# below rather than captured from the package, so that a change to a message
# fails a test instead of being recorded as the new expectation.

data_column_validation_data_yn <- readRDS(
  file.path("testdata",
            "data_column_validation_data_yn.rds"))

data_column_validation_data_y <- readRDS(
  file.path("testdata",
            "data_column_validation_data_y.rds"))


# The conditions the class validators raise, by name

expected_conditions <- list(

  y_missing_p = list(
    kind = "error",
    text = paste0("y not specified. For P and P' charts, y must be ",
                  "specified.")),

  y_type_p = list(
    kind = "error",
    text = paste0("For a P or P' chart with n specified, y must be of type ",
                  "integer or double.")),

  y_not_logical_p = list(
    kind = "error",
    text = paste0("n is not specified and y is not of type logical. For P ",
                  "and P' charts, if n is not specified, y must be of type ",
                  "logical.")),

  n_type_p = list(
    kind = "error",
    text = paste0("For a P or P' chart with n specified, n must be of type ",
                  "integer or double.")),

  y_rounded_p = list(
    kind = "warning",
    text = paste0("At least one element of y has non-zero fractional part. ",
                  "Rounding to the nearest whole number.\n",
                  "P and P' charts with n specified require y to be a count, ",
                  "i.e. whole numbers only.")),

  n_rounded_p = list(
    kind = "warning",
    text = paste0("At least one element of n has non-zero fractional part. ",
                  "Rounding to the nearest whole number.\n",
                  "P and P' charts with n specified require n to be a count, ",
                  "i.e. whole numbers only.")),

  y_missing_x = list(
    kind = "error",
    text = paste0("y not specified. For X, MR and XMR charts, y must be ",
                  "specified.")),

  y_type_x = list(
    kind = "error",
    text = "For X, MR and XMR charts, y must be of type integer or double."),

  y_missing_c = list(
    kind = "error",
    text = "y not specified. For C and C' charts, y must be specified."),

  y_type_c = list(
    kind = "error",
    text = "For a C or C' chart, y must be of type integer or double."),

  y_rounded_c = list(
    kind = "warning",
    text = paste0("At least one element of y has non-zero fractional part. ",
                  "Rounding to the nearest whole number.\n",
                  " C and C' charts are for count data, i.e. whole numbers ",
                  "only."))

)


# What each combination of column types should produce. NA is a chart built
# with no error and no warning.

column_types <- c("absent",
                  "logical",
                  "integer",
                  "double whole",
                  "double fractional")

# P and P' place requirements on both columns, so their expectations are a grid
# of y down and n across. Each row below is one y type, and within a row the
# entries are the n types in the order of column_types. The lookup is by name,
# so the comments describe the grid rather than define it.
p_expectations <- matrix(
  c(
    # y absent
    "y_missing_p", "y_missing_p", "y_missing_p", "y_missing_p", "y_missing_p",
    # y logical
    NA, "y_type_p", "y_type_p", "y_type_p", "y_type_p",
    # y integer
    "y_not_logical_p", "n_type_p", NA, NA, "n_rounded_p",
    # y double whole
    "y_not_logical_p", "n_type_p", NA, NA, "n_rounded_p",
    # y double fractional
    "y_not_logical_p", "n_type_p", "y_rounded_p", "y_rounded_p", "y_rounded_p"),
  nrow = length(column_types),
  byrow = TRUE,
  dimnames = list(y = column_types,
                  n = column_types))

# The other chart types use y alone. X, MR and XMR agree with each other, and
# C agrees with C'.
x_expectations <- c(absent              = "y_missing_x",
                    logical             = "y_type_x",
                    integer             = NA,
                    `double whole`      = NA,
                    `double fractional` = NA)

c_expectations <- c(absent              = "y_missing_c",
                    logical             = "y_type_c",
                    integer             = NA,
                    `double whole`      = NA,
                    `double fractional` = "y_rounded_c")


# The name this file gives a column, so that a case can be described by what it
# holds rather than by its position in the fixture
column_type <- function(data,
                        column) {

  if(!column %in% colnames(data)) {
    return("absent")
  }

  values <- data[[column]]

  if(is.logical(values)) {
    return("logical")
  }

  if(is.integer(values)) {
    return("integer")
  }

  if(is.double(values)) {

    if(all(is_whole_number(values), na.rm = TRUE)) {
      return("double whole")
    }

    return("double fractional")

  }

  return(typeof(values))

}


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
                                      expected) {

  if(is.na(expected)) {

    charts <- expect_no_error(
      expect_no_warning(
        build_from_columns(df, chart_type = chart_type)
      )
    )

    expect_s3_class(charts[[1]], "autospc_chart")

    return(invisible(NULL))

  }

  condition <- expected_conditions[[expected]]

  if(identical(condition$kind, "error")) {

    expect_error(
      build_from_columns(df, chart_type = chart_type),
      regexp = condition$text,
      fixed  = TRUE
    )

    return(invisible(NULL))

  }

  warned <- FALSE

  charts <- withCallingHandlers(
    build_from_columns(df, chart_type = chart_type),
    warning = function(w) {
      if(grepl(condition$text, conditionMessage(w), fixed = TRUE)) {
        warned <<- TRUE
      }
      invokeRestart("muffleWarning")
    }
  )

  expect_true(warned,
              label = paste("Expected warning not found:", condition$text))

  expect_s3_class(charts[[1]], "autospc_chart")

  return(invisible(NULL))

}


for (chart_type in c("P", "P'")) {
  for (i in seq_along(data_column_validation_data_yn)) {

    case <- data_column_validation_data_yn[[i]]
    y_type <- column_type(case, "y")
    n_type <- column_type(case, "n")

    test_that(paste0("column requirements: chart_type = ", chart_type,
                     " | y ", y_type, ", n ", n_type), {
      expect_conditions_of_case(
        df         = case,
        chart_type = chart_type,
        expected   = p_expectations[y_type, n_type]
      )
    })

  }
}


for (chart_type in c("XMR", "X", "MR", "C", "C'")) {
  for (i in seq_along(data_column_validation_data_y)) {

    case <- data_column_validation_data_y[[i]]
    y_type <- column_type(case, "y")

    expectations <- if(chart_type %in% c("C", "C'")) {
      c_expectations
    } else {
      x_expectations
    }

    test_that(paste0("column requirements: chart_type = ", chart_type,
                     " | y ", y_type), {
      expect_conditions_of_case(
        df         = case,
        chart_type = chart_type,
        expected   = expectations[[y_type]]
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
