# Regression tests for log generation and interpretation

test_data <- data.frame(x = 1L:50L, 
                        y = c(50L, 48L, 49L, 50L, 52L, 52L, 48L, 53L, 51L, 51L,
                              47L, 52L, 51L, 47L, 48L, 49L, 51L, 51L, 45L, 49L,
                              49L, 50L, 48L, 53L, 49L, 48L, 51L, 46L, 48L, 49L,
                              49L, 51L, 47L, 53L, 49L, 52L, 50L, 58L, 57L, 51L,
                              48L, 52L, 52L, 54L, 56L, 50L, 51L, 51L, 52L,
                              52L),
                        n = c(98L, 104L, 94L, 104L, 102L, 103L, 102L, 100L,
                              96L, 98L, 106L, 104L, 102L, 101L, 102L, 101L,
                              100L, 96L, 98L, 100L, 101L, 101L, 99L, 99L, 99L,
                              98L, 103L, 100L, 97L, 101L, 102L, 99L, 97L, 99L,
                              100L, 96L, 101L, 101L, 108L, 100L, 96L, 102L,
                              100L, 99L, 104L, 96L, 95L, 108L, 95L, 97L))

correct_log <- structure(list(x = c(1L, 22L, 36L),
                              log = c("0100;0200", "0300;040136",
                                      "050010;0610")),
                         row.names = c(NA, -3L),
                         class = "data.frame") %>%
  tibble::as_tibble()

correct_log_df <- structure(
  list(counter = c(1L, 1L, 2L, 2L, 3L, 3L),
       x = c(1L, 1L, 22L, 22L, 36L, 36L),
       log_entry = c("0100", "0200", "0300", "040136",
                     "050010", "0610"),
       interpretation = c("Counter initialised to 1.",
                          "Sufficient data to form at least one period.",
                          "Main algorithm loop commenced.", 
                          paste0("Sufficient data to proceed. Moving counter ",
                                 "to the next shift rule break, commencing at ",
                                 "point 36."), 
                          paste0("There is a shift rule break commencing here,",
                                 " upwards from the current centre line."), 
                          paste0("Insufficient remaining data for further ",
                                 "re-establishment of limits.")
       )),
  class = c("rowwise_df", "tbl_df", "tbl", "data.frame"),
  row.names = c(NA, -6L),
  groups = structure(list(.rows = structure(list(1L, 2L, 3L, 4L, 5L, 6L),
                                            ptype = integer(0),
                                            class = c("vctrs_list_of",
                                                      "vctrs_vctr",
                                                      "list"))),
                     row.names = c(NA, -6L), class = c("tbl_df",
                                                       "tbl",
                                                       "data.frame")))


invisible(capture.output(
  df_out <- autospc::autospc(test_data,
                                   chart_type = "P",
                                   verbosity = 2L,
                                   plot_chart = FALSE)
))

log_out <- df_out %>%
  dplyr::filter(!is.na(log)) %>%
  dplyr::select(x, log) %>%
  tibble::as_tibble()


test_that("log is populated correctly (regression)", {
  
  expect_equal(log_out,
               correct_log)
  
})


test_that("log is interpreted correctly (regression)", {
  
  log_df <- autospc:::create_log_dataframe(log_out,
                                           verbosity = 2L)
  
  expect_equal(log_df,
               correct_log_df)
  
})


test_that("specific log entries are interpreted correctly", {
  
  interpret_0210 <- interpret_log_entry("0210",
                                        verbosity = 2)
  
  interpret_0410 <- interpret_log_entry("0410",
                                        verbosity = 2)
  
  interpret_050001 <- interpret_log_entry("050001",
                                          verbosity = 2)
  
  interpret_0510 <- interpret_log_entry("0510",
                                        verbosity = 2)
  
  interpret_060011 <- interpret_log_entry("060011",
                                          verbosity = 2)
  
  interpret_0700 <- interpret_log_entry("0700",
                                        verbosity = 2)
  
  interpret_0710 <- interpret_log_entry("0710",
                                        verbosity = 2)
  
  expect_equal(interpret_0210,
               "Insufficient data to form control limits.")
  
  expect_equal(interpret_0410,
               paste("Insufficient remaining data for further",
                     "re-establishment of limits."))
  
  expect_equal(interpret_050001,
               paste("There is a shift rule break commencing here,",
                     "downwards from the current centre line."))
  
  expect_equal(interpret_0510,
               "There are no subsequent shift rule breaks.")
  
  expect_equal(interpret_060011,
               paste("Sufficient data to proceed. Forming candidate limits.",
                     "There is a shift rule break back towards the prevailing",
                     "centre line. The final run in the candidate calculation",
                     "period may become a shift rule break back towards the",
                     "prevailing centre line."))
  
  expect_equal(interpret_0700,
               "Candidate limits accepted, limits re-established.")
  
  expect_equal(interpret_0710,
               paste("Candidate limits rejected, prevailing limits",
                     "retained."))
  
})


# The log column is built by render_log() from the chart's history.

test_that("the log records the algorithm's decisions", {

  analyse <- function(data, ...) {
    suppressWarnings(run_limit_algorithm(prepare_data(
      autospc_chart(chart_type = "C\'", data = data, x = "x", y = "y", ...))))
  }

  entries <- function(chart) {
    log_column <- chart$result$table$log
    stats::setNames(log_column[!is.na(log_column)], which(!is.na(log_column)))
  }

  expect_identical(
    entries(analyse(example_series_2a)),
    c("1" = "0100;0200",
      "22" = "0300;040122;050010;060001;0710",
      "23" = "040023;050010;060000;0700"))

  expect_identical(
    entries(analyse(example_series_2c)),
    c("1" = "0100;0200",
      "22" = "0300;040122;050010;060011;0710",
      "23" = "040023;050010;060011;0710",
      "24" = "040024;050010;060011;0710",
      "25" = "040133",
      "33" = "050010;0610"))

  # too few points to form even one period
  too_short <- data.frame(x = 1:10,
                          y = c(10, 14, 11, 16, 12, 13, 15, 11, 14, 12))

  expect_identical(entries(analyse(too_short)), c("1" = "0100;0210"))

})


test_that("the log covers the other chart types", {

  entries <- function(chart) {
    log_column <- chart$result$table$log
    stats::setNames(log_column[!is.na(log_column)], which(!is.na(log_column)))
  }

  # a scan that finds no break records the position as NA
  proportions <- data.frame(x = 1:60,
                            y = rep(c(10, 12, 11, 13, 9, 10), 10),
                            n = rep(100L, 60))
  fitted_p <- run_limit_algorithm(prepare_data(
    autospc_chart(chart_type = "P", data = proportions,
                  x = "x", y = "y", n = "n")))

  expect_identical(entries(fitted_p),
                   c("1" = "0100;0200", "22" = "0300;0401NA;0510"))

  # 050001 is a break below the centre line, 050010 above
  ed <- data.frame(x = ed_attendances_monthly$month_start,
                   y = ed_attendances_monthly$att_all)
  fitted_mr <- run_limit_algorithm(prepare_data(
    autospc_chart(chart_type = "MR", data = ed, x = "x", y = "y")))

  expect_identical(entries(fitted_mr),
                   c("1" = "0100;0200",
                     "22" = "0300;040157",
                     "57" = "050010;060010;0710",
                     "58" = "040058;050010;060010;0710",
                     "59" = "040197",
                     "97" = "050001;0610"))

})


test_that("an entry past the end of the table is held at the last row", {

  # exactly period_min points, so the counter runs one past the end
  exact <- data.frame(x = 1:21,
                      y = c(10, 12, 11, 13, 9, 10, 12, 11, 13, 9, 10,
                            12, 11, 13, 9, 10, 12, 11, 13, 9, 10))

  analysed <- suppressWarnings(run_limit_algorithm(prepare_data(
    autospc_chart(chart_type = "C\'", data = exact, x = "x", y = "y",
                  period_min = 21L))))

  expect_identical(analysed$result$table$log[21], "co@22|0300")

})
