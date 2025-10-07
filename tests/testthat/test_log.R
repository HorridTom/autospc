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
                         class = "data.frame")

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
  df_out <- autospc::plot_auto_SPC(test_data,
                                   chartType = "P",
                                   verbosity = 2L,
                                   plotChart = FALSE)
))

log_out <- df_out %>%
  dplyr::filter(!is.na(log)) %>%
  dplyr::select(x, log)


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

