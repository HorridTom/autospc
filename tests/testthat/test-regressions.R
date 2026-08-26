# Faults that were reported, fixed, and must stay fixed. Each test names the
# issue it came from; the data that provoked it is in testdata/.

bug_90_data <- readRDS(file.path("testdata", "mnoe_bug_data.rds"))
bug_148_data <- readRDS(file.path("testdata", "bug_148_data.rds"))
bug_154_data <- readRDS(file.path("testdata", "bug_154_data.rds"))
calculation_period_only_data <- readRDS(file.path("testdata",
                                                  "test_e2e_data.rds"))


test_that("Bug 90 is fixed: period_min respected even with NAs", {

  # The mnoe_bug_data has missing data such that there is
  # insufficient data to re-establish limits
  result_with_missing_data <- autospc(bug_90_data,
                                      chart_type = "P'",
                                      period_min = 24L,
                                      max_exclusions = 0L,
                                      plot_chart = FALSE)

  # Establish the number of (non-missing) data points within each calculation
  # period in the algorithm results
  calc_period_lengths <- result_with_missing_data %>%
    dplyr::filter(!is.na(y),
                  period_type == "calculation") %>%
    dplyr::group_by(plot_period) %>%
    dplyr::summarise(period_length = dplyr::n())

  # Check whether each calculation period is compliant with the specified
  # period_min = 24
  period_min_compliant <- calc_period_lengths %>%
    dplyr::mutate(compliant = period_length >= 24L) %>%
    dplyr::pull(compliant)

  # All calculation periods should be compliant in this way
  expect_true(all(period_min_compliant))

})


test_that("Bug 148 is fixed: no error", {

  expect_no_error(
    autospc(bug_148_data,
            chart_type = "C",
            plot_chart = FALSE),
    message = "`opposite_break` must be size"
  )

})


test_that(paste0("break points are inserted even with no change in centre line",
                 "(bug 149 fixed)"), {

  # bug 149 was captured using the same data as bug 148
  result <- autospc(bug_148_data,
                    chart_type = "C",
                    plot_chart = FALSE)

  num_breakpoints = sum(result %>%
                          dplyr::filter(dplyr::row_number()
                                        != 1L) %>%
                          dplyr::pull(break_point))

  expect_equal(num_breakpoints,
               1L)

})


test_that("Bug 154 is fixed: rule columns populate correctly", {

  results_1 <- autospc(bug_154_data %>%
                         dplyr::filter(x <= 287L),
                       chart_type = "X",
                       plot_chart = FALSE)

  results_2 <- autospc(bug_154_data %>%
                         dplyr::filter(x <= 288L),
                       chart_type = "X",
                       plot_chart = FALSE)

  rule2_NAs_1 <- results_1 %>%
    dplyr::summarise(num_nas = sum(is.na(rule2))) %>%
    dplyr::pull(num_nas)

  rule2_NAs_2 <- results_2 %>%
    dplyr::summarise(num_nas = sum(is.na(rule2))) %>%
    dplyr::pull(num_nas)

  expect_equal(rule2_NAs_1,
               0L)

  expect_equal(rule2_NAs_2,
               0L)

})


test_that("calculation period only chart is created without warning", {

  plot_1 <- autospc(calculation_period_only_data,
                    chart_type = "XMR",
                    plot_chart = TRUE,
                    include_annotations = FALSE)

  expect_no_warning(drawn(plot_1),
                    message = "containing missing values"
  )

})


test_that("autospc does not throw an error with integer x values", {

  # Example data
  df1 <- structure(list(x = 1:50,
                        y = c(49, 70, 44, 43, 75, 60, 47, 65,
                              63, 62, 55, 57, 51, 49, 55, 76, 58, 51, 65, 52,
                              48, 60, 65, 71, 70, 68, 43, 76, 98, 108, 79, 92,
                              84, 76, 69, 88, 83, 101, 81, 72, 89, 90, 81, 82,
                              68, 82, 84, 93, 79, 87)),
                   row.names = c(NA, -50L),
                   class = c("tbl_df", "tbl", "data.frame")) %>%
    dplyr::mutate(y = as.integer(y))

  # Expect no error when calling autospc with this data
  expect_error(
    autospc(df1,
            chart_type = "C'",
            title = "my title",
            subtitle = "my subtitle"),
    NA)

})
