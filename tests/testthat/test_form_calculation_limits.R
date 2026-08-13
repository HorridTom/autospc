#load in test data
test_data <- readRDS("testdata/test_data.rds")

# form_calculation_limits() gets which limits to calculate from the chart
# object, so one has to be supplied. These figures are C chart limits.
test_chart <- autospc_chart_c(data = test_data, x = "x", y = "y")

test_that("Calculation period is formed correctly",{
  
  result_counter_one <- form_calculation_limits(test_data,
                                                chart = test_chart,
                                                counter = 1,
                                                period_min = 21,
                                                baseline_length = NULL,
                                                centre_line_tolerance = 0,
                                                shift_rule_threshold = 8)
  result_counter_one_ucl <- result_counter_one$ucl[1:21]
  result_counter_one_cl <- result_counter_one$cl[1:21]
  result_counter_one_lcl <- result_counter_one$lcl[1:21]
  
  result_counter_100 <- form_calculation_limits(result_counter_one,
                                                chart = test_chart,
                                                counter = 100,
                                                period_min = 21,
                                                baseline_length = NULL,
                                                centre_line_tolerance = 0,
                                                shift_rule_threshold = 8)
  result_counter_100_ucl <- result_counter_100$ucl[100:120]
  result_counter_100_cl <- result_counter_100$cl[100:120]
  result_counter_100_lcl <- result_counter_100$lcl[100:120]
  
  correct_answer_counter_one_ucl <- rep(93.24504, 21)
  correct_answer_counter_one_cl <- rep(68.428571, 21)
  correct_answer_counter_one_lcl <- rep(43.612102, 21)

  correct_answer_counter_100_ucl <- rep(58.230826, 21)
  correct_answer_counter_100_cl <- rep(39.40000, 21)
  correct_answer_counter_100_lcl <- rep(20.569174, 21)
  
  correct_answer_counter_100_ucl
  correct_answer_counter_100_cl
  correct_answer_counter_100_lcl
  
  
  testthat::expect_equal(result_counter_one_ucl,correct_answer_counter_one_ucl)
  testthat::expect_equal(result_counter_one_cl,correct_answer_counter_one_cl)
  testthat::expect_equal(result_counter_one_lcl,correct_answer_counter_one_lcl)
  
  testthat::expect_equal(result_counter_100_ucl,correct_answer_counter_100_ucl)
  testthat::expect_equal(result_counter_100_cl,correct_answer_counter_100_cl)
  testthat::expect_equal(result_counter_100_lcl,correct_answer_counter_100_lcl)
  
})


test_that("the limits table keeps n and y_numerator for P charts only", {

  # which columns survive is decided by limits_table_columns(), a method on the
  # chart object. This is the behavioural end of that: the P table has to carry
  # the counts and denominators the limits were calculated from, and the count
  # charts must not gain columns they have no use for.
  proportion_data <- data.frame(x = 1:30,
                                y = rep(c(3, 4, 2, 5, 3, 4), 5),
                                n = rep(20L, 30))

  limits_args <- list(period_min = 21,
                      baseline_length = NULL,
                      shift_rule_threshold = 8L,
                      max_exclusions = 3,
                      no_regrets = TRUE,
                      verbosity = 0L,
                      baseline_only = FALSE,
                      establish_every_shift = FALSE,
                      centre_line_tolerance = 0,
                      show_limits = TRUE,
                      overhanging_reversions = TRUE,
                      mr_screen_max_loops = 1L)

  p_table <- do.call(create_SPC_auto_limits_table,
                     c(list(data = proportion_data, chart_type = "P"),
                       limits_args))

  expect_true(all(c("n", "y_numerator") %in% names(p_table)))

  c_table <- do.call(create_SPC_auto_limits_table,
                     c(list(data = proportion_data, chart_type = "C"),
                       limits_args))

  expect_false(any(c("n", "y_numerator") %in% names(c_table)))

})
