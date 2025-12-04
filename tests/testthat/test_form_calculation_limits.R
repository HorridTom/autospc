#load in test data
test_data <- readRDS("testdata/test_data.rds")

test_that("Calculation period is formed correctly",{
  
  result_counter_one <- form_calculation_limits(test_data,
                                                counter = 1,
                                                period_min = 21,
                                                baseline = NULL,
                                                rule2Tolerance = 0,
                                                shift_rule_threshold = 8)
  result_counter_one_ucl <- result_counter_one$ucl[1:21]
  result_counter_one_cl <- result_counter_one$cl[1:21]
  result_counter_one_lcl <- result_counter_one$lcl[1:21]
  
  result_counter_100 <- form_calculation_limits(result_counter_one,
                                                counter = 100,
                                                period_min = 21,
                                                baseline = NULL,
                                                rule2Tolerance = 0,
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
