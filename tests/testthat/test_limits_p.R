test_data <- structure(list(x = 1:21, 
                            y = c(55L, 42L, 46L, 50L, 49L, 53L, 44L, 46L, 46L,
                                  54L, 48L, 56L, 47L, 43L, 57L, 42L, 55L, 51L,
                                  52L, 55L, 48L), 
                            n = c(197L, 196L, 198L, 209L, 201L, 199L, 202L,
                                  195L, 205L, 191L, 197L, 207L, 196L, 195L,
                                  205L, 197L, 206L, 197L, 183L, 195L, 192L)), 
                       row.names = c(NA, -21L), class = "data.frame")

# Correct answer created using:
# test_p_limit_answer <- qicharts2::qic(x, y, n, data = test_data,
#                                       chart = 'p', return.data = TRUE)
# qicharts2 v.0.7.2
test_p_limit_answer <- readRDS(file.path("testdata",
                                         "test_p_limit_answer.rds"))

test_that("P chart limits the same as qicharts2 v.0.7.2",{

  results <- get_p_limits(y = test_data$y,
                          n = test_data$n)
  
  expect_equal(results$cl, test_p_limit_answer$cl)
  expect_equal(results$lcl, test_p_limit_answer$lcl)
  expect_equal(results$ucl, test_p_limit_answer$ucl)
  
})


# Correct answer created using:
# test_pp_limit_answer <- qicharts2::qic(x, y, n, data = test_data,
#                                         chart = 'pp', multiply = 100,
#                                         return.data = TRUE)
# qicharts2 v.0.7.2
test_pp_limit_answer <- readRDS(file.path("testdata",
                                          "test_pp_limit_answer.rds"))

test_that("P prime chart limits the same as qicharts2 v.0.7.2",{
  
  results <- get_pp_limits(y = test_data$y, n = test_data$n, multiply = 100)
  
  expect_equal(results$cl, test_pp_limit_answer$cl)
  expect_equal(results$lcl, test_pp_limit_answer$lcl)
  expect_equal(results$ucl, test_pp_limit_answer$ucl)
  
})


test_that("P chart display limits follow the denominator", {

  # The display limits of a P chart are recomputed at each point's own n, so
  # with a varying denominator they must vary too.
  varying_n <- data.frame(x = 1:30,
                          y = rep(c(10, 12, 11, 13, 9, 10), 5),
                          n = c(rep(100L, 21), rep(c(25L, 400L), 4), 25L))

  limits <- create_SPC_auto_limits_table(varying_n,
                                         chart_type = "P",
                                         period_min = 21,
                                         baseline_length = NULL,
                                         shift_rule_threshold = 8L,
                                         max_exclusions = 3,
                                         no_regrets = TRUE,
                                         verbosity = 0L,
                                         baseline_only = TRUE,
                                         establish_every_shift = FALSE,
                                         centre_line_tolerance = 0,
                                         show_limits = TRUE,
                                         overhanging_reversions = TRUE,
                                         mr_screen_max_loops = 1L)

  display <- limits[limits$periodType == "display", ]

  expect_gt(nrow(display), 0)
  expect_gt(length(unique(display$ucl)), 1)

  # a larger denominator gives narrower limits
  expect_lt(display$ucl[display$n == 400][1], display$ucl[display$n == 25][1])

  # and the centre line is carried forward unchanged
  expect_identical(length(unique(display$cl)), 1L)

})
