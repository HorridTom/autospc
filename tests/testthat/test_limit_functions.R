test_data <- structure(list(x = 1:21, 
                            y = c(55L, 42L, 46L, 50L, 49L, 53L, 44L, 46L, 46L,
                                  54L, 48L, 56L, 47L, 43L, 57L, 42L, 55L, 51L,
                                  52L, 55L, 48L), 
                            n = c(197L, 196L, 198L, 209L, 201L, 199L, 202L,
                                  195L, 205L, 191L, 197L, 207L, 196L, 195L,
                                  205L, 197L, 206L, 197L, 183L, 195L, 192L)), 
                       row.names = c(NA, -21L), class = "data.frame")

# Correct answer created using:
# test_c_limit_answer <- qicharts2::qic(x, y, data = test_data, chart = 'c',
#                                         return.data = TRUE)
# qicharts2 v.0.7.2
test_c_limit_answer <- readRDS(file.path("testdata",
                                         "test_c_limit_answer.rds"))

test_that("C chart limits the same as qicharts2 v.0.7.2",{
  
  results <- get_c_limits(y = test_data$y)
  
  expect_equal(results$cl, test_c_limit_answer$cl)
  expect_equal(results$lcl, test_c_limit_answer$lcl)
  expect_equal(results$ucl, test_c_limit_answer$ucl)
  
})


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
# test_cp_limit_answer <- qicharts2::qic(x, y, n = rep(1, nrow(test_data)),
#                                         data = test_data, chart = 'up',
#                                         return.data = TRUE)
# qicharts2 v.0.7.2
test_cp_limit_answer <- readRDS(file.path("testdata",
                                         "test_cp_limit_answer.rds"))

test_that("C prime chart limits the same as qicharts2 v.0.7.2",{
  
  results <- get_cp_limits(y = test_data$y)
  
  expect_equal(results$cl, test_cp_limit_answer$cl)
  expect_equal(results$lcl, test_cp_limit_answer$lcl)
  expect_equal(results$ucl, test_cp_limit_answer$ucl)
  
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

