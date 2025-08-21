library(testthat)

# load in test data
test_data <- readRDS("testdata/test_individual_data.rds")
# Correct answer created using:
# test_individual_answer <- qicharts2::qic(x, y, data = test_data, chart = 'i',
#                                           return.data = TRUE)
# qicharts2 v.0.7.2
test_individual_answer <- readRDS(file.path("testdata",
                                            "test_individual_answer.rds"))

# test that the i chart limits is the same as qicharts2 results 
test_that("I chart limits the same as live qicharts2 v.0.7.2",{
  results <- get_i_limits(y = test_data$y,
                          mr_screen_max_loops = 1)
  
  expect_equal(results$cl, test_individual_answer$cl)
  expect_equal(results$lcl, test_individual_answer$lcl)
  expect_equal(results$ucl, test_individual_answer$ucl)
  
})

# test that moving range limits that are above ucl_mr are removed when
# mr_screen_max_loops = 1
extreme_mr_data <- readRDS("testdata/test_mr_data.rds")
# Correct answer created using:
# test_mr_answer <- qicharts2::qic(x, y, data = extreme_mr_data, chart = 'i',
#                                   return.data = TRUE)
# qicharts2 v.0.7.2
test_mr_answer <- readRDS(file.path("testdata",
                                    "test_mr_answer.rds"))

test_that("I chart limits the same as live qicharts2 v.0.7.2",{
  results <- get_i_limits(y = extreme_mr_data$y,
                          mr_screen_max_loops = 1)
  
  expect_equal(results$cl, test_mr_answer$cl)
  expect_equal(results$lcl, test_mr_answer$lcl)
  expect_equal(results$ucl, test_mr_answer$ucl)
  
})

# ...and that they are not when mr_screen_max_loops = 0
test_that("I chart limits correct without mr screening",{
  results <- get_i_limits(y = extreme_mr_data$y,
                          mr_screen_max_loops = 0)
  
  expect_equal(results$cl,
               rep(85.55363,
                   length(results$cl)),
               tolerance = 1e-3)
  expect_equal(results$lcl,
               rep(45.0676,
                   length(results$lcl)),
               tolerance = 1e-3)
  expect_equal(results$ucl,
               rep(126.0396,
                   length(results$ucl)),
               tolerance = 1e-3)
  
})
