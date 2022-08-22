test_data <- structure(list(x = 1:21, 
                            y = c(55L, 42L, 46L, 50L, 49L, 53L, 
                                  44L, 46L, 46L, 54L, 48L, 56L, 47L, 43L, 57L, 42L, 55L, 51L, 52L, 
                                  55L, 48L), 
                            n = c(197L, 196L, 198L, 209L, 201L, 199L, 202L, 195L, 
                                  205L, 191L, 197L, 207L, 196L, 195L, 205L, 197L, 206L, 197L, 183L, 
                                  195L, 192L)), 
                       row.names = c(NA, -21L), class = "data.frame")

test_that("C chart limits the same as live qicharts2 v.0.7.2",{
  
  results <- get_c_limits(y = test_data$y)
  
  correct_answers <- qicharts2::qic(x, y, data = test_data, chart = 'c', return.data = TRUE)

  expect_equal(results$cl, correct_answers$cl)
  expect_equal(results$lcl, correct_answers$lcl)
  expect_equal(results$ucl, correct_answers$ucl)
  
})


test_that("P chart limits the same as live qicharts2 v.0.7.2",{
  
  #list
  results <- get_p_limits(y = test_data$y, n = test_data$n)

  #data frame
  correct_answers <- qicharts2::qic(x, y, n, data = test_data, chart = 'p', return.data = TRUE)
  
  expect_equal(results$cl, correct_answers$cl)
  expect_equal(results$lcl, correct_answers$lcl)
  expect_equal(results$ucl, correct_answers$ucl)
  
})


test_that("C prime chart limits the same as live qicharts2 v.0.7.2",{
  
  results <- get_cp_limits(y = test_data$y)
  
  correct_answers <- qicharts2::qic(x, y, n = rep(1, nrow(test_data)), data = test_data, chart = 'up', return.data = TRUE)
  
  expect_equal(results$cl, correct_answers$cl)
  expect_equal(results$lcl, correct_answers$lcl)
  expect_equal(results$ucl, correct_answers$ucl)
  
})


test_that("P prime chart limits the same as live qicharts2 v.0.7.2",{
  
  results <- get_pp_limits(y = test_data$y, n = test_data$n, multiply = 100)
  
  correct_answers <- qicharts2::qic(x, y, n, data = test_data, chart = 'pp', multiply = 100, return.data = TRUE)
  
  expect_equal(results$cl, correct_answers$cl)
  expect_equal(results$lcl, correct_answers$lcl)
  expect_equal(results$ucl, correct_answers$ucl)
  
})


