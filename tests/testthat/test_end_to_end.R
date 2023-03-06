test_data <- structure(list(x = 1:21, 
                            y = c(55L, 42L, 
                                  46L, 50L, 49L, 53L, 
                                  44L, 46L, 46L, 54L, 
                                  48L, 56L, 47L, 43L, 
                                  57L, 42L, 55L, 51L, 
                                  52L, 55L, 48L), 
                            n = c(197L, 
                                  196L, 198L, 209L, 201L, 
                                  199L, 202L, 195L, 205L, 
                                  191L, 197L, 207L, 196L, 
                                  195L, 205L, 197L, 206L, 
                                  197L, 183L, 195L, 192L
                            )), 
                       class = "data.frame", 
                       row.names = c(NA, -21L))

correct_answer_C <- readRDS("testdata/test_data_end_to_end/correct_answer_C.rds")
correct_answer_CP <- readRDS("testdata/test_data_end_to_end/correct_answer_CP.rds")
correct_answer_P <- readRDS("testdata/test_data_end_to_end/correct_answer_P.rds")
correct_answer_PP <- readRDS("testdata/test_data_end_to_end/correct_answer_PP.rds")

test_that("C chart process works end to end",{
  
  results <- plot_auto_SPC(test_data, chartType = "C", plotChart = FALSE)
  
  expect_equal(results, correct_answer_C)
  
})

test_that("C prime chart process works end to end",{
  
  results <- plot_auto_SPC(test_data, chartType = "C'", plotChart = FALSE)
  
  expect_equal(results, correct_answer_CP)
  
})

test_that("P chart process works end to end",{
  
  results <- plot_auto_SPC(test_data, chartType = "P", plotChart = FALSE)
  results <- results %>%
    dplyr::select(x, y, n, y_numerator, ucl, lcl, cl, periodType, 
                  excluded, breakPoint, rule1, rule2, aboveOrBelowCl, 
                  highlight, limitChange, periodStart, plotPeriod)
  expect_equal(results, correct_answer_P)
  
})

test_that("P prime chart process works end to end",{
  
  results <- plot_auto_SPC(test_data, chartType = "P'", plotChart = FALSE)
  results <- results %>%
    dplyr::select(x, y, n, y_numerator, ucl, lcl, cl, periodType, 
                  excluded, breakPoint, rule1, rule2, aboveOrBelowCl, 
                  highlight, limitChange, periodStart, plotPeriod)
  
  expect_equal(results, correct_answer_PP)
  
})


