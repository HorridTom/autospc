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
                       row.names = c(NA, -21L)) %>%
  tibble::as_tibble()

correct_answer_C <- readRDS("testdata/test_data_end_to_end/correct_answer_C.rds")
correct_answer_CP <- readRDS("testdata/test_data_end_to_end/correct_answer_CP.rds")
correct_answer_P <- readRDS("testdata/test_data_end_to_end/correct_answer_P.rds")
correct_answer_PP <- readRDS("testdata/test_data_end_to_end/correct_answer_PP.rds")

test_that("C chart process works end to end",{
  
  results <- autospc(test_data, chart_type = "C", plot_chart = FALSE)
  results <- results %>%
    dplyr::select(x, y, ucl, lcl, cl, periodType, 
                  excluded, breakPoint, rule1, rule2, aboveOrBelowCl, 
                  highlight, limitChange, periodStart, plotPeriod)
  
  expect_equal(results, correct_answer_C)
  
})

test_that("C prime chart process works end to end",{
  
  results <- autospc(test_data, chart_type = "C'", plot_chart = FALSE)
  results <- results %>%
    dplyr::select(x, y, ucl, lcl, cl, periodType, 
                  excluded, breakPoint, rule1, rule2, aboveOrBelowCl, 
                  highlight, limitChange, periodStart, plotPeriod)
  
  expect_equal(results, correct_answer_CP)
  
})

test_that("P chart process works end to end",{
  
  results <- autospc(test_data, chart_type = "P", plot_chart = FALSE)
  results <- results %>%
    dplyr::select(x, y, n, y_numerator, ucl, lcl, cl, periodType, 
                  excluded, breakPoint, rule1, rule2, aboveOrBelowCl, 
                  highlight, limitChange, periodStart, plotPeriod)
  
  expect_equal(results, correct_answer_P)
  
})

test_that("P prime chart process works end to end",{
  
  results <- autospc(test_data, chart_type = "P'", plot_chart = FALSE)
  results <- results %>%
    dplyr::select(x, y, n, y_numerator, ucl, lcl, cl, periodType, 
                  excluded, breakPoint, rule1, rule2, aboveOrBelowCl, 
                  highlight, limitChange, periodStart, plotPeriod)
  
  expect_equal(results, correct_answer_PP)
  
})




test_that("P chart works with one binary observation per subgroup", {

  # Individual binary observations need n materialising as 1 and y coercing to
  # a count, both of which happen inside aggregation. Every proportion is 0% or
  # 100%, so the chart is degenerate - whether it deserves a chart or an
  # explanatory error is CLEAN UP #6.
  binary_data <- data.frame(
    subgroup = 1:30,
    outcome = c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE,
                TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE,
                TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE)
  )

  results <- autospc(binary_data,
                     chart_type = "P",
                     x = subgroup,
                     y = outcome,
                     period_min = 21L,
                     plot_chart = FALSE)

  expect_equal(results$y[1:5],
               c(0, 0, 100, 100, 0))

  expect_equal(results$n,
               rep(1L, 30))

  expect_equal(results$y_numerator[1:5],
               c(0L, 0L, 1L, 1L, 0L))

  # 14 of the first 21 observations are TRUE, and none is excluded
  expect_equal(results$cl[1],
               100 * 14 / 21)

})
