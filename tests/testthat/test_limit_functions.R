test_data <- structure(list(x = 1:21, 
                            y = c(55.0719790229939, 42.0144192654497, 
                                  46.7157240196836, 50.6737206972748, 49.2704469456624, 53.9227469087851, 
                                  44.6134566881397, 46.7537966779583, 46.6637162904975, 54.7827753497436, 
                                  48.4303262216291, 56.784729571724, 47.6381337320164, 43.2383419657624, 
                                  57.0104366003345, 42.6992116069786, 55.2999365382173, 51.3499837954212, 
                                  52.0887995357614, 55.0599470919289, 48.1012613101909), 
                            n = c(197.873523747693, 
                                  196.231798379957, 198.317118857504, 209.170219820797, 201.853529895978, 
                                  199.500437687328, 202.531834900297, 195.29468078797, 205.427549010134, 
                                  191.448030795482, 197.210488131899, 207.373502853132, 196.425692465862, 
                                  195.906683886944, 205.824108337504, 197.884900714744, 206.908318439478, 
                                  197.239713382726, 183.992221859245, 195.844299959897, 192.038553677379
                                  )), 
                       class = "data.frame", 
                       row.names = c(NA, -21L))

testthat::test_that("C chart limits the same as live qicharts2 v.0.7.2",{
  
  results <- get_c_limits(test_data)
  results <- results %>%
    dplyr::select(cl, ucl, lcl)
  
  correct_answers <- qicharts2::qic(x, y, data = test_data, chart = 'c', return.data = TRUE)
  correct_answers <- correct_answers %>%
    dplyr::select(cl, ucl, lcl)

  testthat::expect_equal(results, correct_answers)
  
})


testthat::test_that("P chart limits the same as live qicharts2 v.0.7.2",{
  
  results <- get_p_limits(test_data)
  results <- results %>%
    dplyr::select(cl, ucl, lcl)
  
  correct_answers <- qicharts2::qic(x, y, n, data = test_data, chart = 'p', return.data = TRUE)
  correct_answers <- correct_answers %>%
    dplyr::select(cl, ucl, lcl)
  
  testthat::expect_equal(results, correct_answers)
  
})


testthat::test_that("C prime chart limits the same as live qicharts2 v.0.7.2",{
  
  results <- get_cp_limits(test_data)
  results <- results %>%
    dplyr::select(cl, ucl, lcl)
  
  correct_answers <- qicharts2::qic(x, y, n = rep(1, nrow(test_data)), data = test_data, chart = 'up', return.data = TRUE)
  correct_answers <- correct_answers %>%
    dplyr::select(cl, ucl, lcl)
  
  testthat::expect_equal(results, correct_answers)
  
})


testthat::test_that("P prime chart limits the same as live qicharts2 v.0.7.2",{
  
  results <- get_pp_limits(test_data, multiply = 100)
  results <- results %>%
    dplyr::select(cl, ucl, lcl)
  
  correct_answers <- qicharts2::qic(x, y, n, data = test_data, chart = 'pp', multiply = 100, return.data = TRUE)
  correct_answers <- correct_answers %>%
    dplyr::select(cl, ucl, lcl)
  
  testthat::expect_equal(results, correct_answers)
  
})


