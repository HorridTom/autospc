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
  
  results <- plot_auto_SPC(test_data, b = y, chartType = "P", plotChart = FALSE)
  
  expect_equal(results, correct_answer_P)
  
})

test_that("P prime chart process works end to end",{
  
  results <- plot_auto_SPC(test_data, b = y, chartType = "P'", plotChart = FALSE)
  
  expect_equal(results, correct_answer_PP)
  
})


