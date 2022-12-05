#load in test data
test_data <- structure(list(x = 1:40, y = c(49.4550391796525, 50.9836438579084, 
                                            50.6914635178602, 50.0729750193407, 48.6495109804686, 49.626107725251, 
                                            50.6211278347098, 49.0978404385377, 50.0460305043023, 47.4266159951451, 
                                            49.656313380928, 50.7784258114023, 50.0494040596126, 48.1606254179084, 
                                            50.1821246999236, 50.8964273644261, 51.416332316312, 52.0612464592402, 
                                            48.2870311333243, 49.8679700848073, 49.7788804539827, 50.5095182483303, 
                                            49.0002986750213, 49.6277227769633, 49.0872898053726, 50, 50, 
                                            50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50)), class = "data.frame", row.names = c(NA, 
                                                                                                                                      -40L))

test_that("Runs on the centre line do not show as rule 2 breaks with tolerance",{
  
  result <- plot_auto_SPC(test_data, plotChart = FALSE, rule2Tolerance = 0.1)
  result <- result$rule2
  
  correct_answer <- rep(FALSE, 40)
  
  testthat::expect_equal(result, correct_answer)
  
})