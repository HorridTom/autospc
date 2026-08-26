#load in test data
test_data <- structure(list(x = 1:40,
                            y = c(49, 51, 51, 50, 49, 50, 51, 49, 50, 47, 50,
                                  51, 50, 48, 50, 51, 51, 52, 48, 50, 50, 51,
                                  49, 50, 49, 50, 50, 50, 50, 50, 50, 50, 50,
                                  50, 50, 50, 50, 50, 50, 50)),
                       class = "data.frame", row.names = c(NA, -40L))

test_that("Runs on the centre line do not show as rule 2 breaks with tolerance",
          {
            result <- autospc(test_data,
                              chart_type = "C'",
                              plot_chart = FALSE, 
                              centre_line_tolerance = 0.11)
            result <- result$rule2
            
            correct_answer <- rep(FALSE, 40)
            
            testthat::expect_equal(result, correct_answer)
            
          })
