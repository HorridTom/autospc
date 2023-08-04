#test that the XMR chart process works end to end 
correct_answer_I <- readRDS("testdata/test_data_end_to_end/correct_answer_I.rds")
test_e2e_data <- readRDS("testdata/test_e2e_data.rds")

test_that("XMR chart process works end to end",{
  
  results <- plot_auto_SPC(test_e2e_data, chartType = "XMR", plotChart = FALSE)
  
  expect_equal(results, correct_answer_I)
  
})
