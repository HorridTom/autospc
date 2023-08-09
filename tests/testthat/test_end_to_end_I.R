# test that the XMR chart process works end to end
#
# correct_answer_I was generated using a specific set of data:
#   load_e2e_data <- function(){
#     set.seed(1605)
#     e2e_values <- rnorm(21,
#                         mean = 90,
#                         sd = 15)
#     e2e_df <- tibble::tibble(x = c(1:21),
#                                y = e2e_values)
#     return(e2e_df)
#   }
# test_e2e_data <- load_e2e_data()
#
# df was now run through qic2 to generate the cl, ucl and lcl: 
# correct_qic2_answers_I <- qicharts2::qic(x,
#                                          y,
#                                          data = test_e2e_data,
#                                          chart = 'i',
#                                          return.data = TRUE)
#
# df was also run through autospc:
# correct_answer_I <- plot_auto_SPC(test_e2e_data, chartType = "XMR", plotChart = FALSE)
#
# after the cl, ucl and lcl were manually checked between autospc and qic2 to be exactly the same 
# the autospc generated correct_answer_I was saved as the correct_answer_I RDS file

correct_answer_I <- readRDS("testdata/test_data_end_to_end/correct_answer_I.rds")
test_e2e_data <- readRDS("testdata/test_e2e_data.rds")

test_that("XMR chart process works end to end",{
  
  results <- plot_auto_SPC(test_e2e_data, chartType = "XMR", plotChart = FALSE)
  
  expect_equal(results, correct_answer_I)
  
})
