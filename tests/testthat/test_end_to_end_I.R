# test that the XMR chart process works end to end
#
# correct_answer_XMR was generated using a specific set of data:
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
# This data was now run through qic2 to generate the cl, ucl and lcl,
# as well as mr, amr, lrl and url: 
# correct_qic2_answers_I <- qicharts2::qic(x,
#                                          y,
#                                          data = test_e2e_data,
#                                          chart = 'i',
#                                          return.data = TRUE)
# correct_qic2_answers_MR <- qicharts2::qic(x,
#                                           y,
#                                           data = test_e2e_data,
#                                           chart = 'mr',
#                                           return.data = TRUE)
# 
# correct_qic2_answers_XMR <- correct_qic2_answers_I %>%
#   dplyr::left_join(correct_qic2_answers_MR %>%
#                      dplyr::select(x,
#                                    mr = y,
#                                    amr = cl,
#                                    url = ucl,
#                                    lrl = lcl),
#                    by = c("x" = "x")) %>%
#   dplyr::select(x, y, cl, ucl, lcl,
#                 mr, amr, url, lrl) %>%
# tibble::as_tibble()
# 
# # Correct for fact that autospc doesn't provide limits where no mR
# correct_qic2_answers_XMR <- correct_qic2_answers_XMR %>%
#   dplyr::mutate(url = dplyr::if_else(is.na(mr), NA_real_, url),
#                 lrl = dplyr::if_else(is.na(mr), NA_real_, lrl))
#

correct_answer_XMR <- readRDS(file.path("testdata",
                                        "test_data_end_to_end",
                                        "correct_answer_XMR.rds"))

test_e2e_data <- readRDS("testdata/test_e2e_data.rds")

test_that("XMR chart process works end to end",{
  
  results <- autospc(test_e2e_data,
                           chartType = "XMR",
                           plotChart = FALSE,
                           title = "XMR Chart",
                           subtitle = "Chart subtitle") %>% 
    dplyr::select(x, y, cl, ucl, lcl,
                  mr, amr, url, lrl)
  
  expect_equal(results, correct_answer_XMR)
  
})


test_that("XMR chart works with numeric y",{
  
  results <- autospc(test_e2e_data %>%
                             dplyr::mutate(y = as.numeric(y)),
                           chartType = "XMR",
                           plotChart = FALSE) %>% 
    dplyr::select(x, y, cl, ucl, lcl,
                  mr, amr, url, lrl)
  
  expect_equal(results, correct_answer_XMR)
  
})
