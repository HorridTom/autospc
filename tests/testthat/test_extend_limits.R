test_data <- structure(list(x = 1:21, 
                            y = c(55L, 42L, 46L, 50L, 49L, 53L, 44L, 46L, 46L,
                                  54L, 48L, 56L, 47L, 43L, 57L, 42L, 55L, 51L, 
                                  52L, 55L, 48L), 
                            n = c(197L, 196L, 198L, 209L, 201L, 199L, 202L,
                                  195L, 205L, 191L, 197L, 207L, 196L, 195L,
                                  205L, 197L, 206L, 197L, 183L, 195L, 192L
                            )), 
                       class = "data.frame", 
                       row.names = c(NA, -21L))

test_that("Limit extension works correctly for C chart", {
  results_nex <- autospc(test_data,
                               chart_type = "C",
                               plotChart = FALSE)
  
  results_ext <- autospc(test_data,
                               chart_type = "C",
                               plotChart = FALSE,
                               extend_limits_to = 35L)
  
  # Get the correct values for the centre line and limits (since this is a C
  # chart with only one period, all three values are constant throughout, so we
  # can get the correct limit values for the extension from, e.g., the last row)
  limit_values <- results_nex %>%
    dplyr::filter(dplyr::row_number() == nrow(test_data)) %>% 
    dplyr::select(cl, lcl, ucl)
  
  # Limit extension should result in two additional rows in the result, marking
  # the start and end of the extension period, with the correct limits and cl
  expect_equal(results_ext %>%
                 dplyr::filter(dplyr::row_number() == nrow(test_data) + 1L) %>%
                 dplyr::select(cl, lcl, ucl),
               limit_values)
  
  expect_equal(results_ext %>%
                 dplyr::filter(dplyr::row_number() == nrow(test_data) + 2L) %>%
                 dplyr::select(cl, lcl, ucl),
               limit_values)
  
  expect_equal(results_ext %>%
                 dplyr::filter(dplyr::row_number() == nrow(test_data) + 1L) %>%
                 dplyr::pull(plotPeriod) %>%
                 stringr::str_extract("^[a-z]*"),
               "display")
  
  expect_equal(results_ext %>%
                 dplyr::filter(dplyr::row_number() == nrow(test_data) + 2L) %>%
                 dplyr::pull(plotPeriod) %>%
                 stringr::str_extract("^[a-z]*"),
               "display")
  
})


test_that("Limit extension works correctly for P chart", {
  results_nex <- autospc(test_data,
                               chart_type = "P",
                               plotChart = FALSE)
  
  results_ext <- autospc(test_data,
                               chart_type = "P",
                               plotChart = FALSE,
                               extend_limits_to = 35L)
  
  # Get the correct values for the centre line and limits
  aggregates <- test_data %>% 
    dplyr::summarise(y = sum(y),
                     n = sum(n),
                     m = dplyr::n()) %>% 
    dplyr::mutate(pbar = y/n,
                  nbar = n/m) %>% 
    dplyr::select(pbar,
                  nbar)
  
  pbar <- aggregates$pbar
  nbar <- aggregates$nbar
  
  lcl <- pbar - 3*sqrt((pbar*(1 - pbar))/nbar)
  ucl <- pbar + 3*sqrt((pbar*(1 - pbar))/nbar)
  
  limit_values <- tibble::tibble_row(cl = pbar * 100,
                                     lcl = lcl * 100,
                                     ucl = ucl * 100) %>% 
    as.data.frame()
  
  # Limit extension should result in two additional rows in the result, marking
  # the start and end of the extension period, with the correct limits and cl
  expect_equal(results_ext %>%
                 dplyr::filter(dplyr::row_number() == nrow(test_data) + 1L) %>%
                 dplyr::select(cl, lcl, ucl),
               limit_values)
  
  expect_equal(results_ext %>%
                 dplyr::filter(dplyr::row_number() == nrow(test_data) + 2L) %>%
                 dplyr::select(cl, lcl, ucl),
               limit_values)
  
  expect_equal(results_ext %>%
                 dplyr::filter(dplyr::row_number() == nrow(test_data) + 1L) %>%
                 dplyr::pull(plotPeriod) %>%
                 stringr::str_extract("^[a-z]*"),
               "display")
  
  expect_equal(results_ext %>%
                 dplyr::filter(dplyr::row_number() == nrow(test_data) + 2L) %>%
                 dplyr::pull(plotPeriod) %>%
                 stringr::str_extract("^[a-z]*"),
               "display")
  
})


test_extend_limits_pp_answer <- readRDS(
  "testdata/test_extend_limits_pp_answer.rds")

test_that("Limit extension works correctly for P-prime chart (regression)", {
  
  results_ext <- autospc(test_data,
                               chart_type = "P'",
                               plotChart = FALSE,
                               extend_limits_to = 35L) %>%
    dplyr::select(x,
                  y,
                  n,
                  y_numerator,
                  ucl,
                  lcl,
                  cl)
  
  test_extend_limits_pp_answer <- test_extend_limits_pp_answer %>%
    dplyr::select(x,
                  y,
                  n,
                  y_numerator,
                  ucl,
                  lcl,
                  cl)
  
  expect_equal(results_ext,
               test_extend_limits_pp_answer)
  
  
})
