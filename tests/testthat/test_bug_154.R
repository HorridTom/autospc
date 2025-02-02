bug_148_data <- readRDS(file.path("testdata",
                                  "bug_154_data.rds"))

test_that("Bug 154 is fixed: rule columns populate correctly", {
  
  results_1 <- plot_auto_SPC(bug_148_data %>%
                               dplyr::filter(x <= 287L),
                             chartType = "XMR",
                             plotChart = FALSE,
                             showMR = FALSE)
  
  results_2 <- plot_auto_SPC(bug_148_data %>%
                               dplyr::filter(x <= 288L),
                             chartType = "XMR",
                             plotChart = FALSE,
                             showMR = FALSE)
  
  rule2_NAs_1 <- results_1 %>% 
    dplyr::summarise(num_nas = sum(is.na(rule2))) %>% 
    dplyr::pull(num_nas)
  
  rule2_NAs_2 <- results_2 %>% 
    dplyr::summarise(num_nas = sum(is.na(rule2))) %>% 
    dplyr::pull(num_nas)
  
  expect_equal(rule2_NAs_1,
               0L)
  
  expect_equal(rule2_NAs_2,
               0L)
  
})
