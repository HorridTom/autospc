test_comparator_approaches_data <- readRDS(
  file.path("testdata", "test_comparator_approaches_data.rds"))

get_breakpoints <- function(df) {
  return(df %>%
           dplyr::pull(breakPoint) %>%
           which())
}

test_that("comparator approaches calculate correctly",{
  
  p1 <- plot_auto_SPC(test_comparator_approaches_data,
                      chartType = "XMR",
                      noPeriodMin = TRUE,
                      recalc_every_shift = TRUE,
                      showMR = FALSE,
                      plotChart = FALSE)
  
  p2 <- plot_auto_SPC(test_comparator_approaches_data,
                      chartType = "XMR",
                      noPeriodMin = FALSE,
                      recalc_every_shift = TRUE,
                      showMR = FALSE,
                      plotChart = FALSE)
  
  p3 <- plot_auto_SPC(test_comparator_approaches_data,
                      chartType = "XMR",
                      noRegrets = FALSE,
                      overhangingReversions = FALSE,
                      showMR = FALSE,
                      plotChart = FALSE)
  
  p4 <- plot_auto_SPC(test_comparator_approaches_data,
                      chartType = "XMR",
                      noRegrets = TRUE,
                      showMR = FALSE,
                      plotChart = FALSE)
  
  breakpoints <- lapply(list(p1,
                             p2,
                             p3,
                             p4),
                        get_breakpoints)
  
  expect_equal(breakpoints[[1]],
               c(22L, 42L, 61L, 75L))
  expect_equal(breakpoints[[2]],
               c(22L, 43L, 64L))
  expect_equal(breakpoints[[3]],
               c(22L, 43L))
  expect_equal(breakpoints[[4]],
               c(22L))
  
})
