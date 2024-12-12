test_recalc_every_shift_with_periodMin <- readRDS(
  file.path("testdata", "test_recalc_every_shift_with_periodMin.rds"))

test_that("recalc_every_shift works as intended",{
  
  results1 <- plot_auto_SPC(
    test_recalc_every_shift_with_periodMin,
    chartType = "XMR",
    recalc_every_shift = FALSE,
    plotChart = FALSE
  )
  
  results2 <- plot_auto_SPC(
    test_recalc_every_shift_with_periodMin,
    chartType = "XMR",
    recalc_every_shift = TRUE,
    plotChart = FALSE
  )
  
  recalc1 <- results1 %>%
    dplyr::pull(breakPoint) %>% 
    any(na.rm = TRUE)
  
  recalc2 <- results2 %>%
    dplyr::pull(breakPoint) %>% 
    any(na.rm = TRUE)

  recalc2_position <- results2 %>% 
    dplyr::pull(breakPoint) %>% 
    which()
  
  expect_false(recalc1)
  expect_true(recalc2)
  expect_equal(recalc2_position,
               31L)
  
})
