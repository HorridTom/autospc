context("Test linetypes are formed correctly")

#load in test data
test_data <- readRDS("testdata/test_data.rds")

testthat::test_that("Linetypes are formed correctly", {
  
  test_plt <- plot_auto_SPC(df = test_data)
  
  layer_2 <- ggplot2::layer_data(test_plt, 2) %>% dplyr::arrange(x)
  layer_3 <- ggplot2::layer_data(test_plt, 3) %>% dplyr::arrange(x)
  layer_4 <- ggplot2::layer_data(test_plt, 4) %>% dplyr::arrange(x)
  
  rle_layer_2 <- rle(layer_2$linetype)
  rle_layer_3 <- rle(layer_3$linetype)
  rle_layer_4 <- rle(layer_4$linetype)
  
  correct_answer <- structure(
    list(lengths = c(21L, 29L, 22L, 47L, 22L, 9L),
         values = c(1L, 2L, 1L, 2L, 1L, 2L)),
    class = "rle")   
  
  expect_identical(rle_layer_2, correct_answer)
  expect_identical(rle_layer_3, correct_answer)
  expect_identical(rle_layer_4, correct_answer)
    
})
