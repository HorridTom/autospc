#load in test data
test_data <- readRDS("testdata/test_data.rds")

testthat::test_that("Linetypes are formed correctly", {
  
  test_plt <- plot_auto_SPC(df = test_data)
  
  layer_2 <- ggplot2::layer_data(test_plt, 2) %>% dplyr::arrange(x)
  layer_4 <- ggplot2::layer_data(test_plt, 4) %>% dplyr::arrange(x)
  
  rle_layer_2 <- rle(layer_2$linetype)
  rle_layer_4 <- rle(layer_4$linetype)
  
  correct_answer_2 <- structure(list(lengths = 150L, values = "solid"),
                                class = "rle")
  correct_answer_4 <- structure(list(lengths = 150L, values = "42"),
                                class = "rle")
  
  expect_identical(rle_layer_2, correct_answer_2)
  expect_identical(rle_layer_4, correct_answer_4)
    
})
