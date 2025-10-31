
test_that("Annotation is completed regardless of basicAnnotation", {

  result_f <- plot_auto_SPC(example_series_1,
                          chartType = "XMR",
                          basicAnnotations = FALSE)
  
  result_t <- plot_auto_SPC(example_series_1,
                            chartType = "XMR",
                            basicAnnotations = TRUE)
  
  expect_equal(result_f$data,
               result_t$data)
  
    
})