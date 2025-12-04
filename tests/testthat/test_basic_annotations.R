
test_that("Annotation is completed regardless of basicAnnotation", {

  result_f <- autospc(example_series_1,
                          chartType = "XMR",
                          basicAnnotations = FALSE,
                          annotation_arrows = TRUE)
  
  result_t <- autospc(example_series_1,
                            chartType = "XMR",
                            basicAnnotations = TRUE)
  
  expect_equal(result_f$data,
               result_t$data)
  
    
})
