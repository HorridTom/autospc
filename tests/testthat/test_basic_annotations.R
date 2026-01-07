
test_that("Annotation is completed regardless of basicAnnotation", {

  result_f <- autospc(example_series_1,
                          chart_type = "XMR",
                          basic_annotations = FALSE,
                          annotation_arrows = TRUE)
  
  result_t <- autospc(example_series_1,
                            chart_type = "XMR",
                            basic_annotations = TRUE)
  
  expect_equal(result_f$data,
               result_t$data)
  
    
})
