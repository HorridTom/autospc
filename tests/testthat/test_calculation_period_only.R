test_data <- readRDS("testdata/test_e2e_data.rds")

test_that("calculation period only chart is created without warning", {
  
  plot_1 <- plot_auto_SPC(test_data,
                          chartType = "XMR",
                          plotChart = TRUE)
  
  expect_no_warning(print(plot_1),
                    #message = "containing missing values"
  )
})
