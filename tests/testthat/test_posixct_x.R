
POSIXct_x_data <- readRDS(file.path("testdata",
                                    "test_POSIXct_x.rds"))

test_that("Chart is created without error when x-axis is POSIXct", {
  
  expect_no_error(
    plot_auto_SPC(POSIXct_x_data,
                  chartType = "C'",
                  x = Month,
                  y = Total),
    message = "works with objects of class <Date> only"
  )
  
})