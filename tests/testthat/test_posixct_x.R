
POSIXct_x_data <- readRDS(file.path("testdata",
                                    "test_POSIXct_x.rds"))

test_that("Chart is created without error when x-axis is POSIXct", {
  
  expect_no_error(
    plot_auto_SPC(POSIXct_x_data,
                  chartType = "C'",
                  x = Month,
                  y = Total),
    message = "the condition has length"
  )
  
})


test_that("x_break argument accepts difftime when x-axis is POSIXct", {
  
  expect_no_error(
    plot_auto_SPC(POSIXct_x_data,
                  chartType = "C'",
                  x = Month,
                  y = Total,
                  x_break = as.difftime(6, units = "weeks")),
    message = "the condition has length"
  )
  
})


test_that("Error thrown when x_break not difftime and x-axis is POSIXct", {
  
  expect_error(
    plot_auto_SPC(POSIXct_x_data,
                  chartType = "C'",
                  x = Month,
                  y = Total,
                  x_break = 6),
    regexp = "x_break as a difftime"
  )
  
})
