bug_148_data <- readRDS(file.path("testdata", "bug_148_data.rds"))


test_that("Bug 148 is fixed: no error", {
  
  expect_no_error(
    plot_auto_SPC(bug_148_data,
                  chartType = "C",
                  plotChart = FALSE),
    message = "`oppositeBreak` must be size"
  )
  
})