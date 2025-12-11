bug_148_data <- readRDS(file.path("testdata", "bug_148_data.rds"))


test_that("Bug 148 is fixed: no error", {
  
  expect_no_error(
    autospc(bug_148_data,
                  chart_type = "C",
                  plot_chart = FALSE),
    message = "`oppositeBreak` must be size"
  )
  
})
