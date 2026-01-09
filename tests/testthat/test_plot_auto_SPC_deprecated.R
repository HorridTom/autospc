test_that("plot_auto_SPC() throws a deprecation error", {
  
  expect_error(
    plot_auto_SPC(),
    "deprecated in autospc"
  )
  
})
