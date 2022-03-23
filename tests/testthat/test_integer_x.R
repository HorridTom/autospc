testthat::test_that("plot_auto_SPC does not throw an error with integer x values", {
  # Example data
  df1 <- structure(list(x = 1:50,
                        y = c(49, 70, 44, 43, 75, 60, 47, 65, 
                              63, 62, 55, 57, 51, 49, 55, 76, 58, 51, 65, 52, 48, 60, 65, 71, 
                              70, 68, 43, 76, 98, 108, 79, 92, 84, 76, 69, 88, 83, 101, 81, 
                              72, 89, 90, 81, 82, 68, 82, 84, 93, 79, 87)),
                   row.names = c(NA, -50L),
                   class = c("tbl_df", "tbl", "data.frame")) %>%
    dplyr::mutate(y = as.integer(y))
  # Expect no error when calling plot_auto_SPC with this data
  expect_error(
    plot_auto_SPC(df1, title = "my title", subtitle = "my subtitle"),
    NA)
})
