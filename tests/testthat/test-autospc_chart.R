test_data <- data.frame(x = 1:3, y = 1:3)


test_that("new_autospc_chart rejects an object that is not a list", {

  expect_error(new_autospc_chart("not a list"), "is\\.list")

})


test_that("validate_autospc_chart rejects an object that is not an autospc_chart", {

  bare_list <- autospc_chart_list(data = test_data, x = "x", y = "y")

  expect_error(validate_autospc_chart(bare_list),
               "Not an autospc_chart object")

})


test_that("y_axis_title has no method for a bare autospc_chart", {

  bare_chart <- autospc_chart(data = test_data, x = "x", y = "y")

  expect_error(y_axis_title(bare_chart), "no applicable method")

})
