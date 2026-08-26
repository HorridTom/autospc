annotation_dist_data <- data.frame(x = 1:30,
                                   y = c(49, 50, 50, 50, 48, 49, 50, 49, 50, 47,
                                         51, 48, 50, 52, 49, 50, 51, 48, 50, 49,
                                         50, 51, 49, 50, 48, 52, 49, 50, 51, 50))

run_annotation_dist <- function(...) {

  autospc(annotation_dist_data,
          chart_type = "C",
          period_min = 21L,
          plot_chart = FALSE,
          ...)

}


test_that("override_annotation_dist throws a deprecation error", {

  expect_error(run_annotation_dist(override_annotation_dist = 10),
               "deprecated")

})


test_that("override_annotation_dist_P throws a deprecation error", {

  expect_error(run_annotation_dist(override_annotation_dist_P = 10),
               "deprecated")

})


test_that("the error names the argument that replaces it", {

  expect_error(run_annotation_dist(override_annotation_dist = 10),
               "upper_annotation_sf")

})


test_that("not supplying them is not an error", {

  # deprecated() is a sentinel, not a value, so is_present() has to distinguish
  # "argument absent" from "argument supplied" - the point of using it over
  # is.null() is that it works for indirect calls too
  expect_no_error(run_annotation_dist())

})


test_that("the replacement arguments still work", {

  result <- run_annotation_dist(upper_annotation_sf = 1.1)

  expect_true("cl" %in% colnames(result))

})
