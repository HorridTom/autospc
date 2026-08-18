# The class methods are tested in the class files. These check the default
# reaches the chart, which the method tests cannot: a default that is correct
# and never consulted looks the same to them.
#
# The assertions compare runs rather than reading annotation_level directly, so
# they do not depend on how add_annotation_data() turns a scale factor into a
# position. They are always within one chart type, because two chart types have
# different limits and so different label positions at the same scale factor.
#
# The second assertion in each is also the "a value the caller passes wins over
# the method" case: passing the other class's factor changes the result.

annotation_data <- data.frame(
  x = 1:30,
  y = c(10, 12, 11, 13, 9, 14, 10, 11, 12, 13,
        10, 12, 11, 13, 9, 14, 10, 11, 12, 13,
        10, 12, 11, 13, 9, 14, 10, 11, 12, 13),
  n = rep(100L, 30)
)

run_annotation <- function(chart_type, ...) {

  suppressWarnings(
    autospc(annotation_data,
            chart_type = chart_type,
            period_min = 21L,
            plot_chart = FALSE,
            ...)$annotation_level
  )

}


test_that("a P chart defaults to 1.04", {

  expect_equal(run_annotation("P"),
               run_annotation("P", upper_annotation_sf = 1.04))

  expect_false(isTRUE(all.equal(run_annotation("P"),
                                run_annotation("P",
                                               upper_annotation_sf = 1.1))))

})


test_that("a C chart defaults to 1.1", {

  expect_equal(run_annotation("C"),
               run_annotation("C", upper_annotation_sf = 1.1))

  expect_false(isTRUE(all.equal(run_annotation("C"),
                                run_annotation("C",
                                               upper_annotation_sf = 1.04))))

})
