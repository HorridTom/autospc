# The algorithm walks the data in row order, so a series given out of x order
# has to be sorted before any of it runs, whatever the chart type.
#
# Values are hard-coded rather than generated, so the test does not depend on
# the RNG behaving identically across R versions. Originally produced with
# set.seed(3); rpois(40, 50) and a sample() permutation, under R 4.5.1.

sort_val <- c(43, 46, 41, 52, 50, 51, 56, 53, 45, 42,
              55, 59, 54, 47, 45, 44, 45, 51, 45, 46,
              44, 57, 49, 41, 58, 55, 47, 55, 62, 50,
              43, 55, 47, 56, 49, 59, 45, 57, 46, 53)

sort_perm <- c(33, 10,  2, 11, 26,  4, 16,  6, 23,  7,
               34,  3, 24, 12, 35, 30,  5, 39, 31, 18,
                9, 37, 28, 15, 20, 32, 14, 17, 22,  1,
               13,  8, 25, 21, 19, 38, 27, 29, 40, 36)

sorted_data <- data.frame(mth = 1:40,
                          val = sort_val)

shuffled_data <- sorted_data[sort_perm, ]

run_sorted <- function(d, chart_type) {

  suppressWarnings(
    autospc(d,
            chart_type = chart_type,
            x = mth,
            y = val,
            plot_chart = FALSE,
            show_mr = FALSE)
  )

}


test_that("a C chart gives the same result whatever order the rows arrive in", {

  # C aggregates, and group_by() then summarise() sorts, so this case was
  # already right by accident - it is here to pin that it stays right
  expect_equal(run_sorted(shuffled_data, "C"),
               run_sorted(sorted_data, "C"))

})


test_that("an XMR chart gives the same result whatever order the rows arrive in", {

  # X and MR have no aggregate_data() method, so ordering the series is the
  # only thing that puts these rows in x order
  expect_equal(run_sorted(shuffled_data, "XMR"),
               run_sorted(sorted_data, "XMR"))

})


test_that("an MR chart gives the same result whatever order the rows arrive in", {

  expect_equal(run_sorted(shuffled_data, "MR"),
               run_sorted(sorted_data, "MR"))

})


test_that("the series comes back in x order", {

  for(chart_type in c("C", "C'", "XMR", "MR")) {

    result <- run_sorted(shuffled_data, chart_type)

    expect_identical(result$x, sort(result$x), info = chart_type)

  }

})


test_that("ordering is stable, so rows sharing an x keep the order they arrived in", {

  tied <- data.frame(x = c(2L, 1L, 2L, 1L),
                     y = c(20, 10, 21, 11),
                     tag = c("first_2", "first_1", "second_2", "second_1"))

  chart <- autospc_chart_c(data = tied, x = "x", y = "y")

  expect_identical(order_series(chart)$data$tag,
                   c("first_1", "second_1", "first_2", "second_2"))

})
