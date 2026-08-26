# Simulate data for aggregation testing
# 1. binary outcome for p(')-charts
# set.seed(202602)
# 
# x <- rep(c(1L:42L),
#          times = rep(c(25L,50L),
#                      c(21L,21L)))
# 
# y <- runif(n = length(x),
#            min = 0,
#            max = 1) <= 0.5
# 
# test_pchart_aggregation_data <- tibble::tibble(x = x,
#                                                y = y)
# 
# saveRDS(test_pchart_aggregation_data,
#         file = "tests/testthat/testdata/test_pchart_aggregation_data.rds")
#
# 2. count data for c(')-charts
#
# set.seed(202603)
# 
# y <- rpois(42L,
#            lambda = 50L)
# 
# x <- rep(c(1L:42L),
#          times = y)
#
# test_cchart_aggregation_data <- tibble::tibble(x = x,
#                                                y = 1L)
#
# saveRDS(test_cchart_aggregation_data,
#         file = "tests/testthat/testdata/test_cchart_aggregation_data.rds")
# 
# test_cchart_aggregated_data <- tibble::tibble(x = c(1L:42L),
#                                               y = y)
# saveRDS(test_cchart_aggregated_data,
#         file = "tests/testthat/testdata/test_cchart_aggregated_data.rds")

test_pchart_aggregation_data <- readRDS(
  file.path("testdata",
            "test_pchart_aggregation_data.rds"))

# Correct aggregation produced using pivot table in Excel
test_pchart_aggregated_data <- readRDS(
  file.path("testdata",
            "test_pchart_aggregated_data.rds"))

test_cchart_aggregation_data <- readRDS(
  file.path("testdata",
            "test_cchart_aggregation_data.rds"))

test_cchart_aggregated_data <- readRDS(
  file.path("testdata",
            "test_cchart_aggregated_data.rds"))

# The expected values come from an Excel pivot table rather than from this
# package, so these two tests check the arithmetic against an independent
# oracle at realistic scale. The class test files check the object plumbing -
# that dispatch happens, that the class survives, that data_original is left
# alone - which these cannot, and neither set replaces the other.

test_that("binary aggregation works", {

  chart <- autospc_chart_p(data = test_pchart_aggregation_data,
                           x = "x",
                           y = "y",
                           n = "n")

  aggregated_data <- aggregate_data(chart)$data

  expect_equal(aggregated_data$x,
               test_pchart_aggregated_data$x)
  expect_equal(aggregated_data$y,
               test_pchart_aggregated_data$y)
  expect_equal(aggregated_data$n,
               test_pchart_aggregated_data$n)

})


test_that("count aggregation works", {

  chart <- autospc_chart_c(data = test_cchart_aggregation_data,
                           x = "x",
                           y = "y")

  aggregated_data <- aggregate_data(chart)$data

  expect_equal(aggregated_data$x,
               test_cchart_aggregated_data$x)
  expect_equal(aggregated_data$y,
               test_cchart_aggregated_data$y)

})

