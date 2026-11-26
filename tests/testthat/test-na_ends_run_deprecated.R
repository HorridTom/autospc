# 25 points on one side of the centre line, then 12 on the other, with a gap at
# point 30, so a run continues across the gap only with na_ends_pattern = FALSE
gap_in_run <- data.frame(
  x = 1:37,
  y = as.numeric(c(rep(10, 25), rep(20, 12)))
)
gap_in_run$y[30] <- NA

run_gap <- function(...) {
  autospc(gap_in_run,
    chart_type = "C\'",
    period_min = 21L,
    plot_chart = FALSE,
    ...
  )
}

facet_gap <- function(...) {
  facet_stages(gap_in_run,
    split_at = 30L,
    chart_type = "C\'",
    period_min = 21L,
    plot_chart = FALSE,
    ...
  )
}


test_that("autospc(na_ends_run) warns that it is deprecated", {
  lifecycle::expect_deprecated(run_gap(na_ends_run = FALSE))
})


test_that("the autospc() warning names the argument that replaces it", {
  rlang::local_options(lifecycle_verbosity = "warning")

  expect_warning(run_gap(na_ends_run = FALSE), "na_ends_pattern")
})


test_that("autospc() takes the value of na_ends_run as na_ends_pattern", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  expect_identical(
    run_gap(na_ends_run = FALSE),
    run_gap(na_ends_pattern = FALSE)
  )
  expect_false(identical(
    run_gap(na_ends_run = FALSE),
    run_gap(na_ends_pattern = TRUE)
  ))
})


test_that("autospc() uses na_ends_pattern where both are given", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  expect_identical(
    run_gap(na_ends_run = FALSE, na_ends_pattern = TRUE),
    run_gap(na_ends_pattern = TRUE)
  )
})


test_that("autospc() does not warn about na_ends_run where it is not given", {
  rlang::local_options(lifecycle_verbosity = "warning")

  expect_no_warning(run_gap(), class = "lifecycle_warning_deprecated")
})


test_that("facet_stages(na_ends_run) warns that it is deprecated", {
  lifecycle::expect_deprecated(facet_gap(na_ends_run = FALSE))
})


test_that("facet_stages() takes the value of na_ends_run as na_ends_pattern", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  expect_identical(
    facet_gap(na_ends_run = FALSE),
    facet_gap(na_ends_pattern = FALSE)
  )
  expect_false(identical(
    facet_gap(na_ends_run = FALSE),
    facet_gap(na_ends_pattern = TRUE)
  ))
})


test_that("facet_stages() uses na_ends_pattern where both are given", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  expect_identical(
    facet_gap(na_ends_run = FALSE, na_ends_pattern = TRUE),
    facet_gap(na_ends_pattern = TRUE)
  )
})
