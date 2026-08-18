# The algorithm's decision history. example_series_2a forms two candidates: the
# first is rejected under no_regrets, the second accepted.

fitted_2a <- function() {
  chart <- autospc_chart(chart_type = "C'",
                         data = example_series_2a,
                         x = "x",
                         y = "y")
  chart <- prepare_data(chart)
  run_limit_algorithm(chart)
}


test_that("every candidate the algorithm forms is recorded", {

  candidates <- fitted_2a()$history$candidates

  expect_length(candidates, 2L)
  expect_identical(vapply(candidates, function(k) k$counter, integer(1)),
                   c(22L, 23L))

})


test_that("the record says which way each decision went, and on what", {

  candidates <- fitted_2a()$history$candidates

  expect_identical(vapply(candidates, function(k) k$accepted, logical(1)),
                   c(FALSE, TRUE))

  # the first is rejected because the final run may revert, not because of an
  # opposing break
  expect_identical(candidates[[1]]$final_run_prevents, TRUE)
  expect_identical(candidates[[1]]$opposite_break, FALSE)

})


test_that("a rejected candidate keeps the limits it would have had", {

  rejected <- fitted_2a()$history$candidates[[1]]

  expect_equal(nrow(rejected$table), nrow(example_series_2a))
  expect_true(all(c("cl", "ucl", "lcl") %in% colnames(rejected$table)))

  # the candidate's own centre line, not the one it was proposing to replace
  expect_false(isTRUE(all.equal(rejected$table$cl[rejected$counter],
                                rejected$prevailing$cl)))

})


test_that("prevailing limits are those in force at the row before", {

  candidates <- fitted_2a()$history$candidates

  for (candidate in candidates) {
    expect_identical(candidate$prevailing$last_row, candidate$counter - 1L)
  }

  expect_identical(candidates[[1]]$period_rows[1], candidates[[1]]$counter)

})
