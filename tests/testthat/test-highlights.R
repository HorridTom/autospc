# Where the highlight column marks points, and which mark wins where a point
# qualifies for more than one.
#
# The assertions are on positions rather than on the whole column, so that
# renaming a label does not fail a test about where rule 2 breaks are found.

highlights_data <- readRDS("testdata/test_highlights_data.rds")
highlights_data2 <- readRDS("testdata/test_highlights_data2.rds")


test_that("a rule 2 highlight does not run across a period boundary", {
  result <- autospc(highlights_data,
    chart_type = "C'",
    plot_chart = FALSE
  )

  expect_identical(
    which(result$highlight == "Rule 2"),
    43:50
  )

  # the whole run sits inside one calculation period, which is the point of
  # the test - a highlight that ran across the boundary would span two
  expect_length(
    unique(result$period_start[43:50]),
    1L
  )

  # an excluded point carries its own mark rather than the rule it broke. The
  # wording of that mark is not the subject here, so it is not asserted.
  # excluded is NA outside a calculation period, so which() rather than [
  excluded_marks <- unique(result$highlight[which(result$excluded)])

  expect_length(excluded_marks, 1L)

  expect_false(excluded_marks %in% c("None", "Rule 1", "Rule 2"))

  expect_identical(
    which(result$highlight == "Rule 1"),
    setdiff(which(result$rule1), which(result$excluded))
  )
})


test_that("a rule 2 highlight does not appear at the end of a period", {
  # this series carries two more periods than the first, so a highlight
  # wrongly placed at a period end has more chances to appear
  result <- autospc(highlights_data2,
    chart_type = "C'",
    plot_chart = FALSE
  )

  expect_identical(
    which(result$highlight == "Rule 2"),
    43:50
  )

  expect_length(
    unique(result$period_start[43:50]),
    1L
  )

  # excluded is NA outside a calculation period, so which() rather than [
  excluded_marks <- unique(result$highlight[which(result$excluded)])

  expect_length(excluded_marks, 1L)

  expect_false(excluded_marks %in% c("None", "Rule 1", "Rule 2"))

  expect_identical(
    which(result$highlight == "Rule 1"),
    setdiff(which(result$rule1), which(result$excluded))
  )
})
