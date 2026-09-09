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

  # highlight names the rule a point broke and nothing else, so every point
  # that broke rule 1 is marked as such, excluded points included
  expect_identical(
    which(result$highlight == "Rule 1"),
    which(result$rule1)
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

  expect_identical(
    which(result$highlight == "Rule 1"),
    which(result$rule1)
  )
})


test_that("the exclusion mark is added when a plot is drawn", {
  # highlight_exclusions is a visualisation parameter, so the mark it asks for
  # is put on the table the plot is drawn from rather than on the analysis. The
  # wording of the mark is not the subject here, so it is not asserted
  drawn <- autospc(highlights_data, chart_type = "C'")$data

  excluded_marks <- unique(drawn$highlight[which(drawn$excluded)])

  expect_length(excluded_marks, 1L)

  expect_false(excluded_marks %in% c("None", "Rule 1", "Rule 2"))

  not_drawn <- autospc(highlights_data,
    chart_type = "C'",
    highlight_exclusions = FALSE
  )$data

  expect_true(all(
    not_drawn$highlight[which(not_drawn$excluded)] %in%
      c("None", "Rule 1", "Rule 2")
  ))
})
