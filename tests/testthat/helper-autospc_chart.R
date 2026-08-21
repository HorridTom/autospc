# Fixtures shared by the autospc_chart class test files. Sourced by testthat
# before any test runs.
#
# Only genuinely shared data belongs here. Fixtures that exist because a class
# needs something the shared ones cannot express stay in their own file:
# screening_data in test-autospc_chart_cp.R, and the P fixtures in
# test-autospc_chart_p.R.

# three rows, one per subgroup
test_data <- data.frame(x = 1:3, y = 1:3)

# two rows per subgroup, plus a column that no chart retains, because
# construction selects only the columns the analysis uses
dup_data <- data.frame(x = rep(1:3, each = 2),
                       y = c(1, 2, 10, 20, 100, 200),
                       site = "a")

dup_data_analysed <- dup_data[c("x", "y")]

# a calculation period of counts with one obvious high point, so that excluding
# it demonstrably moves the limits. Named for the count charts because the
# proportion charts need a period carrying y_numerator and n as well.
count_period_data <- data.frame(x = 1:10,
                                y = c(12, 15, 11, 14, 13, 30, 12, 14, 13, 11))

# a calculation period with a peak extreme enough that moving-range screening
# actually changes the limits. The count period above will not do: its peak of
# 30 leaves the two large moving ranges just under the MR upper limit, so
# screening is inert and a test using it cannot tell whether
# mr_screen_max_loops was read at all. Verified to bite for both
# get_cp_limits() and get_i_limits().
screening_data <- data.frame(x = 1:10,
                             y = c(12, 15, 11, 14, 13, 60, 12, 14, 13, 11))


# a plotted series with limits, as the y axis methods see it
limits_data <- data.frame(x = 1:5,
                          y = c(10, 12, 11, 13, 14),
                          cl = 12,
                          lcl = 6,
                          ucl = 18)
