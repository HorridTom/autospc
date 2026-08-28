# By the time an MR chart reaches these methods its y column already holds the
# moving ranges, so the shared count fixtures serve as moving-range series here.

test_chart_mr <- function(...) {
  autospc_chart_mr(data = test_data, x = "x", y = "y", ...)
}

dup_chart_mr <- function(...) {
  autospc_chart_mr(data = dup_data, x = "x", y = "y", ...)
}


test_that("autospc_chart_mr returns an object of the expected class", {
  expect_identical(
    class(test_chart_mr()),
    c("autospc_chart_mr", "autospc_chart")
  )
})


test_that("autospc_chart_mr returns the common elements and no others", {
  chart <- test_chart_mr()

  expect_true(all(autospc_chart_elements() %in% names(chart)))
  expect_length(chart, length(autospc_chart_elements()))
})


test_that("data_original is populated correctly", {
  expect_identical(test_chart_mr()$data_original, test_data)
})


test_that("an unrecognised argument name is rejected", {
  expect_error(test_chart_mr(period_mn = 30L), "unused argument")
})


test_that("a missing element is caught on the constructor path", {
  expect_error(
    validate_autospc_chart_mr(new_autospc_chart_mr(list(data = test_data))),
    "element\\(s\\) not present"
  )
})


test_that("validate_autospc_chart_mr rejects a bare autospc_chart object", {
  expect_error(
    validate_autospc_chart_mr(new_autospc_chart(autospc_chart_list(data = test_data, x = "x", y = "y"))),
    "Not an autospc_chart_mr object"
  )
})


test_that("validate_autospc_chart_mr rejects an autospc_chart_x object", {
  expect_error(
    validate_autospc_chart_mr(autospc_chart_x(
      data = test_data,
      x = "x",
      y = "y"
    )),
    "Not an autospc_chart_mr object"
  )
})


test_that("validate_autospc_chart_mr returns a valid object unchanged", {
  chart <- test_chart_mr()

  expect_identical(validate_autospc_chart_mr(chart), chart)
})


test_that("y_axis_title returns the MR chart axis title", {
  expect_identical(y_axis_title(test_chart_mr()), "MR")
})


test_that("chart_type_label returns the MR chart label", {
  expect_identical(chart_type_label(test_chart_mr()), "MR")
})


test_that("MR charts have no aggregate_data method of their own", {
  # one moving range per observation, so the superclass default is correct
  expect_null(getS3method("aggregate_data",
    "autospc_chart_mr",
    optional = TRUE
  ))
})


test_that("aggregate_data leaves an MR chart untouched", {
  chart <- dup_chart_mr()

  expect_identical(aggregate_data(chart), chart)
  expect_identical(aggregate_data(chart)$data, dup_data_analysed)
})


test_that("calculate_limits matches get_mr_limits", {
  expect_identical(
    calculate_limits(test_chart_mr(),
      period = count_period_data,
      exclusion_points = NULL
    ),
    get_mr_limits(
      mr = count_period_data$y,
      mr_screen_max_loops = 0L,
      exclusion_points = NULL
    )
  )
})


test_that("calculate_limits passes exclusion_points through", {
  expect_identical(
    calculate_limits(test_chart_mr(),
      period = count_period_data,
      exclusion_points = 6L
    ),
    get_mr_limits(
      mr = count_period_data$y,
      mr_screen_max_loops = 0L,
      exclusion_points = 6L
    )
  )

  # excluding the high point must actually lower the centre line, otherwise the
  # comparison above would pass even if the argument were ignored
  expect_lt(
    calculate_limits(test_chart_mr(),
      period = count_period_data,
      exclusion_points = 6L
    )$cl[1],
    calculate_limits(test_chart_mr(),
      period = count_period_data,
      exclusion_points = NULL
    )$cl[1]
  )
})


test_that("calculate_limits ignores mr_screen_max_loops on the chart", {
  # the MR chart never screens: screening estimates the average moving range for
  # the X chart's limits only (Provost and Murray). So unlike every other class,
  # the field must NOT be read - a chart carrying a different value must give
  # identical limits.
  expect_identical(
    calculate_limits(test_chart_mr(mr_screen_max_loops = 5L),
      period = screening_data, exclusion_points = NULL
    ),
    calculate_limits(test_chart_mr(mr_screen_max_loops = 0L),
      period = screening_data, exclusion_points = NULL
    )
  )

  # screening_data is chosen because screening would change the answer here if
  # it were applied, so the test above is not vacuous
  expect_false(
    identical(
      get_mr_limits(
        mr = screening_data$y,
        mr_screen_max_loops = 0L,
        exclusion_points = NULL
      ),
      get_mr_limits(
        mr = screening_data$y,
        mr_screen_max_loops = 5L,
        exclusion_points = NULL
      )
    )
  )
})


test_that("n_effective_points adds one to the moving ranges", {
  # moving_ranges() prepends NA, so an MR series has one fewer non-missing value
  # than the series it came from, and the data-sufficiency checks are about that
  # underlying series
  counted <- data.frame(y = c(NA, 3, 6, 7))

  expect_identical(n_effective_points(test_chart_mr(), data = counted), 4L)
})


test_that("prepare_data replaces y with the moving ranges", {
  counts <- data.frame(x = 1:5, y = c(5, 8, 2, 9, 4))

  chart <- autospc_chart_mr(data = counts, x = "x", y = "y")

  prepared <- prepare_data(chart)

  # moving_ranges() prepends NA, so the series stays aligned with x
  expect_identical(prepared$data$y, c(NA, 3, 6, 7, 5))
  expect_identical(prepared$data$x, counts$x)

  # and what the user supplied is untouched
  expect_identical(prepared$data_original, counts)
})


test_that("the first MR centre line label goes on row two", {
  expect_identical(first_label_row(test_chart_mr()), 2L)
})


test_that("MR chart labels are rounded to the scale of the axis", {
  expect_identical(label_accuracy(test_chart_mr(), ylimhigh = 1000), 0.1)
})


test_that("the MR chart y axis starts at zero", {
  expect_identical(
    y_axis_range(test_chart_mr(), data = limits_data),
    list(low = 0, high = 18 * 1.1)
  )
})


test_that("moving range labels always stay above the centre line", {
  expect_true(labels_stay_above(test_chart_mr()))
})


test_that("the moving range label separates thousands", {
  expect_identical(
    centre_line_label(test_chart_mr(),
      cl = 1234,
      ylimhigh = 5000
    ),
    "1,234"
  )
})
