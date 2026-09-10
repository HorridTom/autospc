test_data <- structure(
  list(
    x = 1:21,
    y = c(
      55L, 42L, 46L, 50L, 49L, 53L, 44L, 46L, 46L,
      54L, 48L, 56L, 47L, 43L, 57L, 42L, 55L, 51L,
      52L, 55L, 48L
    ),
    n = c(
      197L, 196L, 198L, 209L, 201L, 199L, 202L,
      195L, 205L, 191L, 197L, 207L, 196L, 195L,
      205L, 197L, 206L, 197L, 183L, 195L, 192L
    )
  ),
  class = "data.frame",
  row.names = c(NA, -21L)
) %>%
  tibble::as_tibble()

test_that("Limit extension works correctly for C chart", {
  results_nex <- autospc(test_data,
    chart_type = "C",
    plot_chart = FALSE
  )

  results_ext <- autospc(test_data,
    chart_type = "C",
    plot_chart = FALSE,
    extend_limits_to = 35L
  )

  # Get the correct values for the centre line and limits (since this is a C
  # chart with only one period, all three values are constant throughout, so we
  # can get the correct limit values for the extension from, e.g., the last row)
  limit_values <- results_nex %>%
    dplyr::filter(dplyr::row_number() == nrow(test_data)) %>%
    dplyr::select(cl, lcl, ucl)

  # Limit extension should result in two additional rows in the result, marking
  # the start and end of the extension period, with the correct limits and cl
  expect_equal(
    results_ext %>%
      dplyr::filter(dplyr::row_number() == nrow(test_data) + 1L) %>%
      dplyr::select(cl, lcl, ucl),
    limit_values
  )

  expect_equal(
    results_ext %>%
      dplyr::filter(dplyr::row_number() == nrow(test_data) + 2L) %>%
      dplyr::select(cl, lcl, ucl),
    limit_values
  )

  # the extension is a display period: it shows the limits at points they were
  # not calculated from, and there are no points there at all
  expect_identical(
    stringr::str_extract(
      results_ext$plot_period[nrow(test_data) + 1:2],
      "^[a-z]*"
    ),
    c("display", "display")
  )
})


test_that("Limit extension works correctly for P chart", {
  results_nex <- autospc(test_data,
    chart_type = "P",
    plot_chart = FALSE
  )

  results_ext <- autospc(test_data,
    chart_type = "P",
    plot_chart = FALSE,
    extend_limits_to = 35L
  )

  # Get the correct values for the centre line and limits
  aggregates <- test_data %>%
    dplyr::summarise(
      y = sum(y),
      n = sum(n),
      m = dplyr::n()
    ) %>%
    dplyr::mutate(
      pbar = y / n,
      nbar = n / m
    ) %>%
    dplyr::select(
      pbar,
      nbar
    )

  pbar <- aggregates$pbar
  nbar <- aggregates$nbar

  lcl <- pbar - 3 * sqrt((pbar * (1 - pbar)) / nbar)
  ucl <- pbar + 3 * sqrt((pbar * (1 - pbar)) / nbar)

  limit_values <- data.frame(
    cl = pbar * 100,
    lcl = lcl * 100,
    ucl = ucl * 100
  )

  # Limit extension should result in two additional rows in the result, marking
  # the start and end of the extension period, with the correct limits and cl
  expect_equal(
    results_ext %>%
      dplyr::filter(dplyr::row_number() == nrow(test_data) + 1L) %>%
      dplyr::select(cl, lcl, ucl),
    limit_values
  )

  expect_equal(
    results_ext %>%
      dplyr::filter(dplyr::row_number() == nrow(test_data) + 2L) %>%
      dplyr::select(cl, lcl, ucl),
    limit_values
  )

  # the extension is a display period: it shows the limits at points they were
  # not calculated from, and there are no points there at all
  expect_identical(
    stringr::str_extract(
      results_ext$plot_period[nrow(test_data) + 1:2],
      "^[a-z]*"
    ),
    c("display", "display")
  )
})


test_extend_limits_pp_answer <- readRDS(
  "testdata/test_extend_limits_pp_answer.rds"
)

test_that("Limit extension works correctly for P-prime chart (regression)", {
  results_ext <- autospc(test_data,
    chart_type = "P'",
    plot_chart = FALSE,
    extend_limits_to = 35L
  ) %>%
    dplyr::select(
      x,
      series,
      y,
      n,
      ucl,
      lcl,
      cl
    )

  # the stored answer was saved when autospc() returned a tibble, and when the
  # percentages were `y` and the counts `y_numerator`. The values are
  # unchanged, only the names and the class
  test_extend_limits_pp_answer <- test_extend_limits_pp_answer %>%
    dplyr::rename(series = y, y = y_numerator) %>%
    dplyr::select(
      x,
      series,
      y,
      n,
      ucl,
      lcl,
      cl
    ) %>%
    as.data.frame()

  # it was also saved when the two rows of the extension copied the denominator
  # and numerator of the last row of the data. They now hold no observation,
  # because there is no subgroup at those points on the axis
  extension <- nrow(test_data) + 1:2
  test_extend_limits_pp_answer$n[extension] <- NA_integer_
  test_extend_limits_pp_answer$y[extension] <- NA_integer_

  expect_equal(
    results_ext,
    test_extend_limits_pp_answer
  )
})


test_that("the extension rows carry limits and no observation", {
  result <- autospc(test_data,
    chart_type = "P",
    plot_chart = FALSE,
    extend_limits_to = 35L
  )

  extension <- result[nrow(test_data) + 1:2, ]

  expect_identical(extension$limit_extension, c(TRUE, TRUE))

  expect_false(any(result$limit_extension[seq_len(nrow(test_data))]))

  # the columns that describe a subgroup, of which there is none here
  expect_true(all(is.na(extension$series)))
  expect_true(all(is.na(extension$y)))
  expect_true(all(is.na(extension$n)))
  expect_true(all(is.na(extension$log)))

  # a row that is neither above the centre line nor below it commences no run
  # and breaks no rule
  expect_equal(extension$above_or_below_cl, c(0, 0))
  expect_false(any(extension$run_start))
  expect_false(any(extension$rule1))
  expect_false(any(extension$rule2))
  expect_false(any(extension$break_point))
  expect_identical(extension$highlight, c("None", "None"))
})


test_that("the extension continues the period the series ends in", {
  # the series ends with rows that hold no observation, so the last row of the
  # table belongs to no period at all
  trailing_gap <- dplyr::bind_rows(test_data, test_data) %>%
    dplyr::mutate(x = dplyr::row_number())
  trailing_gap$y[41:42] <- NA

  result <- autospc(trailing_gap,
    chart_type = "P",
    plot_chart = FALSE,
    extend_limits_to = 60L
  )

  extension <- which(result$limit_extension)

  in_a_period <- which(!is.na(result$period_start[-extension]))
  final <- in_a_period[length(in_a_period)]

  expect_identical(result$period_type[extension], c("display", "display"))

  expect_identical(
    result$period_start[extension],
    rep(result$period_start[final], 2L)
  )

  expect_identical(
    result$plot_period[extension],
    rep(paste0("display", result$period_start[final]), 2L)
  )

  expect_false(any(is.na(result$limit_width[extension])))

  expect_false(any(result$limit_change[extension]))

  expect_equal(result$cl_change[extension], c(0, 0))
})


test_that("the extension takes its number from a final calculation period", {
  # a series that ends at the end of its second calculation period, so there is
  # no display period after it. The extension is the display part of that
  # period rather than a period numbered after it
  set.seed(11)
  ends_calculating <- data.frame(
    x = 1:42,
    y = as.integer(c(stats::rpois(21, 20), stats::rpois(21, 60)))
  )

  result <- autospc(ends_calculating,
    chart_type = "C",
    plot_chart = FALSE,
    period_min = 21L,
    extend_limits_to = 60
  )

  extension <- which(result$limit_extension)

  final <- extension[1L] - 1L

  expect_identical(result$period_type[final], "calculation")

  expect_identical(result$period_type[extension], c("display", "display"))

  expect_identical(
    result$period_start[extension],
    rep(result$period_start[final], 2L)
  )
})


test_that("the extension keeps the type of the columns it adds rows to", {
  whole_numbers <- data.frame(
    x = seq_len(nrow(test_data)),
    y = as.integer(test_data$y),
    n = as.integer(test_data$n)
  )

  result <- autospc(whole_numbers,
    chart_type = "C",
    plot_chart = FALSE,
    extend_limits_to = 35
  )

  expect_type(result$x, "integer")
  expect_type(result$y, "integer")

  dated <- whole_numbers
  dated$x <- seq(as.Date("2020-01-01"),
    by = "month",
    length.out = nrow(whole_numbers)
  )

  dated_result <- autospc(dated,
    chart_type = "C",
    plot_chart = FALSE,
    extend_limits_to = as.Date("2024-01-01")
  )

  expect_s3_class(dated_result$x, "Date")
})


test_that("a column of whole numbers gives way to a value it cannot hold", {
  # extend_limits_to is a point on the horizontal axis, and a point between two
  # whole numbers is one, so it is taken rather than rounded
  whole_numbers <- data.frame(
    x = seq_len(nrow(test_data)),
    y = as.integer(test_data$y)
  )

  result <- autospc(whole_numbers,
    chart_type = "C",
    plot_chart = FALSE,
    extend_limits_to = 35.5
  )

  expect_type(result$x, "double")

  expect_identical(result$x[nrow(result)], 35.5)
})


# where the extension begins


test_that("the extension begins one subgroup on from the end of the data", {
  # the step used to be a fixed 1, which is a hundred subgroups here: readings
  # ten milliseconds apart, with x expressed in seconds
  fine <- data.frame(
    x = seq(0.01, 0.40, by = 0.01),
    y = as.numeric(rep(c(30, 28, 32, 29, 31), 8))
  )

  result <- autospc(fine,
    chart_type = "C",
    plot_chart = FALSE,
    period_min = 21L,
    extend_limits_to = 0.60
  )

  extension <- which(result$limit_extension)

  expect_equal(result$x[extension], c(0.41, 0.60))

  expect_false(is.unsorted(result$x))
})


test_that("the step is capped so that the extension holds both rows", {
  # ten days is less than a third of one subgroup here, so the median gap on
  # its own would put the first row beyond the second
  monthly <- data.frame(
    x = seq(as.Date("2020-01-01"), by = "month", length.out = 40),
    y = as.numeric(rep(c(30, 28, 32, 29, 31), 8))
  )

  result <- autospc(monthly,
    chart_type = "C",
    plot_chart = FALSE,
    period_min = 21L,
    extend_limits_to = as.Date("2023-04-11")
  )

  extension <- which(result$limit_extension)

  expect_identical(
    result$x[extension],
    as.Date(c("2023-04-06", "2023-04-11"))
  )

  expect_false(is.unsorted(result$x))
})


test_that("the step is a whole unit on an axis that holds whole units", {
  # a Date is a whole number of days, so half a day is a step to a value the
  # column cannot tell from the one before it
  expect_identical(
    extension_step(
      x_values = seq(as.Date("2020-01-01"), by = "month", length.out = 3),
      extend_limits_to = as.Date("2020-03-02")
    ),
    1
  )

  # 21 gaps of one and 21 of two give a median of one and a half
  irregular <- as.integer(c(seq(1, 22), seq(24, 64, by = 2)))

  expect_identical(
    extension_step(x_values = irregular, extend_limits_to = 100L),
    2
  )

  # and where the axis holds any number, the step is not rounded
  expect_equal(
    extension_step(
      x_values = seq(0.01, 0.40, by = 0.01),
      extend_limits_to = 1.60
    ),
    0.01
  )
})


test_that("an extension of one whole unit adds one row", {
  # the step is a whole unit on this axis and the extension is one unit long,
  # so the first row of the extension is also the last row of it
  whole_numbers <- data.frame(
    x = seq_len(nrow(test_data)),
    y = as.integer(test_data$y)
  )

  result <- autospc(whole_numbers,
    chart_type = "C",
    plot_chart = FALSE,
    extend_limits_to = nrow(test_data) + 1L
  )

  extension <- which(result$limit_extension)

  expect_length(extension, 1L)

  expect_identical(result$x[extension], nrow(test_data) + 1L)

  expect_equal(anyDuplicated(result$x), 0L)
})


test_that("the step does not reach past the end of a short extension", {
  # rounding the step up on a whole-unit axis would otherwise take it past
  # extend_limits_to, which is half a unit from the end of the data
  expect_identical(
    extension_step(x_values = 1:21, extend_limits_to = 21.5),
    0.5
  )

  whole_numbers <- data.frame(
    x = seq_len(nrow(test_data)),
    y = as.integer(test_data$y)
  )

  result <- autospc(whole_numbers,
    chart_type = "C",
    plot_chart = FALSE,
    extend_limits_to = nrow(test_data) + 0.5
  )

  extension <- which(result$limit_extension)

  expect_length(extension, 1L)

  expect_identical(result$x[extension], nrow(test_data) + 0.5)

  expect_false(is.unsorted(result$x))
})
