# load in test data
# zeros in n column only
test_data1 <- data.frame(
  x = 1:50,
  y = c(
    53, 53, 53, 45, 49, 54, 48, 48,
    55, 52, 52, 50, 45, 52, 49, 51,
    52, 50, 54, 47, 27, 25, 23,
    28, 22, 23, 25, 27,
    26, 28, 20, 22,
    24, 21, 29, 28, 28, 21, 20, 22,
    22, 25, 27, 29, 21, 27, 20, 23,
    22, 22
  ),
  n = c(
    197, 196, 203, 201, 201, 200, 195,
    197, 200, 202, 203, 199, 197, 201,
    205, 201, 199, 201, 200, 192, 201,
    202, 203,
    0, 0, 0, 0, 0,
    200, 197, 201, 205, 198, 194, 200,
    201, 201, 195, 200, 191, 194, 206,
    205, 199, 202, 195, 199, 197, 201,
    198
  )
)

# zeros in both y and n
test_data2 <- data.frame(
  x = 1:50,
  y = c(
    53, 53, 53, 45, 49, 54, 48, 48,
    55, 52, 52, 50, 45, 52, 49, 51,
    52, 50, 54, 47, 27, 25, 23,
    0, 0, 0, 0, 0,
    26, 28, 20, 22,
    24, 21, 29, 28, 28, 21, 20, 22,
    22, 25, 27, 29, 21, 27, 20, 23,
    22, 22
  ),
  n = c(
    197, 196, 203, 201, 201, 200, 195,
    197, 200, 202, 203, 199, 197, 201,
    205, 201, 199, 201, 200, 192, 201,
    202, 203,
    0, 0, 0, 0, 0,
    200, 197, 201, 205, 198, 194, 200,
    201, 201, 195, 200, 191, 194, 206,
    205, 199, 202, 195, 199, 197, 201,
    198
  )
)


# zeros in both y and n in calc period. The n column has four zeros, rows 10 to
# 13, so the y column does too: a subgroup with no opportunities can only have
# had no events.
test_data3 <- data.frame(
  x = 1:50,
  y = c(
    53, 53, 53, 45, 49, 54, 48, 48,
    55, 0, 0, 0, 0, 52, 49, 51,
    52, 50, 54, 47, 27, 25, 23,
    25, 27, 29, 21, 27,
    26, 28, 20, 22,
    24, 21, 29, 28, 28, 21, 20, 22,
    22, 25, 27, 29, 21, 27, 20, 23,
    22, 22
  ),
  n = c(
    197, 196, 203, 201, 201, 200, 195,
    197, 200, 0, 0, 0, 0, 201,
    205, 201, 199, 201, 200, 192, 201,
    202, 203,
    201, 195, 200, 191, 194,
    200, 197, 201, 205, 198, 194, 200,
    201, 201, 195, 200, 191, 194, 206,
    205, 199, 202, 195, 199, 197, 201,
    198
  )
)


test_that("a subgroup with no attendances and no events draws", {
  # a week with no patients at a small clinic: no denominator and no numerator,
  # so no proportion, and the limits carry across it
  result2 <- autospc(test_data2,
    chart_type = "P'", plot_chart = FALSE, period_min = 21
  ) %>%
    dplyr::select(x, series, y, n, ucl, lcl, cl)

  result3 <- autospc(test_data3,
    chart_type = "P'", plot_chart = FALSE, period_min = 21
  ) %>%
    dplyr::select(x, series, y, n, ucl, lcl, cl)

  testthat::expect_equal(all(is.na(result2$series[24:28])), TRUE)
  testthat::expect_equal(all(!is.na(result2$ucl[24:28])), TRUE)
  testthat::expect_equal(all(!is.na(result2$lcl[24:28])), TRUE)
  testthat::expect_equal(all(!is.na(result2$cl[24:28])), TRUE)

  testthat::expect_equal(all(is.na(result3$series[10:13])), TRUE)
  testthat::expect_equal(all(!is.na(result3$ucl[10:13])), TRUE)
  testthat::expect_equal(all(!is.na(result3$lcl[10:13])), TRUE)
  testthat::expect_equal(all(!is.na(result3$cl[10:13])), TRUE)

  # the numerator is an observation in its own right, so it is kept where the
  # denominator is zero. It is only the proportion it would give that is missing
  testthat::expect_false(any(is.na(result2$y[24:28])))
  testthat::expect_false(any(is.na(result3$y[10:13])))
})


test_that("events recorded against no attendances are refused", {
  # test_data1 has a numerator of 28 against a denominator of 0, which says 28
  # events happened in a subgroup that had no opportunities for one. That is
  # data entered wrongly rather than a quiet week, so the chart is refused
  # rather than drawn with those subgroups blank.
  testthat::expect_error(
    autospc(test_data1,
      chart_type = "P'", plot_chart = FALSE, period_min = 21
    ),
    "y must be a count from 0 to n"
  )
})
