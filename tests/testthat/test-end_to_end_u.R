# The U and U' charts through autospc() and facet_stages(). The analysis of each
# is tested in test-autospc_chart_u.R and test-autospc_chart_up.R; these tests
# check the whole run against qicharts2, across the forms the data can take.

# qicharts2 cross-check. qicharts2 0.8.1 follows Provost and Murray for U and
# Laney (2002) for U', with the published rounded d2 of 1.128. It screens the
# moving ranges of the z-scores once where the option qic.screenedmr is TRUE,
# and not at all by default. u_e2e_data and correct_answer_U were generated
# with qicharts2 0.8.1:
#
# set.seed(2027)
# n <- round(runif(25, 0.8, 2.4), 3)
# rate <- rgamma(25, shape = 8, rate = 8 / 6)
# rate[17] <- rate[17] * 6
# y <- rpois(25, rate * n)
# u_e2e_data <- data.frame(x = 1:25, y = y, n = n)
#
# qic_table <- function(chart, screened = FALSE) {
#   previous <- options(qic.screenedmr = screened)
#   on.exit(options(previous))
#   q <- qicharts2::qic(x, y, n,
#     data = u_e2e_data, chart = chart,
#     return.data = TRUE
#   )
#   data.frame(x = q$x, series = q$y, cl = q$cl, ucl = q$ucl, lcl = q$lcl)
# }
#
# correct_answer_U <- list(
#   u = qic_table("u"),
#   up = qic_table("up"),
#   up_screened = qic_table("up", screened = TRUE)
# )
#
# The rates vary more than a Poisson count's would, and point 17's is several
# times the others', so the screening removes its two moving ranges.

u_e2e_data <- readRDS(file.path("testdata", "test_u_e2e_data.rds"))

correct_answer_U <- readRDS(file.path(
  "testdata",
  "test_data_end_to_end",
  "correct_answer_U.rds"
))

compared <- c("x", "series", "cl", "ucl", "lcl")

analyse <- function(data, chart_type, ...) {
  autospc(data,
    chart_type = chart_type,
    period_min = 25L,
    max_exclusions = 0L,
    plot_chart = FALSE,
    ...
  )
}

autospc_rounded <- function(...) {
  previous <- options(autospc.rounded_constants = TRUE)
  on.exit(options(previous))

  analyse(...)
}


test_that("U agrees with qicharts2", {
  results <- analyse(u_e2e_data, "U")

  expect_equal(results[compared], correct_answer_U$u)
})


test_that("U' agrees with qicharts2 with the screening off", {
  results <- autospc_rounded(u_e2e_data, "U'", mr_screen_max_loops = 0L)

  expect_equal(results[compared], correct_answer_U$up)
})


test_that("U' agrees with qicharts2 screening the moving ranges once", {
  results <- autospc_rounded(u_e2e_data, "U'", mr_screen_max_loops = 1L)

  expect_equal(results[compared], correct_answer_U$up_screened)

  # the screening makes a difference to these data
  expect_false(isTRUE(all.equal(
    correct_answer_U$up_screened$ucl,
    correct_answer_U$up$ucl
  )))
})


test_that("the exact constants move the U' limits", {
  rounded <- autospc_rounded(u_e2e_data, "U'")
  exact <- analyse(u_e2e_data, "U'")

  expect_equal(exact$cl, rounded$cl)
  expect_false(isTRUE(all.equal(exact$ucl, rounded$ucl)))
})


test_that("several rows per subgroup give the same analysis as one", {
  # each subgroup's count and area of opportunity split across two rows, as
  # where each row is one ward, under column names that are not the defaults
  first_ward <- u_e2e_data %>%
    dplyr::mutate(
      infections = floor(y / 2),
      line_days = n / 3
    )

  second_ward <- u_e2e_data %>%
    dplyr::mutate(
      infections = y - floor(y / 2),
      line_days = n - n / 3
    )

  by_ward <- dplyr::bind_rows(first_ward, second_ward) %>%
    dplyr::select(month = x, infections, line_days)

  for (chart_type in c("U", "U'")) {
    from_subgroups <- analyse(u_e2e_data, chart_type)

    from_wards <- analyse(by_ward,
      chart_type,
      x = month,
      y = infections,
      n = line_days
    )

    expect_equal(
      from_wards[c("x", "y", "n", "series", "cl", "ucl", "lcl")],
      from_subgroups[c("x", "y", "n", "series", "cl", "ucl", "lcl")],
      info = chart_type
    )
  }
})


test_that("a subgroup with no count is a gap that leaves the limits alone", {
  # dropping a subgroup's count leaves it out of the centre line, so the
  # analysis of the other 24 rows is that of the data without it
  gapped <- u_e2e_data
  gapped$y[10] <- NA

  results <- autospc(gapped,
    chart_type = "U", period_min = 24L, max_exclusions = 0L,
    plot_chart = FALSE
  )

  without <- autospc(u_e2e_data[-10, ],
    chart_type = "U", period_min = 24L, max_exclusions = 0L,
    plot_chart = FALSE
  )

  expect_true(is.na(results$series[10]))
  expect_equal(results$cl[-10], without$cl)
  expect_equal(results$ucl[-10], without$ucl)
})


test_that("U and U' charts are drawn", {
  for (chart_type in c("U", "U'")) {
    plot <- autospc(u_e2e_data,
      chart_type = chart_type, period_min = 21L, max_exclusions = 0L
    )

    expect_s3_class(plot, "autospc_plot")
    expect_no_error(drawn(plot))
  }
})


test_that("each facet holds the analysis of the data up to its split", {
  for (chart_type in c("U", "U'")) {
    faceted <- facet_stages(u_e2e_data,
      split_at = c(21L, 25L),
      chart_type = chart_type,
      period_min = 21L,
      max_exclusions = 0L,
      plot_chart = FALSE
    )

    splits <- c(21L, 25L)

    for (i in seq_along(splits)) {
      stage <- faceted[faceted$stage == as.character(i), ]

      alone <- autospc(u_e2e_data[seq_len(splits[[i]]), ],
        chart_type = chart_type,
        period_min = 21L,
        max_exclusions = 0L,
        plot_chart = FALSE
      )

      expect_equal(stage$cl, alone$cl, info = chart_type)
      expect_equal(stage$ucl, alone$ucl, info = chart_type)
    }
  }
})


test_that("facet_stages draws U and U' charts", {
  for (chart_type in c("U", "U'")) {
    plot <- facet_stages(u_e2e_data,
      split_at = c(21L, 25L),
      chart_type = chart_type,
      period_min = 21L,
      max_exclusions = 0L
    )

    expect_no_error(drawn(plot))
  }
})
