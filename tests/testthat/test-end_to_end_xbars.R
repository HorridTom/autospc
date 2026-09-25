# The XbarS chart through autospc() and facet_stages(). The analysis of each
# half is tested in test-autospc_chart_xbar.R and test-autospc_chart_s.R; these
# tests check that the pair is assembled from those two analyses.

xbars_sizes <- c(
  5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 8, 3, 1, 6, 5, 7, 2, 5, 5, 5, 5,
  4, 9, 5, 1, 6, 3, 5, 7, 5
)

# one row per observation
xbars_observations <- function(seed = 42) {
  set.seed(seed)

  data.frame(
    x = rep(seq_along(xbars_sizes), xbars_sizes),
    y = stats::rnorm(sum(xbars_sizes), 50, 5)
  )
}

# one row per subgroup, holding its mean, size and standard deviation, under
# column names that are not the defaults
xbars_summaries <- function(observations = xbars_observations()) {
  observations %>%
    dplyr::summarise(
      subgroup_mean = mean(y),
      size_col = dplyr::n(),
      sd_col = stats::sd(y),
      .by = x
    )
}

s_columns <- c("subgroup_s", "scl", "s_ucl", "s_lcl")


# qicharts2 cross-check for equal subgroup sizes. Where every subgroup is the
# same size, qicharts2's Xbar and S centre lines and limits are Provost and
# Murray's. correct_answer_XbarS was generated with qicharts2 0.8.1:
#
# make_data <- function(size, seed) {
#   set.seed(seed)
#   data.frame(
#     x = rep(1:25, each = size),
#     y = round(stats::rnorm(25 * size, 50, 5), 1)
#   )
# }
# xbars_e2e_data <- list(
#   size_5 = make_data(5L, 1605),
#   size_8 = make_data(8L, 1606)
# )
# correct_answer_XbarS <- lapply(xbars_e2e_data, function(d) {
#   q_x <- qicharts2::qic(x, y, data = d, chart = "xbar", return.data = TRUE)
#   q_s <- qicharts2::qic(x, y, data = d, chart = "s", return.data = TRUE)
#   data.frame(
#     x = q_x$x, y = q_x$y, cl = q_x$cl, ucl = q_x$ucl, lcl = q_x$lcl,
#     subgroup_s = q_s$y, scl = q_s$cl, s_ucl = q_s$ucl, s_lcl = q_s$lcl
#   )
# })
#
# Subgroups of 5 have an S chart lower limit of zero, and subgroups of 8 one
# above zero.

xbars_e2e_data <- readRDS(file.path("testdata", "test_xbars_e2e_data.rds"))

correct_answer_XbarS <- readRDS(file.path(
  "testdata",
  "test_data_end_to_end",
  "correct_answer_XbarS.rds"
))


test_that("XbarS agrees with qicharts2 for equal subgroup sizes", {
  for (size in names(xbars_e2e_data)) {
    results <- autospc(xbars_e2e_data[[size]],
      chart_type = "XbarS",
      period_min = 25L,
      plot_chart = FALSE
    )

    expected <- correct_answer_XbarS[[size]]

    expect_equal(results[names(expected)], expected, info = size)
  }
})


test_that("an XbarS table holds the Xbar analysis and the S analysis", {
  pair_table <- autospc(xbars_observations(),
    chart_type = "XbarS",
    plot_chart = FALSE
  )

  xbar_table <- autospc(xbars_observations(),
    chart_type = "Xbar",
    plot_chart = FALSE
  )

  s_table <- autospc(xbars_observations(),
    chart_type = "S",
    plot_chart = FALSE
  )

  expect_setequal(names(pair_table), c(names(xbar_table), s_columns))

  expect_identical(pair_table[names(xbar_table)], xbar_table)

  expect_identical(
    pair_table[s_columns],
    s_table %>%
      dplyr::select(subgroup_s = series, scl = cl, s_ucl = ucl, s_lcl = lcl)
  )
})


test_that("XbarS gives the same analysis from observations and summaries", {
  from_observations <- autospc(xbars_observations(),
    chart_type = "XbarS",
    plot_chart = FALSE
  )

  from_summaries <- autospc(xbars_summaries(),
    x = x,
    y = subgroup_mean,
    n = size_col,
    s = sd_col,
    chart_type = "XbarS",
    plot_chart = FALSE
  )

  compared <- c("x", "y", "n", "cl", "ucl", "lcl", s_columns)

  expect_equal(from_summaries[compared], from_observations[compared])
})


test_that("an XbarS chart is drawn as two panels", {
  plot <- autospc(xbars_observations(), chart_type = "XbarS")

  expect_s3_class(plot, "autospc_plot")

  expect_no_error(drawn(plot))

  texts <- panel_texts(plot)

  expect_true(all(c("Xbar", "S") %in% texts))
})


test_that("an XbarS request is faceted as its Xbar chart", {
  from_pair <- suppressWarnings(
    facet_stages(xbars_observations(),
      split_at = c(15L, 30L),
      chart_type = "XbarS",
      plot_chart = FALSE
    )
  )

  from_xbar <- suppressWarnings(
    facet_stages(xbars_observations(),
      split_at = c(15L, 30L),
      chart_type = "Xbar",
      plot_chart = FALSE
    )
  )

  expect_identical(from_pair, from_xbar)
})


test_that("facets give the same analysis from observations and summaries", {
  from_observations <- suppressWarnings(
    facet_stages(xbars_observations(),
      split_at = c(15L, 30L),
      chart_type = "XbarS",
      plot_chart = FALSE
    )
  )

  from_summaries <- suppressWarnings(
    facet_stages(xbars_summaries(),
      split_at = c(15L, 30L),
      x = x,
      y = subgroup_mean,
      n = size_col,
      s = sd_col,
      chart_type = "XbarS",
      plot_chart = FALSE
    )
  )

  compared <- c("stage", "x", "y", "n", "cl", "ucl", "lcl")

  expect_equal(from_summaries[compared], from_observations[compared])
})


test_that("show_mr = FALSE with XbarS draws the Xbar chart on its own", {
  lifecycle::expect_deprecated(
    table <- autospc(xbars_observations(),
      chart_type = "XbarS",
      show_mr = FALSE,
      plot_chart = FALSE
    )
  )

  expect_identical(
    table,
    autospc(xbars_observations(), chart_type = "Xbar", plot_chart = FALSE)
  )
})
