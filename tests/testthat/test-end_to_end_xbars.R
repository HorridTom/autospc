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
