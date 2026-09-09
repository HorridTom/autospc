test_data <- readRDS("testdata/test_data.rds")

test_that("Linetypes are formed correctly", {
  test_plt <- autospc(test_data,
    chart_type = "C'"
  )

  # layer 1 holds the centre line and control limits
  layer_1 <- ggplot2::layer_data(test_plt, 1) %>%
    dplyr::arrange(x)
  # layer 2 holds the data series
  layer_2 <- ggplot2::layer_data(test_plt, 2) %>%
    dplyr::arrange(x)

  # layer 1 has 3n rows, as it holds three series; then an additional 3*3 = 9
  # rows for the additional limit connectors added for continuity of limit lines
  expect_equal(nrow(layer_1), 450L + 9L)
  # layer 2 has n rows, as it holds one series
  expect_equal(nrow(layer_2), 150L)

  rle_layer_1 <- rle(layer_1$linetype)
  rle_layer_2 <- rle(layer_2$linetype)

  correct_answer_1 <- structure(
    list(
      lengths = rep(
        c(1L, 2L),
        153
      ),
      values = rep(
        c("solid", "42"),
        153
      )
    ),
    class = "rle"
  )
  correct_answer_2 <- structure(
    list(
      lengths = 150L,
      values = "solid"
    ),
    class = "rle"
  )

  expect_identical(rle_layer_1, correct_answer_1)
  expect_identical(rle_layer_2, correct_answer_2)
})


test_that("the centre line is drawn thicker than the limits and the series", {
  # the widths are keyed by plotted_line, so a mistake there would give one of
  # the four lines another's width with nothing to say so
  test_plt <- autospc(test_data,
    chart_type = "C'"
  )

  limits <- ggplot2::layer_data(test_plt, 1)
  series <- ggplot2::layer_data(test_plt, 2)

  # the centre line is the solid one in layer 1, the control limits the dashed
  centre <- unique(limits$linewidth[limits$linetype == "solid"])
  control <- unique(limits$linewidth[limits$linetype == "42"])
  plotted <- unique(series$linewidth)

  expect_length(centre, 1L)
  expect_length(control, 1L)
  expect_length(plotted, 1L)

  expect_gt(centre, control)
  expect_identical(plotted, control)
})
