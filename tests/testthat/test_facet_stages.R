# Tests for facet_stages

test_that("facet_stages produces correct data output", {
  
  faceted_results <- facet_stages(
    ed_attendances_monthly,
    split_rows = c(30L, 60L, 90L),
    chart_type = "C'",
    x = month_start,
    y = att_all, 
    plot_chart = FALSE
  )
  
  stage2 <- autospc(ed_attendances_monthly %>%
                      dplyr::filter(dplyr::row_number() <= 60L),
                    chart_type = "C'",
                    x = month_start,
                    y = att_all, 
                    plot_chart = FALSE)
  
  expect_equal(faceted_results %>%
                 dplyr::group_by(stage) %>%
                 dplyr::summarise(nr = dplyr::n()) %>%
                 dplyr::pull(nr),
               c(30L,
                 60L,
                 90L,
                 nrow(ed_attendances_monthly)))
  
  expect_equal(faceted_results %>%
                 dplyr::filter(stage == 2) %>%
                 dplyr::select(x,
                               y,
                               ucl,
                               lcl,
                               cl,
                               periodType,
                               excluded,
                               log,
                               breakPoint,
                               rule1,
                               rule2,
                               aboveOrBelowCl,
                               highlight,
                               limitChange,
                               cl_label,
                               cl_change,
                               annotation_level,
                               annotation_curvature,
                               periodStart,
                               plotPeriod),
               stage2 %>%
                 dplyr::select(x,
                               y,
                               ucl,
                               lcl,
                               cl,
                               periodType,
                               excluded,
                               log,
                               breakPoint,
                               rule1,
                               rule2,
                               aboveOrBelowCl,
                               highlight,
                               limitChange,
                               cl_label,
                               cl_change,
                               annotation_level,
                               annotation_curvature,
                               periodStart,
                               plotPeriod))
  
})


test_that("", {
  
  faceted_plot <- facet_stages(
    ed_attendances_monthly,
    split_rows = c(30L, 60L, 90L),
    chart_type = "C'",
    x = month_start,
    y = att_all, 
    plot_chart = TRUE
  )
  
  faceted_build <- ggplot2::ggplot_build(faceted_plot)
  
  expect_equal(length(levels(faceted_build$data[[1]]$PANEL)),
               4L)
  
})


test_that("facet_stages works when relying on x,y columns in data", {
  
  set.seed(1234L)
  
  result <- facet_stages(tibble::tibble(x = 1L:100L,
                                        y = rnorm(n = 100L)),
                         split_rows = c(30L, 60L),
                         chart_type = "XMR",
                         plot_chart = FALSE)
  
  result_lengths <- result %>%
    dplyr::group_by(stage) %>%
    dplyr::summarise(stage_length = dplyr::n()) %>%
    dplyr::pull(stage_length)
  
  expect_equal(result_lengths,
               c(30L,60L,100L))
  
})


test_that("an XMR request is faceted as its X chart", {

  plot <- suppressWarnings(
    facet_stages(data.frame(x = 1L:60L,
                            y = rep(c(10, 12, 11, 13, 9, 14), 10L)),
                 split_rows = c(30L, 60L),
                 chart_type = "XMR")
  )

  y_range <- ggplot2::ggplot_build(plot)$layout$panel_params[[1]]$y.range

  # chart_type = "XMR" is rewritten to "X", so one chart is drawn and the axis
  # comes from the chart object built for it.
  # y_axis_range.autospc_chart_mr() always starts the axis at zero; the X one
  # starts below the lowest of lcl and y, which for this series is above zero.
  expect_gt(y_range[1], 0)

})


# the arguments reach every facet


facet_arg_data <- data.frame(x = 1:90,
                             y = rep(c(50, 48, 49, 51, 52, 47), 15L))

faceted_plot <- function(...) {
  facet_stages(facet_arg_data,
               split_rows = c(30L, 60L, 90L),
               chart_type = "C",
               period_min = 21L,
               ...)
}


faceted_arg <- function(...) {
  facet_stages(facet_arg_data,
               split_rows = c(30L, 60L, 90L),
               chart_type = "C",
               plot_chart = FALSE,
               ...)
}


test_that("the chart parameters reach the analysis of each facet", {

  # period_min is a chart parameter, so it has to travel from facet_stages()
  # through to every chart it builds
  expect_false(isTRUE(all.equal(faceted_arg(period_min = 15L)$cl,
                               faceted_arg(period_min = 30L)$cl)))

})


test_that("the last facet is the whole series, analysed as autospc would", {

  faceted <- faceted_arg(period_min = 15L)

  last <- faceted[faceted$stage == "3", ]

  whole <- autospc(facet_arg_data,
                   chart_type = "C",
                   period_min = 15L,
                   plot_chart = FALSE)

  expect_equal(last$cl, whole$cl)

  expect_equal(last$ucl, whole$ucl)

  expect_equal(last$lcl, whole$lcl)

})


test_that("an argument the caller did not give takes autospc's default", {

  expect_equal(faceted_arg()$cl,
               faceted_arg(period_min = formals(autospc)$period_min)$cl)

})


# autospc_argument_values()


test_that("a default written as an expression comes back as a value", {

  values <- autospc_argument_values(list())

  # autospc() defaults basic_annotations to getRversion() < "4.3.0", so
  # formals() hands it over as an unevaluated call
  expect_type(values$basic_annotations, "logical")

  expect_identical(values$basic_annotations, getRversion() < "4.3.0")

})


test_that("a value the caller gave wins over the default", {

  values <- autospc_argument_values(list(point_size = 7))

  expect_identical(values$point_size, 7)

  expect_identical(values$line_width_sf, formals(autospc)$line_width_sf)

})


test_that("the annotation positioning autospc would choose is what is drawn", {

  # tautological on R 4.3 and later, where both defaults are FALSE. It is the
  # earlier versions this pins: autospc() defaults basic_annotations to
  # getRversion() < "4.3.0" and create_spc_plot() defaults it to FALSE, so a
  # faceted chart used to lose the basic positioning that autospc() gives.
  drawn_default <- ggplot2::ggplot_build(faceted_plot())$data

  drawn_explicit <- ggplot2::ggplot_build(
    faceted_plot(basic_annotations = getRversion() < "4.3.0")
  )$data

  expect_equal(drawn_default, drawn_explicit)

})


# the axes and the caption describe every facet


test_that("the axes hold every facet, not just the first", {

  plot <- faceted_plot()

  panel <- ggplot2::ggplot_build(plot)$layout$panel_params[[1]]

  drawn <- faceted_plot(plot_chart = FALSE)

  expect_gte(panel$x.range[2], max(drawn$x))

  expect_gte(panel$y.range[2], max(drawn$ucl, drawn$y, na.rm = TRUE))

  expect_lte(panel$y.range[1], min(drawn$lcl, drawn$y, na.rm = TRUE))

})


test_that("the caption names the chart type that was asked for", {

  plot <- facet_stages(facet_arg_data,
                       split_rows = c(30L, 60L, 90L),
                       chart_type = "C'",
                       period_min = 21L)

  expect_match(plot$labels$caption, "C' Shewhart Chart", fixed = TRUE)

})


test_that("x_break reaches the drawing", {

  breaks <- function(...) {
    ggplot2::ggplot_build(faceted_plot(...))$layout$panel_params[[1]]$x$breaks
  }

  expect_false(isTRUE(all.equal(breaks(x_break = 10), breaks(x_break = 30))))

})
