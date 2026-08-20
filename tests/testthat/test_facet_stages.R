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
