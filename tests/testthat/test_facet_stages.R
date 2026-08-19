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

  # show_mr is forced to FALSE, so one chart is drawn and the axis comes from
  # the chart object built for it. y_axis_range.autospc_chart_mr() always starts
  # the axis at zero; the X one starts below the lowest of lcl and y, which for
  # this series is well above zero.
  expect_gt(y_range[1], 0)

})
