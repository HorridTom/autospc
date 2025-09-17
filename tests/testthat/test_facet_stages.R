# Tests for facet_stages

test_that("facet_stages produces correct data output", {
  
  faceted_results <- facet_stages(
    ed_attendances_monthly,
    split_rows = c(30L, 60L, 90L),
    chartType = "C'",
    x = Month_Start,
    y = Att_All, 
    plotChart = FALSE
  )
  
  stage2 <- plot_auto_SPC(ed_attendances_monthly %>%
                            dplyr::filter(dplyr::row_number() <= 60L),
                          chartType = "C'",
                          x = Month_Start,
                          y = Att_All, 
                          plotChart = FALSE)
  
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
    chartType = "C'",
    x = Month_Start,
    y = Att_All, 
    plotChart = TRUE
  )
  
  faceted_build <- ggplot2::ggplot_build(faceted_plot)
  
  expect_equal(length(levels(faceted_build$data[[1]]$PANEL)),
               4L)
  
})
