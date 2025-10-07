# Tests for variants of SSA selected using overhangingReversions and noRegrets

calc_period_starts <- function(df) {
  
  cps <- df %>%
    dplyr::group_by(periodStart,
                    periodType) %>%
    dplyr::summarise(npoints = dplyr::n(),
                     .groups = "drop") %>%
    dplyr::filter(periodType == "calculation") %>%
    dplyr::arrange(periodStart) %>%
    dplyr::pull(periodStart)
  
  return(cps)
  
}


test_that(
  "SSA variants perform correctly on first 96 points of example_series_1", {
    
    result_ssa <- plot_auto_SPC(df = example_series_1[1:96,],
                                chartType = "XMR",
                                plotChart = FALSE,
                                noRegrets = TRUE,
                                overhangingReversions = TRUE)
    
    result_nRF_oRT <- plot_auto_SPC(df = example_series_1[1:96,],
                                    chartType = "XMR",
                                    plotChart = FALSE,
                                    noRegrets = FALSE,
                                    overhangingReversions = TRUE)
    
    result_nRF_oRF <- plot_auto_SPC(df = example_series_1[1:96,],
                                    chartType = "XMR",
                                    plotChart = FALSE,
                                    noRegrets = FALSE,
                                    overhangingReversions = FALSE)
    
    calc_period_starts_ssa <- result_ssa %>%
      calc_period_starts()
    
    calc_period_starts_nRF_oRT <- result_nRF_oRT %>%
      calc_period_starts()
    
    calc_period_starts_nRF_oRF <- result_nRF_oRF %>%
      calc_period_starts()
    
    expect_equal(calc_period_starts_ssa,
                 c(1, 53))
    
    expect_equal(calc_period_starts_nRF_oRT,
                 c(1, 53, 74))
    
    expect_equal(calc_period_starts_nRF_oRF,
                 c(1, 31, 52, 74))
    
  })


test_that(
  "SSA variants perform correctly on full example_series_1", {
    
    result_ssa <- plot_auto_SPC(df = example_series_1,
                                chartType = "XMR",
                                plotChart = FALSE,
                                noRegrets = TRUE,
                                overhangingReversions = TRUE)
    
    result_nRF_oRT <- plot_auto_SPC(df = example_series_1,
                                    chartType = "XMR",
                                    plotChart = FALSE,
                                    noRegrets = FALSE,
                                    overhangingReversions = TRUE)
    
    result_nRF_oRF <- plot_auto_SPC(df = example_series_1,
                                    chartType = "XMR",
                                    plotChart = FALSE,
                                    noRegrets = FALSE,
                                    overhangingReversions = FALSE)
    
    calc_period_starts_ssa <- result_ssa %>%
      calc_period_starts()
    
    calc_period_starts_nRF_oRT <- result_nRF_oRT %>%
      calc_period_starts()
    
    calc_period_starts_nRF_oRF <- result_nRF_oRF %>%
      calc_period_starts()
    
    expect_equal(calc_period_starts_ssa,
                 c(1, 53, 74))
    
    expect_equal(calc_period_starts_nRF_oRT,
                 c(1, 53, 74))
    
    expect_equal(calc_period_starts_nRF_oRF,
                 c(1, 31, 52, 74, 98))
    
  })


test_that("warning is issued if incompatible variant requested", {
  
  expect_warning(
    plot_auto_SPC(
      ed_attendances_monthly, 
      chartType = "C'", 
      x = Month_Start, 
      y = Att_All,
      noRegrets = TRUE,
      overhangingReversions = FALSE
    ),
    "noRegrets requires consideration of overhanging reversions")
  
})
