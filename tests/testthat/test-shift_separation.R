test_that("Consecutive opposite shifts are considered distinct",{
  
  result_df <- autospc(ed_attendances_monthly,
                          x = month_start,
                          y = within_4h,
                          chart_type = "C'",
                          plot_chart = FALSE)
  
  result <- result_df %>% 
    dplyr::group_by(plot_period) %>%
    dplyr::summarise(period_type = dplyr::first(period_type),
                     first_point = dplyr::first(period_start),
                     num_points = dplyr::n()) %>%
    dplyr::arrange(first_point)
  
  # In this data, point 22 is part of a rule break downwards that starts during
  # the first calculation period. It is followed by a rule break upwards,
  # starting at point 24. Therefore point 22 should not be a candidate for
  # re-establishing limits. Limits should be re-established at point 50 only,
  # therefore there should be two sets of limits, with the second set commencing
  # at point 50.
  
  num_calc_periods <- result %>%
    dplyr::filter(period_type == "calculation") %>%
    nrow()
  
  calc_period_starts <- result %>%
    dplyr::filter(period_type == "calculation") %>%
    dplyr::pull(first_point)
  
  expect_equal(num_calc_periods,
               2L)
  
  expect_equal(calc_period_starts[2],
               50L)
  
})

