test_that("autospc errors informatively for invalid chart_type", {
  
  expect_error(
    autospc(
      ed_attendances_monthly,
      chart_type = "XmR'",
      x = month_start,
      y = att_all
    ),
    "Available chart types are"
  )
  
})
