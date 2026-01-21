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

test_that("autospc errors when chart_type is NULL", {
  expect_error(
    autospc(
      ed_attendances_monthly,
      chart_type = NULL,
      x = month_start,
      y = att_all
    ),
    "pass the desired chart type"
  )
})

test_that("autospc errors when chart_type has length > 1", {
  expect_error(
    autospc(
      ed_attendances_monthly,
      chart_type = c("XMR", "MR"),
      x = month_start,
      y = att_all
    ),
    "length one"
  )
})

test_that("autospc errors when chart_type is not character", {
  expect_error(
    autospc(
      ed_attendances_monthly,
      chart_type = 1,
      x = month_start,
      y = att_all
    ),
    "character"
  )
})
