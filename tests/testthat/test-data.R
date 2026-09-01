# The bundled datasets. Their dimensions are asserted here against what the help
# pages say, so that a regenerated dataset cannot quietly disagree with them.


test_that("ed_attendances_monthly holds 109 consecutive month starts", {
  expect_identical(
    ed_attendances_monthly$month_start,
    seq(as.Date("2015-06-01"), by = "month", length.out = 109L)
  )
})


test_that("ed_attendances_monthly has the columns its help page lists", {
  expect_identical(
    names(ed_attendances_monthly),
    c(
      "month_start", "att_all", "within_4h", "over_4h", "percent_in_4h",
      "e_adm_over_4h", "e_adm_via_ed"
    )
  )
})


test_that("the bundled datasets have the dimensions their help pages give", {
  expect_identical(dim(ed_attendances_monthly), c(109L, 7L))

  expect_identical(dim(example_series_1), c(125L, 2L))

  expect_identical(dim(example_series_2a), c(43L, 2L))

  expect_identical(dim(example_series_2b), c(43L, 2L))

  expect_identical(dim(example_series_2c), c(47L, 2L))
})
