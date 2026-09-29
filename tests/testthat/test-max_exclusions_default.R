# Until autospc 0.3.0, when the default of max_exclusions changes from 3 to 0,
# a call that relies on the default and excludes points warns that its results
# will change.

default_warning <- "autospc_max_exclusions_default_warning"

# ed_attendances_monthly on a C' chart excludes points under the default;
# example_series_2b on a C chart excludes none
excluding <- function(...) {
  autospc(ed_attendances_monthly,
    chart_type = "C'", x = month_start, y = att_all, plot_chart = FALSE, ...
  )
}


test_that("the default warns where it excluded points", {
  expect_warning(excluding(), class = default_warning)
})


test_that("the warning says the default changes to 0 in 0.3.0", {
  expect_warning(excluding(), "change to 0 in autospc 0.3.0")
})


test_that("a drawn chart warns as a table does", {
  expect_warning(
    autospc(ed_attendances_monthly,
      chart_type = "C'", x = month_start, y = att_all
    ),
    class = default_warning
  )
})


test_that("max_exclusions = 3 silences the warning and changes nothing", {
  expect_no_warning(
    explicit <- excluding(max_exclusions = 3L),
    class = default_warning
  )

  by_default <- suppressWarnings(excluding())

  expect_identical(explicit, by_default)
})


test_that("max_exclusions = 0 silences the warning", {
  expect_no_warning(excluding(max_exclusions = 0L), class = default_warning)
})


test_that("the default does not warn where it excluded nothing", {
  result <- expect_no_warning(
    autospc(example_series_2b, chart_type = "C", plot_chart = FALSE),
    class = default_warning
  )

  expect_false(any(result$excluded %in% TRUE))
})


test_that("facet_stages warns once for the call, however many facets", {
  warnings_given <- 0L

  withCallingHandlers(
    facet_stages(ed_attendances_monthly,
      split_at = c(30L, 60L, 90L), chart_type = "C'",
      x = month_start, y = att_all, plot_chart = FALSE
    ),
    autospc_max_exclusions_default_warning = function(w) {
      warnings_given <<- warnings_given + 1L
      invokeRestart("muffleWarning")
    }
  )

  expect_identical(warnings_given, 1L)
})


test_that("facet_stages does not warn when max_exclusions is set", {
  expect_no_warning(
    facet_stages(ed_attendances_monthly,
      split_at = c(30L, 60L, 90L), chart_type = "C'",
      x = month_start, y = att_all, max_exclusions = 3L, plot_chart = FALSE
    ),
    class = default_warning
  )
})
