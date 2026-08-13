# TEMPORARY - delete this whole file. See CLEAN UP #16 in the worklist.
#
# These tests answer one question: is the new code path reached at all during a
# real autospc() run? They exist because the refactor is behaviour preserving,
# so the old chart_type branches and the new calculate_limits() methods produce
# identical numbers, and a passing suite cannot tell which one ran.
#
# Once the old branches are gone and the methods are the only way limits get
# calculated, every end to end test covers this and the file can go.

# Runs `run`, with all six calculate_limits() methods replaced by recorders that
# pass the call on to the real method, and returns the chart classes whose
# methods were used. The mocking has to happen in the frame that runs the call,
# because local_mocked_bindings() is undone when that frame exits.
#
# `run` is a function rather than arguments forwarded through `...`, because
# autospc() cannot be called with a forwarded `...` when chart_type = "XMR":
# the re-invocation at autospc.R:357-363 uses match.call() and re-evaluates the
# captured call, which then contains the `...` symbol and fails with "'...'
# used in an incorrect context". Pre-existing - see CLEAN UP #17.
methods_used <- function(run) {

  hits <- character()

  suffixes <- c("c", "cp", "p", "pp", "x", "mr")
  method_names <- paste0("calculate_limits.autospc_chart_", suffixes)
  real_methods <- mget(method_names, envir = asNamespace("autospc"))

  recorder <- function(suffix, real_method) {
    force(suffix)
    force(real_method)
    function(chart, period, exclusion_points) {
      hits <<- c(hits, suffix)
      real_method(chart = chart,
                  period = period,
                  exclusion_points = exclusion_points)
    }
  }

  mocks <- stats::setNames(
    Map(recorder, suffixes, real_methods),
    method_names
  )

  do.call(testthat::local_mocked_bindings,
          c(mocks, list(.package = "autospc", .env = environment())))

  invisible(run())

  return(unique(hits))

}


run_path_data <- autospc::ed_attendances_monthly


test_that("each count chart type reaches its own calculate_limits method", {

  expect_identical(
    methods_used(function() autospc(run_path_data, chart_type = "C",
                                    x = month_start, y = att_all,
                                    plot_chart = FALSE)),
    "c")

  expect_identical(
    methods_used(function() autospc(run_path_data, chart_type = "C'",
                                    x = month_start, y = att_all,
                                    plot_chart = FALSE)),
    "cp")

})


test_that("each proportion chart type reaches its own calculate_limits method", {

  expect_identical(
    methods_used(function() autospc(run_path_data, chart_type = "P",
                                    x = month_start, y = within_4h,
                                    n = att_all, plot_chart = FALSE)),
    "p")

  expect_identical(
    methods_used(function() autospc(run_path_data, chart_type = "P'",
                                    x = month_start, y = within_4h,
                                    n = att_all, plot_chart = FALSE)),
    "pp")

})


test_that("an MR chart reaches the MR method", {

  expect_identical(
    methods_used(function() autospc(run_path_data, chart_type = "MR",
                                    x = month_start, y = att_all,
                                    plot_chart = FALSE)),
    "mr")

})


test_that("an XMR run reaches both the X and the MR method", {

  # the X chart from the top level run, the MR chart from the
  # chart_type = "MR" re-invocation in autospc()
  expect_setequal(
    methods_used(function() autospc(run_path_data, chart_type = "XMR",
                                    x = month_start, y = att_all,
                                    plot_chart = FALSE)),
    c("x", "mr"))

})


test_that("no chart type reaches a method belonging to another", {

  # the check above is per chart type; this one pins that nothing reaches a
  # second, wrong method as well - which the identical() calls above would
  # already catch, but not with a message saying so
  expect_length(
    methods_used(function() autospc(run_path_data, chart_type = "C'",
                                    x = month_start, y = att_all,
                                    plot_chart = FALSE)),
    1)

})


test_that("autospc() builds a chart object during a real run", {

  # Nothing reads the chart object yet, so if chart_type_for_object() started
  # returning NULL for everything, no object would ever be built and every
  # other test would still pass. This is the only test that checks autospc()
  # really does call autospc_chart(). Once the object is used, the end to end
  # tests cover that and this one should go.
  built <- autospc_chart

  called <- NULL

  testthat::local_mocked_bindings(
    autospc_chart = function(chart_type, ...) {
      called <<- chart_type
      built(chart_type = chart_type, ...)
    }
  )

  invisible(autospc(autospc::ed_attendances_monthly,
                    chart_type = "C'",
                    x = month_start,
                    y = att_all,
                    plot_chart = FALSE))

  expect_identical(called, "C'")

})
