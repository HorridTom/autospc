# autospc() selects chart_args and passed from the arguments of the call, by the
# names in autospc_chart_parameters() and autospc_plot_passed_elements(). These
# tests check that every parameter named in those two functions arrives where it
# is meant to.

argument_data <- data.frame(x = 1:30,
                            y = rep(c(10L, 12L, 11L, 13L, 9L, 14L), 5L))

# One value per chart parameter, different from that parameter's default. The
# values only need to be valid: no assertion below is about what the analysis
# does with them.
chart_argument_alternatives <- list(
  period_min = 25,
  baseline_length = 25L,
  shift_rule_threshold = 7L,
  baseline_only = TRUE,
  establish_every_shift = TRUE,
  no_regrets = FALSE,
  overhanging_reversions = FALSE,
  max_exclusions = 1,
  mr_screen_max_loops = 2L,
  centre_line_tolerance = 0.5,
  floating_median = "yes",
  floating_median_n = 8L,
  keep_candidate_tables = TRUE
)


test_that("the deprecated arguments are the ones autospc declares as deprecated", {

  # the deprecated arguments are found by their default rather than named, and
  # this is the list that is expected
  expect_setequal(autospc_deprecated_arguments(),
                  c("show_mr",
                    "write_table",
                    "override_annotation_dist",
                    "override_annotation_dist_P"))

})


test_that("the alternatives cover every chart parameter", {

  # a chart parameter added later has to be given a value here before the test
  # below can cover it, so this check fails until it is
  expect_setequal(names(chart_argument_alternatives),
                  autospc_chart_parameters())

})


test_that("every chart parameter autospc takes reaches the chart", {

  for(parameter in autospc_chart_parameters()) {

    given <- chart_argument_alternatives[parameter]

    from_autospc <- suppressWarnings(
      rlang::exec(autospc,
                  argument_data,
                  chart_type = "C",
                  !!!given)
    )

    # autospc() changes the value of overhanging_reversions when it is FALSE
    # and no_regrets is TRUE, through validate_algorithm_parameters(), so the
    # comparison is against the validated value rather than the value passed in
    expected <- given[[parameter]]

    if(identical(parameter, "overhanging_reversions")) {
      validated <- suppressWarnings(
        validate_algorithm_parameters(
          list(no_regrets = autospc_default("no_regrets"),
               overhanging_reversions = expected)
        )
      )
      expected <- validated$overhanging_reversions
    }

    expect_identical(autospc_plot_charts(from_autospc)[[1]][[parameter]],
                     expected,
                     info = parameter)

  }

})


test_that("every presentation parameter autospc takes reaches the plot object", {

  # These six are not compared: where the caller passes NULL, autospc() replaces
  # it with a value taken from the data or the chart type, so the value recorded
  # on the plot object is not the value passed in.
  resolved <- c("title", "subtitle",
                "override_x_title", "override_y_title",
                "upper_annotation_sf", "lower_annotation_sf")

  plot <- suppressWarnings(
    autospc(argument_data, chart_type = "C", period_min = 21L)
  )

  expect_setequal(names(autospc_plot_passed(plot)),
                  autospc_plot_passed_elements())

  for(parameter in setdiff(autospc_plot_passed_elements(), resolved)) {

    given <- list(1.5)
    names(given) <- parameter

    # 1.5 is a valid value for the numeric parameters; the logical and character
    # parameters need a value of their own type
    if(is.logical(formals(autospc)[[parameter]])) {
      given[[parameter]] <- !formals(autospc)[[parameter]]
    }

    if(is.character(formals(autospc)[[parameter]])) {
      given[[parameter]] <- "given"
    }

    drawn_with <- suppressWarnings(
      rlang::exec(autospc,
                  argument_data,
                  chart_type = "C",
                  period_min = 21L,
                  !!!given)
    )

    expect_identical(autospc_plot_passed(drawn_with, parameter),
                     given[[parameter]],
                     info = parameter)

  }

})
