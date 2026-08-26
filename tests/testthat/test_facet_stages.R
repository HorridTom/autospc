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

  # chart_type = "XMR" is rewritten to "X", so one chart is drawn and the axis
  # comes from the chart object built for it.
  # y_axis_range.autospc_chart_mr() always starts the axis at zero; the X one
  # starts below the lowest of lcl and y, which for this series is above zero.
  expect_gt(y_range[1], 0)

})


# the arguments reach every facet


facet_arg_data <- data.frame(x = 1:90,
                             y = rep(c(50, 48, 49, 51, 52, 47), 15L))

faceted_plot <- function(...) {
  facet_stages(facet_arg_data,
               split_rows = c(30L, 60L, 90L),
               chart_type = "C",
               period_min = 21L,
               ...)
}


faceted_arg <- function(...) {
  facet_stages(facet_arg_data,
               split_rows = c(30L, 60L, 90L),
               chart_type = "C",
               plot_chart = FALSE,
               ...)
}


test_that("the chart parameters reach the analysis of each facet", {

  # period_min is a chart parameter, so it has to travel from facet_stages()
  # through to every chart it builds
  expect_false(isTRUE(all.equal(faceted_arg(period_min = 15L)$cl,
                               faceted_arg(period_min = 30L)$cl)))

})


test_that("the last facet is the whole series, analysed as autospc would", {

  faceted <- faceted_arg(period_min = 15L)

  last <- faceted[faceted$stage == "3", ]

  whole <- autospc(facet_arg_data,
                   chart_type = "C",
                   period_min = 15L,
                   plot_chart = FALSE)

  expect_equal(last$cl, whole$cl)

  expect_equal(last$ucl, whole$ucl)

  expect_equal(last$lcl, whole$lcl)

})


test_that("an argument the caller did not give takes autospc's default", {

  expect_equal(faceted_arg()$cl,
               faceted_arg(period_min = formals(autospc)$period_min)$cl)

})


# autospc_argument_values()


test_that("a default written as an expression comes back as a value", {

  values <- autospc_argument_values(list())

  # autospc() defaults basic_annotations to getRversion() < "4.3.0", so
  # formals() hands it over as an unevaluated call
  expect_type(values$basic_annotations, "logical")

  expect_identical(values$basic_annotations, getRversion() < "4.3.0")

})


test_that("a value the caller gave wins over the default", {

  values <- autospc_argument_values(list(point_size = 7))

  expect_identical(values$point_size, 7)

  expect_identical(values$line_width_sf, formals(autospc)$line_width_sf)

})


test_that("the annotation positioning autospc would choose is what is drawn", {

  # tautological on R 4.3 and later, where both defaults are FALSE. It is the
  # earlier versions this pins: autospc() defaults basic_annotations to
  # getRversion() < "4.3.0" and create_spc_plot() defaults it to FALSE, so a
  # faceted chart used to lose the basic positioning that autospc() gives.
  drawn_default <- ggplot2::ggplot_build(faceted_plot())$data

  drawn_explicit <- ggplot2::ggplot_build(
    faceted_plot(basic_annotations = getRversion() < "4.3.0")
  )$data

  expect_equal(drawn_default, drawn_explicit)

})


# the axes and the caption describe every facet


test_that("the axes hold every facet, not just the first", {

  plot <- faceted_plot()

  panel <- ggplot2::ggplot_build(plot)$layout$panel_params[[1]]

  drawn <- faceted_plot(plot_chart = FALSE)

  expect_gte(panel$x.range[2], max(drawn$x))

  expect_gte(panel$y.range[2], max(drawn$ucl, drawn$y, na.rm = TRUE))

  expect_lte(panel$y.range[1], min(drawn$lcl, drawn$y, na.rm = TRUE))

})


test_that("the caption names the chart type that was asked for", {

  plot <- facet_stages(facet_arg_data,
                       split_rows = c(30L, 60L, 90L),
                       chart_type = "C'",
                       period_min = 21L)

  expect_match(plot$labels$caption, "C' Shewhart Chart", fixed = TRUE)

})


test_that("x_break reaches the drawing", {

  breaks <- function(...) {
    ggplot2::ggplot_build(faceted_plot(...))$layout$panel_params[[1]]$x$breaks
  }

  expect_false(isTRUE(all.equal(breaks(x_break = 10), breaks(x_break = 30))))

})


# what facet_stages() returns


test_that("facet_stages returns an autospc_plot that is still a ggplot", {

  plot <- faceted_plot()

  expect_s3_class(plot, "autospc_plot")

  expect_identical(class(plot), c("autospc_plot", "gg", "ggplot"))

})


test_that("it carries one analysed chart per facet, in stage order", {

  charts <- autospc_plot_charts(faceted_plot())

  expect_length(charts, 3L)

  expect_true(all(vapply(charts,
                         function(chart) inherits(chart, "autospc_chart_c"),
                         logical(1))))

  # the facets are cumulative stages, so each chart analyses more of the series
  # than the one before
  expect_identical(vapply(charts,
                          function(chart) nrow(chart$result$table),
                          integer(1)),
                   c(30L, 60L, 90L))

})


test_that("named split_rows name the charts", {

  charts <- autospc_plot_charts(
    facet_stages(facet_arg_data,
                 split_rows = c(early = 30L, mid = 60L, all = 90L),
                 chart_type = "C",
                 period_min = 21L)
  )

  expect_identical(names(charts), c("early", "mid", "all"))

})


test_that("it records the axis extents it was drawn with", {

  plot <- faceted_plot()

  expect_setequal(names(autospc_plot_derived(plot)),
                  c("start_x", "x_max", "end_x", "ylimlow", "ylimhigh"))

  # what is recorded is what the y scale was given, before ggplot expands it
  limits <- ggplot2::ggplot_build(plot)$layout$panel_scales_y[[1]]$limits

  expect_identical(limits, c(autospc_plot_derived(plot, "ylimlow"),
                             autospc_plot_derived(plot, "ylimhigh")))

  drawn <- faceted_plot(plot_chart = FALSE)

  expect_identical(autospc_plot_derived(plot, "start_x"), min(drawn$x))

  expect_identical(autospc_plot_derived(plot, "x_max"), max(drawn$x))

  expect_identical(autospc_plot_derived(plot, "end_x"), max(drawn$x))

})


test_that("plot_chart = FALSE still returns a plain data frame", {

  result <- faceted_plot(plot_chart = FALSE)

  expect_s3_class(result, "data.frame")

  expect_false(inherits(result, "autospc_plot"))

})


test_that("as.data.frame names the facets the way the frame does", {

  faceted <- as.data.frame(faceted_plot())

  drawn <- faceted_plot(plot_chart = FALSE)

  expect_true("stage" %in% colnames(faceted))

  expect_setequal(unique(faceted$stage), unique(drawn$stage))

})


# the titles reach the drawing


test_that("a faceted chart has axis labels", {

  plot <- faceted_plot()

  expect_identical(plot$labels$x, "Day")

  expect_identical(plot$labels$y, "Number")

})


test_that("an axis title the caller gave wins over the resolved one", {

  plot <- faceted_plot(override_y_title = "Attendances")

  expect_identical(plot$labels$y, "Attendances")

})


test_that("a title in the data reaches the faceted chart", {

  titled <- facet_arg_data
  titled$title <- "From the data"
  titled$subtitle <- "Also from the data"

  plot <- facet_stages(titled,
                       split_rows = c(30L, 60L, 90L),
                       chart_type = "C",
                       period_min = 21L)

  expect_identical(plot$labels$title, "From the data")

  expect_identical(plot$labels$subtitle, "Also from the data")

})


test_that("the plot object records the titles it was drawn with", {

  plot <- faceted_plot()

  expect_identical(autospc_plot_visualisation_params(plot, "override_x_title"),
                   plot$labels$x)

  expect_identical(autospc_plot_visualisation_params(plot, "override_y_title"),
                   plot$labels$y)

})


test_that("the plot object records the annotation scale factors it was drawn with", {

  # the caller passes NULL and the chart type answers; what is recorded is the
  # answer
  plot <- faceted_plot()

  expect_identical(
    autospc_plot_visualisation_params(plot, "upper_annotation_sf"),
    upper_annotation_sf_default(autospc_plot_charts(plot)[[1]])
  )

  expect_identical(
    autospc_plot_visualisation_params(plot, "lower_annotation_sf"),
    2 - autospc_plot_visualisation_params(plot, "upper_annotation_sf")
  )

})


# too few points for limits


short_facet_data <- data.frame(x = 1:40,
                               y = rep(c(50, 48, 49, 51, 52, 47, 50, 49), 5L))

facet_warnings <- function(...) {

  warnings_given <- character()

  withCallingHandlers(
    facet_stages(short_facet_data, chart_type = "C", period_min = 21L,
                 plot_chart = FALSE, ...),
    warning = function(w) {
      warnings_given <<- c(warnings_given, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  return(warnings_given)

}


test_that("one warning names the stage that is short of points", {

  # 10 points in the first stage, 40 in the second
  given <- facet_warnings(split_rows = c(10L, 40L))

  expect_length(given, 1L)

  expect_match(given, "Stage 1 has fewer than the minimum number of points")

})


test_that("one warning names every stage that is short of points", {

  given <- facet_warnings(split_rows = c(5L, 10L, 40L))

  expect_length(given, 1L)

  expect_match(given, "Stages 1, 2 have fewer than the minimum number of points")

})


test_that("the warning names the stages the way split_rows does", {

  given <- facet_warnings(split_rows = c(early = 10L, late = 40L))

  expect_match(given, "Stage early has")

})


test_that("no stage short of points gives no warning", {

  expect_length(facet_warnings(split_rows = c(30L, 40L)), 0L)

})


test_that("a faceted chart with no limits errors - CLEAN UP #35", {

  # the pinned behaviour is a bug: the plot data has no limits columns to draw
  # from, and the fix is CLEAN UP #35
  short <- data.frame(x = 1:10, y = rep(c(10L, 12L), 5L))

  expect_error(
    suppressWarnings(facet_stages(short, split_rows = c(5L, 10L),
                                  chart_type = "C", period_min = 21L))
  )

})


test_that("leaving out chart_type says so, rather than failing on a length", {

  # facet_stages() used to test dots_exprs$chart_type == "XMR", which is
  # logical(0) when no chart type was given, so the call died before
  # validate_chart_type() could say what was wrong
  message <- tryCatch(
    facet_stages(facet_arg_data, split_rows = c(30L, 60L, 90L)),
    error = conditionMessage
  )

  expect_match(message, "chart_type")

  expect_no_match(message, "argument is of length zero", fixed = TRUE)

})


test_that("a rounding warning is given once for the call, not once per facet", {

  fractional <- data.frame(x = 1:90,
                           y = rep(c(10.4, 12.6, 11.5, 13.4, 9.6, 14.4), 15L))

  warnings_given <- character()

  withCallingHandlers(
    facet_stages(fractional, split_rows = c(30L, 60L, 90L), chart_type = "C",
                 period_min = 21L, plot_chart = FALSE),
    warning = function(w) {
      warnings_given <<- c(warnings_given, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  rounding <- grepl("Rounding to the nearest whole", warnings_given, fixed = TRUE)

  expect_identical(sum(rounding), 1L)

})


test_that("the facets are analysed from the rounded counts", {

  fractional <- data.frame(x = 1:90,
                           y = rep(c(10.4, 12.6, 11.5, 13.4, 9.6, 14.4), 15L))

  result <- suppressWarnings(
    facet_stages(fractional, split_rows = c(30L, 60L, 90L), chart_type = "C",
                 period_min = 21L, plot_chart = FALSE)
  )

  expect_identical(result$y[result$stage == "3"], round(fractional$y))

})
