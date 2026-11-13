# Drawing forced restarts as vertical lines and prevented ranges as shading.

draw_series <- data.frame(
  x = example_series_1$x,
  y = round(example_series_1$y)
)

draw_plot <- function(...) {
  autospc(draw_series, chart_type = "C", max_exclusions = 0L, ...)
}

# The built data of every layer drawn with the given geom
geom_data <- function(plot, geom) {
  built <- ggplot2::ggplot_build(plot)$data

  is_geom <- vapply(
    plot$layers,
    function(layer) inherits(layer$geom, geom),
    logical(1L),
    USE.NAMES = FALSE
  )

  return(built[is_geom])
}

# The text of each legend, top to bottom, as one string per legend
legend_texts <- function(plot) {
  grob <- ggplot2::ggplotGrob(plot)
  box <- grob$grobs[[which(grob$layout$name == "guide-box-right")]]
  is_legend <- vapply(box$grobs, inherits, logical(1L), what = "gtable")

  texts <- vapply(
    box$grobs[is_legend],
    function(legend) {
      walk <- function(g) {
        if (!is.null(g$grobs)) {
          return(unlist(lapply(g$grobs, walk)))
        }
        if (!is.null(g$children)) {
          return(unlist(lapply(g$children, walk)))
        }
        if (!is.null(g$label)) {
          return(as.character(g$label))
        }

        return(character(0))
      }

      return(paste(walk(legend), collapse = " "))
    },
    character(1L)
  )

  return(texts[order(box$layout$t[is_legend])])
}


test_that("a forced restart is drawn as a vertical line at its row", {
  lines <- geom_data(draw_plot(force_restart_at = 60), "GeomVline")

  expect_length(lines, 1L)
  expect_identical(lines[[1]]$xintercept, 60)
})


test_that("a prevented range is shaded half a step beyond each end", {
  areas <- geom_data(
    draw_plot(prevent_establishment_in = c(20, 30)),
    "GeomRect"
  )

  expect_length(areas, 1L)
  expect_identical(c(areas[[1]]$xmin, areas[[1]]$xmax), c(19.5, 30.5))
})


test_that("a range of a single x is shaded", {
  areas <- geom_data(
    draw_plot(prevent_establishment_in = c(26, 26)),
    "GeomRect"
  )

  expect_identical(c(areas[[1]]$xmin, areas[[1]]$xmax), c(25.5, 26.5))
})


test_that("a range with an open end is shaded to that end of the data", {
  plot <- draw_plot(prevent_establishment_in = list(c(NA, 10), c(90, NA)))

  # the area must not reach past the axis, whose limits would drop it
  expect_no_warning(areas <- geom_data(plot, "GeomRect")[[1]])

  expect_identical(areas$xmin, c(1, 89.5))
  expect_identical(areas$xmax, c(10.5, 125))
})


test_that("the shading lies behind everything else", {
  plot <- draw_plot(prevent_establishment_in = c(20, 30))

  expect_s3_class(plot$layers[[1]]$geom, "GeomRect")
})


test_that("a chart with no restart or range draws neither", {
  plot <- draw_plot()

  expect_length(geom_data(plot, "GeomVline"), 0L)
  expect_length(geom_data(plot, "GeomRect"), 0L)
})


test_that("restarts and ranges outside the window are not drawn", {
  plot <- draw_plot(
    force_restart_at = 60,
    prevent_establishment_in = list(c(20, 30), c(90, 100)),
    display_window = c(1, 50)
  )

  expect_length(geom_data(plot, "GeomVline"), 0L)
  expect_identical(geom_data(plot, "GeomRect")[[1]]$xmin, 19.5)
})


test_that("the legend entries come after the chart's own legends", {
  texts <- legend_texts(draw_plot(
    force_restart_at = 60,
    prevent_establishment_in = c(20, 30)
  ))

  expect_length(texts, 4L)
  expect_match(texts[[1]], "Rule triggered")
  expect_match(texts[[2]], "Period Type")
  expect_match(texts[[3]], "Re-establishment\nprevented")
  expect_match(texts[[4]], "Forced restart")
})


test_that("the legends keep their order without restarts or ranges", {
  texts <- legend_texts(draw_plot())

  expect_match(texts[[1]], "Rule triggered")
  expect_match(texts[[2]], "Period Type")
})


test_that("there is no legend entry for what is not drawn", {
  texts <- legend_texts(draw_plot(
    force_restart_at = 60,
    prevent_establishment_in = c(20, 30),
    display_window = c(1, 50)
  ))

  expect_false(any(grepl("Forced restart", texts)))
  expect_true(any(grepl("Re-establishment\nprevented", texts)))
})


test_that("both panels of a pair draw the restart and the range", {
  plot <- autospc(example_series_1,
    chart_type = "XMR", max_exclusions = 0L,
    force_restart_at = 60, prevent_establishment_in = c(20, 30)
  )
  params <- autospc_plot_visualisation_params(plot)
  plot_data <- build_plot_data(autospc_plot_charts(plot), params)

  location <- create_spc_plot(list(plot_data$location), params)
  dispersion <- draw_dispersion_panel(plot_data$dispersion, params)

  for (panel in list(location, dispersion)) {
    expect_identical(geom_data(panel, "GeomVline")[[1]]$xintercept, 60)
    expect_identical(geom_data(panel, "GeomRect")[[1]]$xmin, 19.5)
  }
})


test_that("each facet draws the restarts in its own data", {
  plot <- facet_stages(draw_series,
    split_at = c(50L, 125L), chart_type = "C", max_exclusions = 0L,
    force_restart_at = 80, prevent_establishment_in = c(20, 30)
  )

  lines <- geom_data(plot, "GeomVline")[[1]]
  areas <- geom_data(plot, "GeomRect")[[1]]

  expect_identical(as.integer(lines$PANEL), 2L)
  expect_identical(as.integer(areas$PANEL), 1:2)
})


test_that("each facet shades only the ranges in its own data", {
  plot <- facet_stages(draw_series,
    split_at = c(50L, 125L), chart_type = "C", max_exclusions = 0L,
    prevent_establishment_in = c(60, 70)
  )

  areas <- geom_data(plot, "GeomRect")[[1]]

  expect_identical(as.integer(areas$PANEL), 2L)
  expect_identical(areas$xmin, 59.5)
})


test_that("a chart without limits draws the restart and the range", {
  # 30 points split at row 15, so neither segment has period_min points
  short <- data.frame(x = 1:30, y = rep(c(10L, 12L), 15L))

  plot <- suppressWarnings(autospc(short,
    chart_type = "C",
    force_restart_at = 15,
    prevent_establishment_in = c(5, 10)
  ))

  expect_identical(geom_data(plot, "GeomVline")[[1]]$xintercept, 15)
  expect_identical(geom_data(plot, "GeomRect")[[1]]$xmin, 4.5)
})


test_that("ranges of dates are shaded on a chart of dates", {
  plot <- autospc(ed_attendances_monthly,
    chart_type = "C", x = month_start, y = att_all, max_exclusions = 0L,
    prevent_establishment_in = as.Date(c("2016-01-01", "2017-12-01"))
  )

  areas <- geom_data(plot, "GeomRect")[[1]]

  expect_true(areas$xmin < as.numeric(as.Date("2016-01-01")))
  expect_true(areas$xmax > as.numeric(as.Date("2017-12-01")))
})


# A series for restarts after gaps: a shift upwards at x = 31, cut short by a
# gap of 21 points at 41 to 61, so that the series restarts at 62
gap_draw_series <- function() {
  data <- data.frame(
    x = 1:100,
    y = c(rep(c(9L, 11L), 15L), rep(c(14L, 16L), 15L), rep(c(9L, 11L), 20L))
  )
  data$y[41:61] <- NA

  return(data)
}

gap_draw_plot <- function(data = gap_draw_series(), ...) {
  autospc(data, chart_type = "C", max_exclusions = 0L, ...)
}


test_that("a restart after a gap is drawn as a dotted line", {
  lines <- geom_data(gap_draw_plot(), "GeomVline")[[1]]

  expect_identical(lines$xintercept, 62)
  expect_identical(lines$linetype, "13")
  expect_identical(lines$colour, "grey30")
})


test_that("forced restarts and restarts after gaps are drawn apart", {
  plot <- gap_draw_plot(force_restart_at = 85)
  lines <- geom_data(plot, "GeomVline")[[1]]
  lines <- lines[order(lines$xintercept), ]

  expect_identical(lines$xintercept, c(62, 85))
  expect_identical(lines$linetype, c("13", "solid"))

  texts <- legend_texts(plot)
  expect_match(texts[[length(texts)]], "Forced restart Restart after gap")
})


test_that("a restart after a gap that follows a forced restart is dotted", {
  lines <- geom_data(gap_draw_plot(force_restart_at = 20), "GeomVline")[[1]]
  lines <- lines[order(lines$xintercept), ]

  expect_identical(lines$xintercept, c(20, 62))
  expect_identical(lines$linetype, c("solid", "13"))
})


test_that("a forced restart at the last point of a gap is drawn apart", {
  # the forced restart is at 61, which has no value, and the restart after the
  # gap is at 62
  lines <- geom_data(gap_draw_plot(force_restart_at = 61), "GeomVline")[[1]]
  lines <- lines[order(lines$xintercept), ]

  expect_identical(lines$xintercept, c(61, 62))
  expect_identical(lines$linetype, c("solid", "13"))
})


test_that("a forced restart at the end of a gap is drawn as forced", {
  lines <- geom_data(gap_draw_plot(force_restart_at = 62), "GeomVline")[[1]]

  expect_identical(lines$xintercept, 62)
  expect_identical(lines$linetype, "solid")
  expect_false(any(grepl("Restart after gap", legend_texts(
    gap_draw_plot(force_restart_at = 62)
  ))))
})


test_that("a restart moved past a prevented range is drawn as after a gap", {
  # the gap ends at 62, inside the range, so the restart moves to 71
  plot <- gap_draw_plot(prevent_establishment_in = c(55, 70))
  lines <- geom_data(plot, "GeomVline")[[1]]

  expect_identical(lines$xintercept, 71)
  expect_identical(lines$linetype, "13")
})


test_that("a forced restart among the stranded points is drawn as forced", {
  plot <- gap_draw_plot(
    prevent_establishment_in = c(55, 70),
    force_restart_at = 66
  )
  lines <- geom_data(plot, "GeomVline")[[1]]

  expect_identical(lines$xintercept, 66)
  expect_identical(lines$linetype, "solid")
})


test_that("a restart after a gap outside the window is not drawn", {
  plot <- gap_draw_plot(display_window = c(1, 50))

  expect_length(geom_data(plot, "GeomVline"), 0L)
  expect_false(any(grepl("Restart after gap", legend_texts(plot))))
})


test_that("both panels of a pair draw a restart after a gap", {
  plot <- autospc(gap_draw_series(), chart_type = "XMR", max_exclusions = 0L)
  params <- autospc_plot_visualisation_params(plot)
  plot_data <- build_plot_data(autospc_plot_charts(plot), params)

  location <- create_spc_plot(list(plot_data$location), params)
  dispersion <- draw_dispersion_panel(plot_data$dispersion, params)

  for (panel in list(location, dispersion)) {
    lines <- geom_data(panel, "GeomVline")[[1]]

    expect_identical(lines$xintercept, 62)
    expect_identical(lines$linetype, "13")
  }
})


test_that("each facet draws the restarts after gaps in its own data", {
  plot <- facet_stages(gap_draw_series(),
    split_at = c(50L, 79L), chart_type = "C", max_exclusions = 0L,
    force_restart_at = 90
  )
  lines <- geom_data(plot, "GeomVline")[[1]]
  lines <- lines[order(lines$PANEL, lines$xintercept), ]

  # stage 1 ends at 50, before the gap ends, and stage 2 ends at 79, before the
  # forced restart at 90
  expect_identical(as.integer(lines$PANEL), c(2L, 3L, 3L))
  expect_identical(lines$xintercept, c(62, 62, 90))
  expect_identical(lines$linetype, c("13", "13", "solid"))
})


test_that("a forced restart in two facets is drawn as forced in each", {
  plot <- facet_stages(gap_draw_series(),
    split_at = c(50L, 79L), chart_type = "C", max_exclusions = 0L,
    force_restart_at = 62
  )
  lines <- geom_data(plot, "GeomVline")[[1]]

  expect_identical(as.integer(sort(lines$PANEL)), c(2L, 3L))
  expect_identical(lines$linetype, c("solid", "solid"))
})


test_that("a chart without limits draws a restart after a gap", {
  short <- data.frame(x = 1:30, y = rep(c(10L, 12L), 15L))
  short$y[10:13] <- NA

  plot <- suppressWarnings(autospc(short,
    chart_type = "C",
    restart_after_gap = 4L
  ))
  lines <- geom_data(plot, "GeomVline")[[1]]

  expect_identical(lines$xintercept, 14)
  expect_identical(lines$linetype, "13")
})


test_that("restarts on a chart of dates are drawn as the right type", {
  monthly <- gap_draw_series()
  monthly$x <- seq(as.Date("2010-01-01"), by = "month", length.out = 100L)

  plot <- gap_draw_plot(monthly, force_restart_at = as.Date("2016-12-15"))
  lines <- geom_data(plot, "GeomVline")[[1]]
  lines <- lines[order(lines$xintercept), ]

  expect_identical(
    as.numeric(lines$xintercept),
    as.numeric(as.Date(c("2015-02-01", "2017-01-01")))
  )
  expect_identical(lines$linetype, c("13", "solid"))
})
