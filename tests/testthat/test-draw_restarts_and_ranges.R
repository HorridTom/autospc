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
  is_legend <- vapply(box$grobs, gtable::is.gtable, logical(1L))

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
