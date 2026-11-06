# The line joining the series points breaks at each row whose value is missing
# (#321). Time points absent from the data are not gaps: the line joins across
# them.


# A C chart of example_series_1 with the values at the given rows set missing
c_chart_missing <- function(missing_at, ...) {
  d <- example_series_1
  d$y[missing_at] <- NA

  return(suppressWarnings(autospc(d,
    chart_type = "C",
    max_exclusions = 3L,
    ...
  )))
}


# The pieces the series line is drawn in, in each panel.
#
# The series line is the line layer through the same values as the points. A
# piece is a stretch of consecutive points within one group with no missing
# value between them, which is what ggplot2 draws as one unbroken line. So the
# count is the same whether a break comes from the grouping or from a missing
# value left in the layer's data.
#
# Returns the number of pieces in each panel.
series_line_pieces <- function(plot) {
  built <- ggplot2::ggplot_build(plot)

  is_layer <- function(geom) {
    return(vapply(
      plot$layers,
      function(layer) inherits(layer$geom, geom),
      logical(1L)
    ))
  }

  points <- built$data[is_layer("GeomPoint")][[1]]

  is_series_line <- is_layer("GeomLine") & vapply(
    built$data,
    function(layer) {
      isTRUE(all.equal(sort(stats::na.omit(layer$y)), sort(points$y)))
    },
    logical(1L)
  )

  expect_identical(sum(is_series_line), 1L)

  line <- built$data[[which(is_series_line)]]
  line <- line[order(line$PANEL, line$group, line$x), ]

  pieces_in <- function(rows) {
    pieces <- 0L

    for (group in unique(rows$group)) {
      observed <- !is.na(rows$y[rows$group == group])
      pieces <- pieces + sum(observed & !c(FALSE, utils::head(observed, -1L)))
    }

    return(pieces)
  }

  return(unname(vapply(
    split(line, line$PANEL, drop = TRUE),
    pieces_in,
    integer(1L)
  )))
}


test_that("a missing value inside the series breaks the line in two", {
  expect_equal(series_line_pieces(c_chart_missing(40L)), 2L)
})


test_that("several missing values in a row make one break", {
  expect_equal(series_line_pieces(c_chart_missing(40:43)), 2L)
})


test_that("each separate gap makes a break of its own", {
  expect_equal(series_line_pieces(c_chart_missing(c(30L, 70:71, 100L))), 4L)
})


test_that("missing values at the start and end of the series do not break the line", {
  expect_equal(series_line_pieces(c_chart_missing(c(1:2, 124:125))), 1L)
})


test_that("the line breaks whatever na_ends_run is set to", {
  for (na_ends_run in c(TRUE, FALSE)) {
    expect_equal(
      series_line_pieces(c_chart_missing(40L, na_ends_run = na_ends_run)),
      2L
    )
  }
})


test_that("time points absent from the data do not break the line", {
  # rows 40 to 43 left out altogether, rather than given a missing value
  plot <- suppressWarnings(autospc(example_series_1[-(40:43), ],
    chart_type = "C",
    max_exclusions = 3L
  ))

  expect_equal(series_line_pieces(plot), 1L)
})


test_that("every observed point is still drawn", {
  plot <- c_chart_missing(40:43)

  points <- ggplot2::ggplot_build(plot)$data[vapply(
    plot$layers,
    function(layer) inherits(layer$geom, "GeomPoint"),
    logical(1L)
  )][[1]]

  expect_identical(nrow(points), nrow(example_series_1) - 4L)
})


test_that("the line breaks on a chart drawn without limits", {
  expect_equal(
    series_line_pieces(c_chart_missing(40L, show_limits = FALSE)),
    2L
  )
})


test_that("the line breaks in each facet that holds the gap", {
  # the stages run to rows 30, 60 and the end, so the gap at row 40 is in the
  # second and third facets and not the first
  d <- example_series_1
  d$y[40L] <- NA

  plot <- suppressWarnings(facet_stages(d,
    split_at = c(30L, 60L),
    chart_type = "C",
    max_exclusions = 3L
  ))

  expect_equal(series_line_pieces(plot), c(1L, 2L, 2L))
})


test_that("breaking the line adds nothing to the returned table", {
  analyse <- function(d) {
    return(suppressWarnings(autospc(d,
      chart_type = "C",
      max_exclusions = 3L,
      plot_chart = FALSE
    )))
  }

  d <- example_series_1
  d$y[40:43] <- NA

  expect_identical(
    colnames(analyse(d)),
    colnames(analyse(example_series_1))
  )
})
