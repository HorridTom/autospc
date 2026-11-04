# Draw a plot to a null device.
#
# A bare print() with no device open makes R open the default one, which is
# pdf(), leaving an Rplots.pdf in tests/testthat. Tests that have to draw -
# because what they check only happens at draw time - use this instead.
drawn <- function(plot) {
  pdf(NULL)
  on.exit(dev.off())

  print(plot)
}


# Every piece of text in a combined XmR plot.
#
# cowplot::plot_grid() puts the two panels into one ggplot layer as a gtable,
# so they can be read without drawing, but only by walking it.
panel_texts <- function(plot) {
  grob <- plot$layers[[1]]$geom_params$grob

  if (is.null(grob)) {
    stop("Not a combined plot - there is only one panel.", call. = FALSE)
  }

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

  return(walk(grob))
}


# The floating median as drawn on a single chart: its line layer, the line's
# built data, and the built data of its label.
#
# The line is the layer whose y is mapped to `median`. The label is the text
# whose label starts "Median ", from whichever layer draws it.
median_drawing <- function(plot) {
  built <- ggplot2::ggplot_build(plot)$data

  is_median_line <- vapply(
    plot$layers,
    function(layer) {
      identical(rlang::as_label(layer$mapping$y), "median")
    },
    logical(1L),
    USE.NAMES = FALSE
  )

  label <- do.call(rbind, lapply(built, function(layer) {
    if (is.null(layer$label)) {
      return(NULL)
    }

    return(layer %>%
      dplyr::mutate(label = as.character(label)) %>%
      dplyr::filter(!is.na(label), startsWith(label, "Median ")) %>%
      dplyr::select(dplyr::any_of(
        c("x", "y", "label", "hjust", "colour", "size")
      )) %>%
      as.data.frame())
  }))

  return(list(
    layer = plot$layers[is_median_line],
    line = built[is_median_line],
    label = label
  ))
}
