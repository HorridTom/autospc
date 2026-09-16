#' Whether a floating median was drawn over any row
#'
#' The `median` column is always returned and holds no value where no median
#' was drawn, so its presence does not say whether there is a line to draw.
#'
#' @param table The analysis table.
#'
#' @return TRUE where at least one row holds a median, FALSE otherwise.
#' @noRd
has_floating_median <- function(table) {
  return("median" %in% colnames(table) && any(!is.na(table$median)))
}


#' Add the floating median line and its label to a plot
#'
#' The label sits at the first point of the median window, which is
#' `floating_median_n` rows from the end.
#'
#' @param table One row per point, holding `x` and `median`.
#' @param spc_plot The plot to add the median to.
#' @param floating_median_n The number of points the median is taken over.
#'
#' @return The plot, with the median line and its label added.
#' @noRd
add_floating_median <- function(table,
                                spc_plot,
                                floating_median_n) {
  label_row <- nrow(table) - floating_median_n + 1L

  spc_plot <- spc_plot +
    ggplot2::geom_line(
      data = table,
      ggplot2::aes(x, median),
      linetype = "75551555",
      colour = "gray50",
      linewidth = 0.5,
      show.legend = TRUE,
      na.rm = TRUE
    ) +
    ggplot2::annotate(
      "text",
      x = table$x[label_row],
      y = table$median[label_row] * 0.95,
      label = "Median",
      size = 3,
      colour = "gray50",
      na.rm = TRUE
    )

  return(spc_plot)
}
