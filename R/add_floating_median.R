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
#' The line is drawn in the style the caller gave, with its width multiplied by
#' `line_width_sf`, and has no legend entry. The label sits just after the last
#' point the median is taken over, centred on the line, and gives the median's
#' value.
#'
#' @param table One row per point, holding `x` and `median`.
#' @param spc_plot The plot to add the median to.
#' @param chart The chart the median belongs to, which sets the label's format.
#' @param visualisation_params The visualisation parameters, from which
#'   `floating_median_style`, `line_width_sf` and `annotation_size` are read.
#' @param ylimhigh The upper end of the vertical axis, which the label's format
#'   can depend on.
#'
#' @return The plot, with the median line and its label added.
#' @noRd
add_floating_median <- function(table,
                                spc_plot,
                                chart,
                                visualisation_params,
                                ylimhigh) {
  style <- visualisation_params$floating_median_style

  median_rows <- table %>%
    dplyr::filter(!is.na(median))

  median_value <- dplyr::first(median_rows$median)

  label_text <- floating_median_label(
    chart = chart,
    value = median_value,
    ylimhigh = ylimhigh
  )

  label_gap <- floating_median_spacing(table$x, fraction = 0.01)

  # one row, kept with its facet where the plot is faceted. The values are
  # injected with !! so that a column of the same name cannot stand in for them
  label_row <- median_rows %>%
    dplyr::slice_max(x, n = 1L, with_ties = FALSE) %>%
    dplyr::mutate(
      x = x + !!label_gap,
      y = !!median_value,
      label = !!label_text
    ) %>%
    dplyr::select(x, y, label, dplyr::any_of("stage"))

  spc_plot <- spc_plot +
    ggplot2::geom_line(
      data = table,
      ggplot2::aes(x, median),
      colour = style$colour,
      linetype = style$linetype,
      linewidth = style$linewidth * visualisation_params$line_width_sf,
      show.legend = FALSE,
      na.rm = TRUE
    ) +
    ggplot2::geom_text(
      data = label_row,
      ggplot2::aes(x, y, label = label),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 0.5,
      lineheight = 0.9,
      size = visualisation_params$annotation_size,
      colour = style$colour,
      na.rm = TRUE
    )

  return(spc_plot)
}


#' The text of the floating median's label
#'
#' "Median" and, on a second line, the value, formatted as the chart formats
#' its centre line labels. Where those are whole numbers and the median is
#' halfway between two, as the median of an even number of counts can be, the
#' value is given to one decimal place.
#'
#' @param chart The chart the median belongs to.
#' @param value The median.
#' @param ylimhigh The upper end of the vertical axis.
#'
#' @return A string.
#' @noRd
floating_median_label <- function(chart,
                                  value,
                                  ylimhigh) {
  whole_numbers <- label_accuracy(chart = chart, ylimhigh = ylimhigh) == 1

  if (whole_numbers && value %% 1 == 0.5) {
    formatted <- scales::number(value, big.mark = ",", accuracy = 0.1)
  } else {
    formatted <- centre_line_label(
      chart = chart,
      cl = value,
      ylimhigh = ylimhigh
    )
  }

  return(paste0("Median\n", formatted))
}


#' Where the horizontal axis must reach to show the floating median's label
#'
#' The last point the median is taken over, plus space for the label.
#'
#' @param table One row per point, holding `x` and `median`, with a median in
#'   at least one row.
#'
#' @return A value on the horizontal axis.
#' @noRd
floating_median_label_end <- function(table) {
  return(max(median_x_values(table)) +
    floating_median_spacing(table$x, fraction = 0.12))
}


#' Whether the floating median is drawn
#'
#' Where the table has one, and the range shown, if any, reaches the last point
#' it is taken over.
#'
#' @param table One row per point, holding `x` and `median`.
#' @param window The range of the horizontal axis shown, or NULL for all of it.
#'
#' @return `TRUE` or `FALSE`.
#' @noRd
floating_median_shown <- function(table,
                                  window) {
  if (!has_floating_median(table)) {
    return(FALSE)
  }

  return(is.null(window) || window[[2L]] >= max(median_x_values(table)))
}


#' The horizontal axis values the floating median is taken over
#'
#' @param table One row per point, holding `x` and `median`, with a median in
#'   at least one row.
#'
#' @return The values of `x` in the rows with a median.
#' @noRd
median_x_values <- function(table) {
  return(table %>%
    dplyr::filter(!is.na(median)) %>%
    dplyr::pull(x))
}


#' A fraction of the horizontal extent of the data
#'
#' In the units of the horizontal axis: days for dates, seconds for date-times.
#'
#' @param x The horizontal axis values.
#' @param fraction The fraction wanted.
#'
#' @return A number.
#' @noRd
floating_median_spacing <- function(x,
                                    fraction) {
  extent <- as.numeric(max(x, na.rm = TRUE)) - as.numeric(min(x, na.rm = TRUE))

  return(extent * fraction)
}
