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


#' Add the floating medians and their labels to a plot
#'
#' One median for each segment of the series that has one, in each facet. Each
#' is drawn in the style the caller gave, with its width multiplied by
#' `line_width_sf`, and has no legend entry. `floating_median_labels` sets
#' which medians are labelled, and `labelled_floating_medians()` finds them. A
#' label sits just after the last point its median is taken over, centred on
#' the line, and gives the median's value. Where only part of the
#' horizontal axis is shown, a median is drawn only where the range shown
#' reaches its last point.
#'
#' @param table One row per point, holding `x`, `median`, `restart`, and the
#'   columns `observed_rows()` reads.
#' @param spc_plot The plot to add the median to.
#' @param chart The chart the median belongs to, which sets the label's format
#'   and, for `floating_median_labels = "gaps"`, where the gaps are.
#' @param visualisation_params The visualisation parameters, from which
#'   `floating_median_style`, `floating_median_labels`, `line_width_sf` and
#'   `annotation_size` are read.
#' @param ylimhigh The upper end of the vertical axis, which the label's format
#'   can depend on.
#' @param window The range of the horizontal axis shown, or NULL for all of it.
#'   The gap before the label is a share of its extent.
#'
#' @return The plot, with the median line and its label added.
#' @noRd
add_floating_median <- function(table,
                                spc_plot,
                                chart,
                                visualisation_params,
                                ylimhigh,
                                window = NULL) {
  style <- visualisation_params$floating_median_style

  table <- shown_floating_medians(table, window = window)

  extent_x <- table$x

  if (!is.null(window)) {
    extent_x <- window
  }

  label_gap <- floating_median_spacing(extent_x, fraction = 0.01)

  # one row for each labelled median, in each facet
  label_rows <- labelled_floating_medians(table,
    window = window,
    chart = chart,
    labels = visualisation_params$floating_median_labels
  ) %>%
    dplyr::filter(!is.na(median)) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of("stage")), median_segment) %>%
    dplyr::slice_max(x, n = 1L, with_ties = FALSE) %>%
    dplyr::ungroup()

  label_text <- vapply(
    label_rows$median,
    function(value) {
      floating_median_label(chart = chart, value = value, ylimhigh = ylimhigh)
    },
    character(1L)
  )

  # the values are injected with !! so that a column of the same name cannot
  # stand in for them
  label_rows <- label_rows %>%
    dplyr::mutate(
      x = x + !!label_gap,
      y = median,
      label = !!label_text
    ) %>%
    dplyr::select(x, y, label, dplyr::any_of("stage"))

  spc_plot <- spc_plot +
    ggplot2::geom_line(
      data = table,
      ggplot2::aes(x, median, group = median_segment),
      colour = style$colour,
      linetype = style$linetype,
      linewidth = style$linewidth * visualisation_params$line_width_sf,
      show.legend = FALSE,
      na.rm = TRUE
    ) +
    ggplot2::geom_text(
      data = label_rows,
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
#' The last point the median is taken over, plus space for the label: 12% of
#' the extent of `extent_x`.
#'
#' @param table One row per point, holding `x` and `median`, with a median in
#'   at least one row, and only on the rows of the labelled median.
#' @param extent_x The horizontal axis values whose extent the space is a share
#'   of: the data's, or the range shown.
#'
#' @return A value on the horizontal axis.
#' @noRd
floating_median_label_end <- function(table,
                                      extent_x = table$x) {
  return(max(median_x_values(table)) +
    floating_median_spacing(extent_x, fraction = 0.12))
}


#' Whether any floating median is drawn
#'
#' Where the table has one, and the range shown, if any, reaches the last point
#' it is taken over.
#'
#' @param table One row per point, holding `x`, `median` and `restart`.
#' @param window The range of the horizontal axis shown, or NULL for all of it.
#'
#' @return `TRUE` or `FALSE`.
#' @noRd
floating_median_shown <- function(table,
                                  window) {
  return(has_floating_median(shown_floating_medians(table, window = window)))
}


#' The floating medians that are drawn
#'
#' Each segment of the series has its own median, in each facet. Where only
#' part of the horizontal axis is shown, a median is kept only where the range
#' shown reaches the last point it is taken over.
#'
#' @param table One row per point, holding `x`, `median` and `restart`.
#' @param window The range of the horizontal axis shown, or NULL for all of it.
#'
#' @return `table`, with `median_segment` added, and `median` NA for every
#'   median that is not drawn.
#' @noRd
shown_floating_medians <- function(table,
                                   window) {
  table <- median_segment_column(table)

  if (is.null(window) || !has_floating_median(table)) {
    return(table)
  }

  table <- table %>%
    dplyr::group_by(dplyr::across(dplyr::any_of("stage")), median_segment) %>%
    dplyr::mutate(
      median_end = suppressWarnings(max(x[!is.na(median)])),
      median = dplyr::if_else(median_end <= !!window[[2L]], median, NA_real_)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-median_end)

  return(table)
}


#' The floating medians that are labelled
#'
#' Of the medians that are drawn, in each facet: with `labels = "last"`, the
#' median of the last segment of the series, so none where that segment has no
#' median; with `"all"`, every median; with `"gaps"`, the median of the last
#' segment and every median that `medians_before_gaps()` finds; with `"none"`,
#' none.
#'
#' @param table One row per point, holding `x`, `median`, `restart`, and, for
#'   `labels = "gaps"`, the columns `observed_rows()` reads.
#' @param window The range of the horizontal axis shown, or NULL for all of it.
#' @param chart The chart drawn, which says where the gaps are.
#' @param labels The value of `floating_median_labels`.
#'
#' @return `table`, with `median_segment` added, and `median` NA everywhere but
#'   the labelled medians.
#' @noRd
labelled_floating_medians <- function(table,
                                      window,
                                      chart,
                                      labels) {
  table <- shown_floating_medians(table, window = window)

  before_gaps <- if (labels == "gaps") {
    medians_before_gaps(table, chart = chart)
  } else {
    dplyr::slice_head(
      dplyr::select(table, dplyr::any_of("stage"), median_segment),
      n = 0L
    )
  }

  table <- table %>%
    dplyr::group_by(dplyr::across(dplyr::any_of("stage"))) %>%
    dplyr::mutate(last_segment = median_segment == max(median_segment)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      dplyr::mutate(before_gaps, before_gap = TRUE),
      by = c(intersect("stage", names(table)), "median_segment")
    ) %>%
    dplyr::mutate(
      labelled = switch(labels,
        last = last_segment,
        all = TRUE,
        gaps = last_segment | dplyr::coalesce(before_gap, FALSE),
        none = FALSE
      ),
      median = dplyr::if_else(labelled, median, NA_real_)
    ) %>%
    dplyr::select(-last_segment, -before_gap, -labelled)

  return(table)
}


#' The floating medians that are followed by a long gap
#'
#' A median is followed by a long gap where its last point is followed by at
#' least `restart_after_gap` points with no value on the location chart, and
#' then by a point with a value. `observed_rows()`, with `location = TRUE`,
#' says which points have a value.
#'
#' @param table One row per point in each facet, in order of `x`, holding `x`,
#'   `median`, `median_segment`, and the columns `observed_rows()` reads.
#' @param chart The chart drawn, read for `restart_after_gap`.
#'
#' @return A data frame of `median_segment`, and `stage` where the table has
#'   it, one row for each median followed by a long gap.
#' @noRd
medians_before_gaps <- function(table,
                                chart) {
  # for each point with a value, the number of points with no value between
  # it and the next point with a value. The points after the last point with a
  # value in a facet are not a gap, and neither are those before the first.
  gaps_after <- table %>%
    dplyr::mutate(
      has_value = observed_rows(chart, data = table, location = TRUE)
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of("stage"))) %>%
    dplyr::mutate(value_number = cumsum(has_value)) %>%
    dplyr::filter(
      value_number > 0L,
      value_number < max(value_number)
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of("stage")), value_number) %>%
    dplyr::summarise(
      x = dplyr::first(x),
      gap_length = sum(!has_value),
      .groups = "drop"
    )

  median_ends <- table %>%
    dplyr::filter(!is.na(median)) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of("stage")), median_segment) %>%
    # the table is in order of x. Unlike max(), last() doesn't warn when there
    # is no median, and so no rows: dplyr still evaluates it once then.
    dplyr::summarise(x = dplyr::last(x), .groups = "drop")

  return(median_ends %>%
    dplyr::inner_join(gaps_after,
      by = c(intersect("stage", names(table)), "x")
    ) %>%
    dplyr::filter(gap_length >= !!chart$restart_after_gap) %>%
    dplyr::select(dplyr::any_of("stage"), median_segment))
}


#' Number the segments of the series, so that each floating median is drawn on
#' its own
#'
#' A row belongs to the segment of the last restart at or before its `x`,
#' whether forced or after a gap, counting from 0 for the rows before the
#' first, within each facet.
#'
#' @param table One row per point, holding `x` and, where the table has it,
#'   `restart`.
#'
#' @return `table`, with `median_segment` added.
#' @noRd
median_segment_column <- function(table) {
  if (!"restart" %in% names(table)) {
    return(dplyr::mutate(table, median_segment = 0L))
  }

  table <- table %>%
    dplyr::group_by(dplyr::across(dplyr::any_of("stage"))) %>%
    dplyr::mutate(
      median_segment = findInterval(
        x,
        sort(x[dplyr::coalesce(restart, FALSE)])
      )
    ) %>%
    dplyr::ungroup()

  return(table)
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
