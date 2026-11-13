# Drawing restarts and the ranges where re-establishment is prevented


#' Where the chart marks a restart, and which type it is
#'
#' Each row where `restart` is TRUE, kept with its facet where the plot is
#' faceted. A restart is forced where it is the first point of its facet at or
#' after a value of `force_restart_at`, which is where `forced_restart_rows()`
#' puts a forced restart in the series. Any other restart came after a gap.
#' Where only part of the horizontal axis is shown, only the restarts inside it
#' are kept.
#'
#' @param chart The chart drawn, read for `force_restart_at`.
#' @param table The table drawn, one row per point.
#' @param window The range of the horizontal axis shown, or NULL for all of it.
#'
#' @return A data frame holding `x`, `type` ("Forced restart" or "Restart
#'   after gap"), and `stage` where the table has it.
#' @noRd
restart_line_positions <- function(chart,
                                   table,
                                   window) {
  if (!"restart" %in% names(table)) {
    return(data.frame(x = table$x[0L], type = character(0)))
  }

  at <- chart$force_restart_at

  # For each restart, previous_x is the x of the row just before the restart
  # in its facet; the rows are in order of x within each facet. The restart
  # was forced if a value of force_restart_at is greater than previous_x and
  # no greater than the restart's x, because the restart is then the first
  # point at or after that value.
  lines <- table %>%
    dplyr::group_by(dplyr::across(dplyr::any_of("stage"))) %>%
    dplyr::mutate(previous_x = dplyr::lag(x)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(dplyr::coalesce(restart, FALSE)) %>%
    dplyr::distinct(x, previous_x, dplyr::across(dplyr::any_of("stage"))) %>%
    dplyr::mutate(
      forced = vapply(
        seq_along(x),
        function(i) any(at > previous_x[[i]] & at <= x[[i]]),
        logical(1L)
      ),
      type = dplyr::if_else(forced, "Forced restart", "Restart after gap")
    ) %>%
    dplyr::select(x, type, dplyr::any_of("stage"))

  if (!is.null(window)) {
    lines <- lines %>%
      dplyr::filter(x >= !!window[[1L]], x <= !!window[[2L]])
  }

  return(lines)
}


#' The areas shaded for the ranges where re-establishment is prevented
#'
#' Each range given by `prevent_establishment_in`, within the data of each
#' facet: an open end is taken as that end of the data, and a range outside the
#' data is dropped. Each area reaches half the spacing between points beyond
#' the range's ends, so that the points at its ends, which it includes, are
#' covered, and a range of a single `x` is visible; but not beyond the
#' horizontal axis, whose limits would otherwise drop the area. Where only part
#' of the horizontal axis is shown, only the areas that reach into it.
#'
#' @param chart The chart drawn, read for `prevent_establishment_in`.
#' @param table The table drawn, one row per point.
#' @param axis_extents The chart's axis extents, holding `start_x`, `end_x`
#'   and `window`, the range of the horizontal axis shown or NULL for all of
#'   it.
#'
#' @return A data frame holding `xmin` and `xmax`, and `stage` where the table
#'   has it, with no rows where nothing is shaded.
#' @noRd
prevented_range_areas <- function(chart,
                                  table,
                                  axis_extents) {
  window <- axis_extents$window
  ranges <- chart$prevent_establishment_in

  data_rows <- table

  if ("limit_extension" %in% names(table)) {
    data_rows <- data_rows %>%
      dplyr::filter(!dplyr::coalesce(limit_extension, FALSE))
  }

  if (is.null(ranges) || nrow(data_rows) == 0L) {
    return(data.frame(xmin = numeric(0), xmax = numeric(0)))
  }

  extents <- data_rows %>%
    dplyr::group_by(dplyr::across(dplyr::any_of("stage"))) %>%
    dplyr::summarise(
      first_x = min(x),
      last_x = max(x),
      half_step = half_spacing(x),
      .groups = "drop"
    )

  areas <- dplyr::bind_rows(lapply(ranges, function(bounds) {
    start <- bounds[[1L]]
    end <- bounds[[2L]]

    extents %>%
      dplyr::mutate(
        xmin = if (is.na(start)) first_x else pmax(first_x, start),
        xmax = if (is.na(end)) last_x else pmin(last_x, end)
      ) %>%
      dplyr::filter(xmin <= xmax) %>%
      dplyr::mutate(
        xmin = pmax(xmin - half_step, !!axis_extents$start_x),
        xmax = pmin(xmax + half_step, !!axis_extents$end_x)
      )
  }))

  if (!is.null(window) && nrow(areas) > 0L) {
    areas <- areas %>%
      dplyr::filter(xmax >= !!window[[1L]], xmin <= !!window[[2L]])
  }

  return(dplyr::select(areas, xmin, xmax, dplyr::any_of("stage")))
}


#' Half the usual spacing between points on the horizontal axis
#'
#' The median gap between consecutive values of `x`, halved. Zero where there
#' is only one value.
#'
#' @param x The values of `x`.
#'
#' @return A single value of the type a difference of `x` has.
#' @noRd
half_spacing <- function(x) {
  gaps <- diff(sort(unique(x)))

  if (length(gaps) == 0L) {
    return(x[[1L]] - x[[1L]])
  }

  return(stats::median(gaps) / 2)
}


#' Shade the ranges where re-establishment is prevented
#'
#' Added before anything else is drawn, so that the shading lies behind the
#' points and lines. Its legend entry comes after the chart's own legends.
#'
#' @param spc_plot The plot, before its points and lines are added.
#' @param areas The areas, as `prevented_range_areas()` gives them.
#'
#' @return The plot, with the shading and its legend entry added where there
#'   is anything to shade.
#' @noRd
add_prevented_ranges <- function(spc_plot,
                                 areas) {
  if (nrow(areas) == 0L) {
    return(spc_plot)
  }

  spc_plot <- spc_plot +
    ggplot2::geom_rect(
      data = areas,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = -Inf,
        ymax = Inf,
        fill = "Re-establishment prevented"
      ),
      alpha = 0.2,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual(
      name = NULL,
      values = c("Re-establishment prevented" = "grey80"),
      labels = c("Re-establishment prevented" = "Re-establishment\nprevented"),
      guide = ggplot2::guide_legend(order = 3L)
    )

  return(spc_plot)
}


#' Mark each restart with a vertical line
#'
#' A solid line for a forced restart and a dotted one for a restart after a
#' gap. Their legend entries come after the chart's own legends.
#'
#' @param spc_plot The plot.
#' @param lines Where the lines go, as `restart_line_positions()` gives them.
#' @param line_width_sf The scale factor for line widths.
#'
#' @return The plot, with the lines and their legend entries added where there
#'   is a restart.
#' @noRd
add_restart_lines <- function(spc_plot,
                              lines,
                              line_width_sf) {
  if (nrow(lines) == 0L) {
    return(spc_plot)
  }

  colours <- c("Forced restart" = "grey30", "Restart after gap" = "grey30")
  linetypes <- c("Forced restart" = "solid", "Restart after gap" = "13")

  spc_plot <- spc_plot +
    ggnewscale::new_scale_colour() +
    ggnewscale::new_scale("linetype") +
    ggplot2::geom_vline(
      data = lines,
      ggplot2::aes(xintercept = x, colour = type, linetype = type),
      linewidth = 0.6 * line_width_sf
    ) +
    ggplot2::scale_colour_manual(
      name = NULL,
      values = colours,
      guide = ggplot2::guide_legend(order = 4L)
    ) +
    ggplot2::scale_linetype_manual(
      name = NULL,
      values = linetypes,
      guide = ggplot2::guide_legend(order = 4L)
    )

  return(spc_plot)
}
