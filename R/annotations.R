# Add annotation data to main dataframe. With a window, only the periods that
# overlap it are labelled, a period starting before it at its first row, and
# align_labels takes the limits in it.
add_annotation_data <- function(table,
                                chart,
                                ylimhigh,
                                align_labels,
                                flip_labels,
                                upper_annotation_sf,
                                lower_annotation_sf,
                                annotation_arrow_curve,
                                window = NULL) {
  in_window <- rep(TRUE, nrow(table))

  if (!is.null(window)) {
    in_window <- !is.na(table$x) &
      table$x >= window[[1L]] &
      table$x <= window[[2L]]
  }

  # the first label goes on the first row in the window, from the chart's
  # earliest label row on, that has a centre line; a facet wholly outside the
  # window, or with no centre line, has none
  first_row <- match(
    TRUE,
    in_window &
      !is.na(table$cl) &
      seq_len(nrow(table)) >= first_label_row(chart, cl = table$cl)
  )

  if (is.na(first_row)) {
    first_row <- 0L
  }

  labels <- centre_line_label(
    chart = chart,
    cl = table$cl,
    ylimhigh = ylimhigh
  )

  table <- table %>%
    dplyr::mutate(
      in_window = !!in_window,
      labelled_break = break_point & in_window,
      cl_label = dplyr::if_else(
        labelled_break |
          dplyr::row_number() == first_row,
        labels,
        ""
      ),
      align_labels = align_labels,
      flip_labels = flip_labels,
      upper_annotation_level = dplyr::if_else(
        align_labels,
        label_position(
          max(c(-Inf, ucl[in_window]), na.rm = TRUE),
          scale_factor = upper_annotation_sf
        ),
        label_position(ucl, scale_factor = upper_annotation_sf)
      ),
      lower_level = dplyr::if_else(
        align_labels,
        label_position(
          min(c(Inf, lcl[in_window]), na.rm = TRUE),
          scale_factor = lower_annotation_sf
        ),
        label_position(lcl, scale_factor = lower_annotation_sf)
      ),
      lower_annotation_level = dplyr::if_else(
        labels_stay_above(chart) | !flip_labels,
        upper_annotation_level,
        lower_level
      ),
      annotation_level = dplyr::case_when(
        dplyr::row_number() == first_row ~
          upper_annotation_level,
        labelled_break == FALSE ~ 0,
        cl_change == 1 ~ upper_annotation_level,
        cl_change == 0 ~ upper_annotation_level,
        cl_change == -1 ~ lower_annotation_level
      ),
      annotation_curvature = dplyr::case_when(
        dplyr::row_number() == first_row ~
          annotation_arrow_curve,
        labelled_break == FALSE ~ 0,
        cl_change == 1 ~ annotation_arrow_curve,
        cl_change == -1 & flip_labels ~ -annotation_arrow_curve,
        cl_change == -1 & !flip_labels ~ annotation_arrow_curve
      )
    ) %>%
    dplyr::select(
      -in_window,
      -labelled_break,
      -align_labels,
      -flip_labels,
      -upper_annotation_level,
      -lower_level,
      -lower_annotation_level
    )

  return(table)
}


add_annotations_to_plot <- function(spc_plot,
                                    table,
                                    basic_annotations,
                                    annotation_size,
                                    annotation_arrows,
                                    annotation_arrow_curve,
                                    window = NULL) {
  use_basic_annotations <- basic_annotations

  if (!basic_annotations &
    !(rlang::is_installed("ggrepel") & rlang::is_installed("ggpp"))) {
    warning(
      paste(
        "Packages ggrepel and ggpp are required for basic_annotations",
        "= FALSE. Using basic_annotations = TRUE. To use",
        "basic_annotations = FALSE, please ensure both packages are installed."
      )
    )
    use_basic_annotations <- TRUE
  }

  if (!use_basic_annotations) {
    annotated_plot <- add_annotations_to_plot_pp(
      spc_plot = spc_plot,
      table = table,
      annotation_size = annotation_size,
      annotation_arrows = annotation_arrows,
      annotation_arrow_curve = annotation_arrow_curve
    )
  } else {
    annotated_plot <- add_annotations_to_plot_basic(
      spc_plot = spc_plot,
      table = table,
      annotation_size = annotation_size,
      annotation_arrows = annotation_arrows,
      annotation_arrow_curve = annotation_arrow_curve,
      window = window
    )
  }

  return(annotated_plot)
}


add_annotations_to_plot_pp <- function(spc_plot,
                                       table,
                                       annotation_size,
                                       annotation_arrows,
                                       annotation_arrow_curve) {
  if (annotation_arrows) {
    annotated_plot <- spc_plot + ggrepel::geom_text_repel(
      data = . %>% dplyr::filter(
        plotted_line %in% c("cl"),
        !is.na(annotation_level)
      ),
      ggplot2::aes(
        x = x,
        y = value,
        label = cl_label
      ),
      position = ggpp::position_nudge_to(
        y = table %>%
          dplyr::filter(
            plotted_line %in% c("cl"),
            !is.na(value),
            !is.na(annotation_level)
          ) %>%
          dplyr::pull(annotation_level)
      ),
      color = "grey40",
      size = annotation_size,
      fontface = "bold",
      segment.color = "grey40",
      segment.linetype = 1L,
      force = 0,
      hjust = 0,
      segment.size = 0.75,
      segment.curvature = table %>%
        dplyr::filter(
          plotted_line %in% c("cl"),
          !is.na(value),
          !is.na(annotation_level)
        ) %>%
        dplyr::pull(annotation_curvature),
      segment.ncp = 4,
      segment.inflect = FALSE,
      segment.square = FALSE,
      arrow = grid::arrow(length = grid::unit(0.015, "npc")),
      na.rm = TRUE,
      max.overlaps = Inf
    )
  } else {
    annotated_plot <- spc_plot + ggrepel::geom_text_repel(
      data = . %>% dplyr::filter(
        plotted_line %in% c("cl"),
        !is.na(annotation_level)
      ),
      ggplot2::aes(
        x = x,
        y = value,
        label = cl_label
      ),
      position = ggpp::position_nudge_to(
        y = table %>%
          dplyr::filter(
            plotted_line %in% c("cl"),
            !is.na(value),
            !is.na(annotation_level)
          ) %>%
          dplyr::pull(annotation_level)
      ),
      color = "grey40",
      size = annotation_size,
      fontface = "bold",
      force = 0,
      hjust = 0,
      min.segment.length = Inf,
      na.rm = TRUE,
      max.overlaps = Inf
    )
  }

  return(annotated_plot)
}


add_annotations_to_plot_basic <- function(spc_plot,
                                          table,
                                          annotation_size,
                                          annotation_arrows,
                                          annotation_arrow_curve,
                                          window = NULL) {
  x_range <- max(table$x, na.rm = TRUE) - min(table$x, na.rm = TRUE)

  if (!is.null(window)) {
    x_range <- window[[2L]] - window[[1L]]
  }

  x_nudge <- x_range / 25

  annotated_plot <- spc_plot +
    ggplot2::geom_text(
      data = . %>% dplyr::filter(
        plotted_line %in% c("cl"),
        !is.na(annotation_level)
      ),
      mapping = ggplot2::aes(
        x = x,
        y = annotation_level,
        label = cl_label
      ),
      nudge_x = x_nudge,
      na.rm = TRUE,
      color = "grey40",
      size = annotation_size,
      fontface = "bold"
    )

  return(annotated_plot)
}


#' The height of a centre line label placed beyond a control limit
#'
#' The label is placed `abs(scale_factor - 1)` times the size of the limit
#' outside the limit. So a positive limit is multiplied by `scale_factor`, and a
#' negative limit is multiplied by `2 - scale_factor`.
#'
#' @param limit The control limits, upper or lower.
#' @param scale_factor `upper_annotation_sf` for an upper limit,
#'   `lower_annotation_sf` for a lower one.
#'
#' @return numeric vector, one element per limit
#' @noRd
label_position <- function(limit,
                           scale_factor) {
  return(dplyr::if_else(
    limit < 0,
    limit * (2 - scale_factor),
    limit * scale_factor
  ))
}
