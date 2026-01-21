# Add annotation data to main dataframe
add_annotation_data <- function(df,
                                chart_type,
                                ylimhigh,
                                align_labels,
                                flip_labels,
                                upper_annotation_sf,
                                lower_annotation_sf,
                                annotation_arrow_curve) {
  
  label_accuracy <- switch(chart_type,
                           C = 1,
                           `C'` = 1,
                           P = 0.1,
                           `P'` = 0.1,
                           XMR = 10^(ceiling(log10(ylimhigh)) - 4),
                           MR = 10^(ceiling(log10(ylimhigh)) - 4))
  
  df <- df %>% 
    dplyr::mutate(cl_label = dplyr::if_else(
      breakPoint |
        dplyr::row_number() == (1L + (chart_type == "MR")),
      dplyr::if_else(rep(chart_type == "P" | chart_type == "P'",
                         nrow(df)),
                     scales::number(cl,
                                    accuracy = label_accuracy,
                                    suffix = "%"),
                     scales::number(cl,
                                    big.mark = ",",
                                    accuracy = label_accuracy)),
      ""),
      cl_change = sign(cl - dplyr::lag(cl)),
      chart_type = chart_type,
      align_labels = align_labels,
      flip_labels = flip_labels,
      upper_annotation_level = dplyr::if_else(
        align_labels,
        max(ucl,
            na.rm = TRUE) * upper_annotation_sf,
        ucl * upper_annotation_sf),
      lower_level = dplyr::if_else(
        align_labels,
        min(lcl, na.rm = TRUE) * lower_annotation_sf,
        lcl * lower_annotation_sf),
      lower_annotation_level = dplyr::if_else(
        chart_type == "MR" | !flip_labels,
        upper_annotation_level,
        lower_level),
      annotation_level = dplyr::case_when(
        dplyr::row_number() == (1L + (chart_type == "MR")) ~
          upper_annotation_level,
        breakPoint == FALSE ~ 0,
        cl_change == 1 ~ upper_annotation_level,
        cl_change == 0 ~ upper_annotation_level,
        cl_change == -1 ~ lower_annotation_level
      ),
      annotation_curvature = dplyr::case_when(
        dplyr::row_number() == (1L + (chart_type == "MR")) ~
          annotation_arrow_curve,
        breakPoint == FALSE ~ 0,
        cl_change == 1 ~ annotation_arrow_curve,
        cl_change == -1 & flip_labels ~ -annotation_arrow_curve,
        cl_change == -1 & !flip_labels ~ annotation_arrow_curve
      )
    ) %>%
    dplyr::select(
      -chart_type,
      -align_labels,
      -flip_labels,
      -upper_annotation_level,
      -lower_level,
      -lower_annotation_level)
  
  return(df)
  
}


add_annotations_to_plot <- function(p,
                                    df,
                                    basic_annotations,
                                    annotation_size,
                                    annotation_arrows,
                                    annotation_curvature) {
  
  use_basic_annotations <- basic_annotations
  
  if(!basic_annotations &
     !(rlang::is_installed("ggrepel") & rlang::is_installed("ggpp"))) {
    warning(
      paste(
        "Packages ggrepel and ggpp are required for basic_annotations",
        "= FALSE. Using basic_annotations = TRUE. To use",
        "basic_annotations = FALSE, please ensure both packages are installed.")
    )
    use_basic_annotations <- TRUE
  }
  
  if(!use_basic_annotations) {
    p_annotated <- add_annotations_to_plot_pp(
      p = p,
      df = df,
      annotation_size = annotation_size,
      annotation_arrows = annotation_arrows,
      annotation_curvature = annotation_curvature
    )
  } else {
    p_annotated <- add_annotations_to_plot_basic(
      p = p,
      df = df,
      annotation_size = annotation_size,
      annotation_arrows = annotation_arrows,
      annotation_curvature = annotation_curvature
    )
  }
  
  return(p_annotated)
  
}


add_annotations_to_plot_pp <- function(p,
                                       df,
                                       annotation_size,
                                       annotation_arrows,
                                       annotation_curvature) {
  
  if(annotation_arrows) {
    
    p_annotated <- p + ggrepel::geom_text_repel(
      data = . %>% dplyr::filter(series %in% c("cl"),
                                 !is.na(annotation_level)),
      ggplot2::aes(x = x,
                   y = value,
                   label = cl_label),
      position = ggpp::position_nudge_to(
        y = df %>%
          dplyr::filter(series %in% c("cl"),
                        !is.na(value),
                        !is.na(annotation_level)) %>%
          dplyr::pull(annotation_level)),
      color = "grey40",
      size = annotation_size,
      fontface = "bold",
      segment.color = "grey40",
      segment.linetype = 1L,
      force             = 0,
      hjust             = 0,
      segment.size      = 0.75,
      segment.curvature = df %>%
        dplyr::filter(series %in% c("cl"),
                      !is.na(value),
                      !is.na(annotation_level)) %>%
        dplyr::pull(annotation_curvature),
      segment.ncp = 4,
      segment.inflect = FALSE,
      segment.square = FALSE,
      arrow = grid::arrow(length = grid::unit(0.015, "npc")),
      na.rm = TRUE,
      max.overlaps = Inf)
  } else {
    p_annotated <- p + ggrepel::geom_text_repel(
      data = . %>% dplyr::filter(series %in% c("cl"),
                                 !is.na(annotation_level)),
      ggplot2::aes(x = x,
                   y = value,
                   label = cl_label),
      position = ggpp::position_nudge_to(
        y = df %>%
          dplyr::filter(series %in% c("cl"),
                        !is.na(value),
                        !is.na(annotation_level)) %>%
          dplyr::pull(annotation_level)),
      color = "grey40",
      size = annotation_size,
      fontface = "bold",
      force             = 0,
      hjust             = 0,
      min.segment.length = Inf,
      na.rm = TRUE,
      max.overlaps = Inf)
  }
  
  return(p_annotated)
  
}


add_annotations_to_plot_basic <- function(p,
                                          df,
                                          annotation_size,
                                          annotation_arrows,
                                          annotation_curvature) {
  
  x_range <- max(df$x, na.rm = TRUE) - min(df$x, na.rm = TRUE)
  x_nudge <- x_range/25
  
  p_annotated <- p +
    ggplot2::geom_text(
      data = . %>% dplyr::filter(series %in% c("cl"),
                                 !is.na(annotation_level)),
      mapping = ggplot2::aes(x = x,
                             y = annotation_level,
                             label = cl_label),
      nudge_x = x_nudge,
      na.rm = TRUE,
      color = "grey40",
      size = annotation_size,
      fontface = "bold")
  
  return(p_annotated)
  
}

