# Add annotation data to main dataframe
add_annotation_data <- function(df,
                                chartType,
                                ylimhigh,
                                align_labels,
                                flip_labels,
                                upper_annotation_sf,
                                lower_annotation_sf,
                                annotation_arrow_curve) {
  
  label_accuracy <- switch(chartType,
                           C = 1,
                           `C'` = 1,
                           P = 0.1,
                           `P'` = 0.1,
                           XMR = 10^(ceiling(log10(ylimhigh)) - 4),
                           MR = 10^(ceiling(log10(ylimhigh)) - 4))
  
  df <- df %>% 
    dplyr::mutate(cl_label = dplyr::if_else(
      breakPoint |
        dplyr::row_number() == (1L + (chartType == "MR")),
      dplyr::if_else(rep(chartType == "P" | chartType == "P'",
                         nrow(df)),
                     scales::number(cl,
                                    accuracy = label_accuracy,
                                    suffix = "%"),
                     scales::number(cl,
                                    big.mark = ",",
                                    accuracy = label_accuracy)),
      ""),
      cl_change = sign(cl - dplyr::lag(cl)),
      chartType = chartType,
      align_labels = align_labels,
      flip_labels = flip_labels,
      upper_annotation_level = dplyr::if_else(align_labels,
                                              max(ucl, na.rm = TRUE) * upper_annotation_sf,
                                              ucl * upper_annotation_sf),
      lower_level = dplyr::if_else(align_labels,
                                   min(lcl, na.rm = TRUE) * lower_annotation_sf,
                                   lcl * lower_annotation_sf),
      lower_annotation_level = dplyr::if_else(chartType == "MR" | !flip_labels,
                                              upper_annotation_level,
                                              lower_level),
      annotation_level = dplyr::case_when(
        dplyr::row_number() == (1L + (chartType == "MR")) ~ upper_annotation_level,
        breakPoint == FALSE ~ 0,
        cl_change == 1 ~ upper_annotation_level,
        cl_change == -1 ~ lower_annotation_level
      ),
      annotation_curvature = dplyr::case_when(
        dplyr::row_number() == (1L + (chartType == "MR")) ~ annotation_arrow_curve,
        breakPoint == FALSE ~ 0,
        cl_change == 1 ~ annotation_arrow_curve,
        cl_change == -1 & flip_labels ~ -annotation_arrow_curve,
        cl_change == -1 & !flip_labels ~ annotation_arrow_curve
      )
    ) %>%
    dplyr::select(
      -chartType,
      -align_labels,
      -flip_labels,
      -upper_annotation_level,
      -lower_level,
      -lower_annotation_level)
  
  return(df)
  
}


add_annotations_to_plot <- function(p,
                                    df,
                                    annotation_size,
                                    annotation_arrows,
                                    annotation_curvature) {
  
  if(annotation_arrows) {
    
    p_annotated <- p + ggrepel::geom_text_repel(ggplot2::aes(x = x,
                                                             y = cl,
                                                             label = cl_label),
                                                position = ggpp::position_nudge_to(y = df %>%
                                                                                     dplyr::filter(!is.na(y)) %>%
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
                                                  dplyr::filter(!is.na(y)) %>%
                                                  dplyr::pull(annotation_curvature),
                                                segment.ncp = 4,
                                                segment.inflect = FALSE,
                                                segment.square = FALSE,
                                                arrow = grid::arrow(length = grid::unit(0.015, "npc")))
  } else {
    p_annotated <- p + ggrepel::geom_text_repel(ggplot2::aes(x = x,
                                                             y = cl,
                                                             label = cl_label),
                                                position = ggpp::position_nudge_to(y = df %>%
                                                                                     dplyr::filter(!is.na(y)) %>%
                                                                                     dplyr::pull(annotation_level)),
                                                color = "grey40",
                                                size = annotation_size,
                                                fontface = "bold",
                                                force             = 0,
                                                hjust             = 0,
                                                min.segment.length = Inf)
  }
  
  return(p_annotated)
}
