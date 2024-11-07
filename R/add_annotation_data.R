# Add annotation data to main dataframe
add_annotation_data <- function(df,
                                chartType,
                                ylimhigh,
                                upper_annotation_sf,
                                lower_annotation_sf,
                                annotation_arrow_curve) {
  
  upper_annotation_level <- max(df$ucl, na.rm = TRUE)*upper_annotation_sf
  
  lower_annotation_level <- ifelse(chartType == "MR",
                                   max(df$ucl, na.rm = TRUE)*upper_annotation_sf,
                                   min(df$lcl, na.rm = TRUE)*lower_annotation_sf)
  
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
        cl_change == -1 ~ -annotation_arrow_curve
      )
    )
  
  return(df)
  
}