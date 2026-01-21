# Add floating median line to the plot p
add_floating_median <- function(df,
                                p,
                                floating_median_n) {
  
  p <- p +
    ggplot2::geom_line(data = df %>%
                         dplyr::filter(series == "cl"), 
                       ggplot2::aes(x, median),
                       linetype = "75551555",
                       colour = "gray50",
                       linewidth = 0.5,
                       show.legend = TRUE,
                       na.rm = TRUE) +
    ggplot2::annotate(
      "text",
      x = df %>%
        dplyr::filter(series == "cl") %>%
        dplyr::filter(dplyr::row_number() ==
                        nrow(df %>%
                               dplyr::filter(series == "cl")) -
                        floating_median_n + 1L) %>%
        dplyr::pull(x),
      y = df %>%
        dplyr::filter(series == "cl") %>%
        dplyr::filter(dplyr::row_number() ==
                        nrow(df %>%
                               dplyr::filter(series == "cl")) -
                        floating_median_n + 1L) %>%
        dplyr::pull(median)*0.95,
      label = "Median",
      size = 3,
      colour = "gray50",
      na.rm = TRUE)
  
  return(p)
  
}