# Add floating median line to the plot spc_plot
add_floating_median <- function(table,
                                spc_plot,
                                floating_median_n) {
  spc_plot <- spc_plot +
    ggplot2::geom_line(
      data = table %>%
        dplyr::filter(series == "cl"),
      ggplot2::aes(x, median),
      linetype = "75551555",
      colour = "gray50",
      linewidth = 0.5,
      show.legend = TRUE,
      na.rm = TRUE
    ) +
    ggplot2::annotate(
      "text",
      x = table %>%
        dplyr::filter(series == "cl") %>%
        dplyr::filter(dplyr::row_number() ==
          nrow(table %>%
            dplyr::filter(series == "cl")) -
            floating_median_n + 1L) %>%
        dplyr::pull(x),
      y = table %>%
        dplyr::filter(series == "cl") %>%
        dplyr::filter(dplyr::row_number() ==
          nrow(table %>%
            dplyr::filter(series == "cl")) -
            floating_median_n + 1L) %>%
        dplyr::pull(median) * 0.95,
      label = "Median",
      size = 3,
      colour = "gray50",
      na.rm = TRUE
    )

  return(spc_plot)
}
