# Shared aggregation internals for the aggregate_data() methods

#' Aggregate ratio-chart data over subgroups
#'
#' Shared by the P/P' methods, and by u/u' when those exist: all four plot a
#' ratio, so all four sum a numerator and a denominator over x. Named for ratios
#' rather than proportions because a rate's denominator is an area of
#' opportunity rather than a count of trials, so rates are not always
#' proportions.
#'
#' `allow_individual_observations` controls whether the y-only input form is
#' accepted, in which each row is one binary observation and the denominator is
#' one. Proportion charts accept it; rate charts have no such form.
#'
#' @return autospc_chart object of the same class as chart
#' @noRd
aggregate_ratios <- function(chart,
                             allow_individual_observations) {
  any_multiple_x <- chart$data %>%
    dplyr::group_by(x) %>%
    dplyr::summarise(num_rows = dplyr::n()) %>%
    dplyr::mutate(multiple_rows = num_rows > 1L) %>%
    dplyr::pull(multiple_rows) %>%
    any()

  # Check if data fully pre-aggregated, return with the same column signature
  # as the aggregated route if so
  if (("n" %in% colnames(chart$data)) &&
    is.numeric(chart$data$y) &&
    !any_multiple_x) {
    chart$data <- chart$data %>%
      dplyr::select(x, y, n)

    return(chart)
  }

  # Set up n for aggregation if data provided as individual binary observations
  if (allow_individual_observations &&
    !("n" %in% colnames(chart$data)) &&
    is.logical(chart$data$y)) {
    chart$data <- chart$data %>%
      dplyr::mutate(n = 1L)
  }

  chart$data <- chart$data %>%
    dplyr::group_by(x) %>%
    dplyr::summarise(
      y = sum(y),
      n = sum(n)
    )

  return(chart)
}
