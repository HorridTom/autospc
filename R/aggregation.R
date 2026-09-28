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

  chart$data <- sum_over_subgroups(chart$data,
    columns = c("y", "n"),
    aggregation_na_rm = chart$aggregation_na_rm
  )

  return(chart)
}


#' Aggregate subgroup data into one mean, size and standard deviation per x
#'
#' Shared by the Xbar and S methods. Each row is either one observation, where
#' the data has no `n` column, or a summary of a subgroup as its mean `y`, size
#' `n` and sample standard deviation `s`. The rows that share an `x` are
#' combined by `combine_subgroup_rows()`.
#'
#' `aggregation_na_rm` decides what a row with no value does to its subgroup: by
#' default it leaves the subgroup with no mean, and with `aggregation_na_rm =
#' TRUE` the row is left out. A subgroup with no rows left keeps its `x`, with
#' no value.
#'
#' @return autospc_chart object of the same class as chart, whose data holds one
#'   row per `x`, with columns `x`, `y`, `n` and `s`
#' @noRd
aggregate_xbars_statistics <- function(chart) {
  data <- chart$data

  if (!"n" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(n = 1L, s = NA_real_)
  }

  if (!chart$aggregation_na_rm) {
    chart$data <- combine_subgroup_rows(data)

    return(chart)
  }

  complete <- data %>%
    dplyr::filter(!is.na(y), !is.na(n), n < 2 | !is.na(s))

  chart$data <- data %>%
    dplyr::distinct(x) %>%
    dplyr::left_join(combine_subgroup_rows(complete), by = "x")

  return(chart)
}


#' Combine the rows that share an x into one subgroup
#'
#' The size is the sum of the sizes and the mean is the mean of the rows' means
#' weighted by size. The standard deviation is pooled from two sums: the spread
#' within each row, `(n - 1) * s^2`, and the spread of each row's mean about the
#' combined mean, `n * (y - mean)^2`. Together they are the sum of squared
#' deviations of every observation from the combined mean, so the result is
#' exactly that of the observations taken together. A row of one observation
#' has no spread within it, so it adds nothing to the first sum, and its
#' deviation from the combined mean is counted in the second. A subgroup of
#' fewer than two observations has no standard deviation.
#'
#' @param data Rows holding `x`, `y`, `n` and `s`.
#'
#' @return A data frame of one row per `x`, with columns `x`, `y`, `n` and `s`.
#' @noRd
combine_subgroup_rows <- function(data) {
  combined <- data %>%
    dplyr::group_by(x) %>%
    dplyr::summarise(
      subgroup_n = sum(n),
      subgroup_mean = sum(n * y) / subgroup_n,
      spread = sum(dplyr::if_else(n > 1, (n - 1) * s^2, 0)) +
        sum(n * (y - subgroup_mean)^2),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      x,
      y = dplyr::if_else(is.finite(subgroup_mean), subgroup_mean, NA_real_),
      n = subgroup_n,
      s = dplyr::if_else(
        !is.na(subgroup_n) & subgroup_n >= 2,
        sqrt(spread / (subgroup_n - 1)),
        NA_real_
      ),
      .keep = "none"
    )

  return(combined)
}
