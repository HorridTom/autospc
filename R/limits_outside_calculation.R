# Limits for the rows outside the calculation rows: display rows, rows with no
# observation, and rows beyond the data. Each is given the limits of its period,
# formed at the row itself from the period's centre line and standard deviation
# estimate.


#' Control limits at a set of rows from a period's statistics
#'
#' The standard deviation estimate at each row is formed by `sd_estimate_at()`,
#' and the limits by `limits_from_statistics()`, constrained to the range the
#' plotted statistic can take, so a row whose limits vary with the denominator
#' is given limits at its own `n`. Where `statistics` is NULL every row is
#' given NA.
#'
#' @param statistics A period's centre line and standard deviation estimate, as
#'   `period_statistics()` gives them.
#' @param rows The rows to give limits to, holding `n` for the classes whose
#'   limits vary with the denominator.
#'
#' @return list of four numeric vectors named cl, ucl, lcl and sd_estimate, and
#'   sbar where `statistics` holds it, one value per row of `rows`
#' @noRd
limits_at_rows <- function(chart,
                           statistics,
                           rows) {
  if (is.null(statistics)) {
    missing <- rep(NA_real_, nrow(rows))

    return(list(
      cl = missing,
      ucl = missing,
      lcl = missing,
      sd_estimate = missing
    ))
  }

  sd_estimate <- sd_estimate_at(
    chart = chart,
    statistics = statistics,
    rows = rows
  )

  limits <- constrain_limits(
    limits = limits_from_statistics(
      chart = chart,
      statistics = list(cl = statistics$cl, sd_estimate = sd_estimate),
      rows = rows
    ),
    bounds = limit_bounds(chart)
  )

  at_rows <- list(
    cl = rep_len(statistics$cl, nrow(rows)),
    ucl = limits$ucl,
    lcl = limits$lcl,
    sd_estimate = sd_estimate
  )

  if (!is.null(statistics$sbar)) {
    at_rows$sbar <- rep_len(statistics$sbar, nrow(rows))
  }

  return(at_rows)
}


#' A period's centre line and standard deviation estimate
#'
#' Read from the first row that holds the centre line and either the standard
#' deviation estimate or `sbar`, with `sbar` from the same row where the table
#' has that column. A period holds one centre line throughout, and either one
#' standard deviation estimate or, where the estimate varies with the subgroup
#' size, one `sbar`, so any such row serves. A subgroup of one on an Xbar chart
#' holds `sbar` but no estimate, because the estimate is formed at its size.
#'
#' @param rows Rows of a limits table.
#'
#' @return list of single values named cl and sd_estimate, and sbar where the
#'   table has it, or NULL where no row holds them
#' @noRd
period_statistics <- function(rows) {
  if (!all(c("cl", "sd_estimate") %in% names(rows))) {
    return(NULL)
  }

  holding <- !is.na(rows$cl) & !is.na(rows$sd_estimate)

  if ("sbar" %in% names(rows)) {
    holding <- holding | (!is.na(rows$cl) & !is.na(rows$sbar))
  }

  holding <- which(holding)

  if (length(holding) == 0L) {
    return(NULL)
  }

  first <- holding[1L]

  statistics <- list(
    cl = as.numeric(rows$cl[first]),
    sd_estimate = as.numeric(rows$sd_estimate[first])
  )

  if ("sbar" %in% names(rows)) {
    statistics$sbar <- as.numeric(rows$sbar[first])
  }

  return(statistics)
}


#' Whether a chart's limits vary with the denominator
#'
#' True for the classes whose limits table carries `n`.
#'
#' @return TRUE or FALSE
#' @noRd
has_denominator <- function(chart) {
  return("n" %in% limits_table_columns(chart))
}


#' The denominator to calculate a row with no observation's limits at
#'
#' Returns the denominator (`n`) at the row in question, where that is present
#' and greater than zero. Where it is not, returns the period's mean
#' denominator, as `mean_denominator()` gives it.
#'
#' @param period The rows of the period.
#' @param rows The rows that hold no observation, to be given limits.
#'
#' @return numeric vector, one value per row of `rows`
#' @noRd
denominators_for_missing_rows <- function(chart,
                                          period,
                                          rows) {
  return(dplyr::if_else(!is.na(rows$n) & rows$n > 0,
    as.numeric(rows$n),
    mean_denominator(chart, period = period)
  ))
}


#' The mean denominator of a period's observations that are not excluded
#'
#' The denominator the limits are formed at for a row that has none of its
#' own. Rows with no observation and excluded points are left out. A display
#' row's `excluded` is NA, so its observation counts.
#'
#' @param period The rows of the period.
#'
#' @return number, or NA where no row counts
#' @noRd
mean_denominator <- function(chart,
                             period) {
  counted <- period %>%
    dplyr::filter(
      observed_rows(chart, data = period),
      !excluded %in% TRUE
    )

  if (nrow(counted) == 0L) {
    return(NA_real_)
  }

  return(mean(counted$n))
}
