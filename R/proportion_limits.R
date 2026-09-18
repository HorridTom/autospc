# The limit arithmetic the P and P' charts share.


#' The standard deviation estimate a period's rows carry
#'
#' An estimate of the standard deviation of a single observation, which is the
#' same at every point of a period however much the denominators vary. The
#' standard error at a denominator is this over the square root of that
#' denominator, and the limits sit three standard errors either side of the
#' centre line, so the limits can be recalculated at any other denominator. For
#' a p-chart this is sqrt(p_bar*(1-p_bar)), for a p-prime chart this is modified
#' by the average moving range.
#'
#' `get_p_limits()` and `get_pp_limits()` calculate it where they calculate the
#' limits themselves, and `form_calculation_limits()` carries it into the
#' limits table as the `sd_estimate` column, so it is read from there rather
#' than worked back out of the limits. Working it back out would give the wrong
#' answer for a row whose limits have been constrained to 0 or 100.
#'
#' @param rows Rows of a limits table, holding an `sd_estimate` column.
#'
#' @return the first value of `sd_estimate` in `rows` that is not NA, or NA
#'   where the column is absent or every value is NA
#' @noRd
sd_estimate_of <- function(rows) {
  if (!"sd_estimate" %in% names(rows)) {
    return(NA_real_)
  }

  present <- rows$sd_estimate[!is.na(rows$sd_estimate)]

  if (length(present) == 0L) {
    return(NA_real_)
  }

  return(as.numeric(present[1L]))
}


#' Extend a calculation period's limits over the display rows
#'
#' Called by the `extend_display_limits()` methods of the P and P' classes.
#' Takes the centre line and the standard deviation estimate of the row at
#' `counter - 1`, which is the last row of the calculation period, and gives
#' every row from
#' `counter` to the end of the table that centre line and limits recalculated
#' at its own denominator.
#'
#' @param limits_table The limits table being built.
#' @param counter The first display row.
#' @param bounds The range a percentage can take, as `limit_bounds()` gives it.
#'
#' @return `limits_table`, with `cl`, `ucl`, `lcl`, `sd_estimate` and
#'   `period_type` set on the rows from `counter` onwards
#' @noRd
extend_display_limits_at_denominators <- function(chart,
                                                  limits_table,
                                                  counter) {
  last_calculated <- counter - 1

  sd_estimate <- sd_estimate_of(limits_table[last_calculated, , drop = FALSE])
  pbar <- as.numeric(limits_table[last_calculated, "cl"])

  display_rows <- counter:nrow(limits_table)

  limits_table[display_rows, "cl"] <- pbar
  limits_table[display_rows, "sd_estimate"] <- sd_estimate
  limits_table[display_rows, "period_type"] <- "display"

  constrained <- constrain_limits(
    limits = limits_from_statistics(
      chart = chart,
      statistics = list(cl = pbar, sd_estimate = sd_estimate),
      rows = limits_table[display_rows, , drop = FALSE]
    ),
    bounds = limit_bounds(chart)
  )

  limits_table$ucl[display_rows] <- constrained$ucl
  limits_table$lcl[display_rows] <- constrained$lcl

  return(limits_table)
}


#' A period's rows, with every denominator replaced by the period's mean
#'
#' An extension row sits beyond the end of the data and so has no denominator
#' of its own. The limits of the whole extension are placed at the mean of the
#' period they are carried from.
#'
#' @param period The rows of the calculation period.
#'
#' @return `period`, with `n` replaced by its mean
#' @noRd
at_mean_denominator <- function(period) {
  return(period %>%
    dplyr::mutate(n = mean(n, na.rm = TRUE)))
}


#' The denominator to calculate a row with no observation's limits at
#'
#' Returns the denominator (`n`) at the row in question, where that is present
#' and greater than zero. Where it is not, returns the mean of the
#' denominators of the period's observations, which is the denominator the
#' limits of a period are extended by elsewhere. Where the period has no
#' denominators either, returns NA.
#'
#' @param period The rows of the period that hold an observation.
#' @param rows The rows that hold no observation, to be given limits.
#'
#' @return numeric vector, one value per row of `rows`
#' @noRd
denominators_for_missing_rows <- function(period,
                                          rows) {
  n <- as.numeric(rows$n)
  n[!is.na(n) & n <= 0] <- NA_real_

  mean_n <- mean(period$n, na.rm = TRUE)

  if (is.finite(mean_n) && mean_n > 0) {
    n[is.na(n)] <- mean_n
  }

  return(n)
}


#' Limits for the rows of a P or P' chart that hold no observation
#'
#' Called by the `limits_for_missing_rows()` methods of the P and P' classes,
#' with the limits the default method has already given those rows. Replaces
#' `ucl` and `lcl` with the limits calculated at each row's own denominator,
#' from the standard deviation estimate of the period the row sits in.
#'
#' A row that `denominators_for_missing_rows()` returns NA for keeps the limits
#' it was given, as do all of them where no observation in the period holds a
#' standard deviation estimate.
#'
#' @param limits The limits these rows have already been given, as vectors
#'   named cl, ucl and lcl, one value per row of `rows`.
#' @param period The rows of the period that hold an observation.
#' @param rows The rows that hold no observation, to be given limits.
#' @param bounds The range a percentage can take, as `limit_bounds()` gives it.
#'
#' @return `limits`, with `ucl` and `lcl` recalculated
#' @noRd
proportion_limits_for_missing_rows <- function(chart,
                                               limits,
                                               period,
                                               rows) {
  sd_estimate <- sd_estimate_of(period)

  n <- denominators_for_missing_rows(period = period, rows = rows)
  usable <- !is.na(n)

  if (is.na(sd_estimate) || !any(usable)) {
    return(limits)
  }

  at_n <- limits_from_statistics(
    chart = chart,
    statistics = list(cl = limits$cl[usable], sd_estimate = sd_estimate),
    rows = data.frame(n = n[usable])
  )
  constrained <- constrain_limits(limits = at_n, bounds = limit_bounds(chart))

  limits$ucl[usable] <- constrained$ucl
  limits$lcl[usable] <- constrained$lcl

  return(limits)
}
