# The limit arithmetic the P and P' charts share.


#' The limit width a period's rows carry
#'
#' The limits of a P or P' chart sit a distance from the centre line that
#' varies as one over the square root of the denominator. The rest of that
#' distance is the same at every point of a period, so it is one value however
#' much the denominators vary, and the limits can be recalculated from it at
#' any other denominator. For a p-chart this is 3*sqrt(p_bar*(1-p_bar)), for a
#' p-prime chart this is modified by the average moving range.
#'
#' `get_p_limits()` and `get_pp_limits()` calculate it where they calculate the
#' limits themselves, and `form_calculation_limits()` carries it into the
#' limits table as the `limit_width` column, so it is read from there rather
#' than worked back out of the limits. Working it back out would give the wrong
#' answer for a row whose limits have been held at 0 or 100.
#'
#' @param rows Rows of a limits table, holding a `limit_width` column.
#'
#' @return the first value of `limit_width` in `rows` that is not NA, or NA
#'   where the column is absent or every value is NA
#' @noRd
limit_width_of <- function(rows) {
  if (!"limit_width" %in% names(rows)) {
    return(NA_real_)
  }

  present <- rows$limit_width[!is.na(rows$limit_width)]

  if (length(present) == 0L) {
    return(NA_real_)
  }

  return(as.numeric(present[1L]))
}


#' Calculate limits at given denominators
#'
#' Returns `cl` plus and minus `constant / sqrt(n)`, one pair of limits for
#' each element of `n`. The values are not held within the range a percentage
#' can take; `clamp_percentage_limits()` does that.
#'
#' @param cl The centre line the limits sit either side of.
#' @param constant The period's limit width, from `limit_width_of()`.
#' @param n The denominators to calculate the limits at.
#'
#' @return list of two numeric vectors named ucl and lcl, each as long as `n`
#' @noRd
limits_at_denominators <- function(cl,
                                   constant,
                                   n) {
  half_width <- constant / sqrt(n)

  return(list(
    ucl = cl + half_width,
    lcl = cl - half_width
  ))
}


#' Hold limits within the range a percentage can take
#'
#' Returns `limits` with any `ucl` above 100 replaced by 100 and any `lcl`
#' below 0 replaced by 0. NA values are left as they are.
#'
#' @param limits A list of two numeric vectors, named ucl and lcl.
#'
#' @return `limits`, with nothing above 100 or below 0
#' @noRd
clamp_percentage_limits <- function(limits) {
  return(list(
    ucl = pmin(limits$ucl, 100),
    lcl = pmax(limits$lcl, 0)
  ))
}


#' Extend a calculation period's limits over the display rows
#'
#' Called by the `extend_display_limits()` methods of the P and P' classes.
#' Takes the centre line and the limit width of the row at `counter - 1`, which
#' is the last row of the calculation period, and gives every row from
#' `counter` to the end of the table that centre line and limits recalculated
#' at its own denominator.
#'
#' @param limits_table The limits table being built.
#' @param counter The first display row.
#'
#' @return `limits_table`, with `cl`, `ucl`, `lcl`, `limit_width` and
#'   `period_type` set on the rows from `counter` onwards
#' @noRd
extend_display_limits_at_denominators <- function(limits_table,
                                                  counter) {
  last_calculated <- counter - 1

  constant <- limit_width_of(limits_table[last_calculated, , drop = FALSE])
  pbar <- as.numeric(limits_table[last_calculated, "cl"])

  display_rows <- counter:nrow(limits_table)

  limits_table[display_rows, "cl"] <- pbar
  limits_table[display_rows, "limit_width"] <- constant
  limits_table[display_rows, "period_type"] <- "display"

  display_limits <- limits_at_denominators(
    cl = pbar,
    constant = constant,
    n = limits_table[["n"]][display_rows]
  )
  held <- clamp_percentage_limits(display_limits)

  limits_table$ucl[display_rows] <- held$ucl
  limits_table$lcl[display_rows] <- held$lcl

  return(limits_table)
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
#' from the width of the period the row sits in.
#'
#' A row that `denominators_for_missing_rows()` returns NA for keeps the limits
#' it was given, as do all of them where no observation in the period holds a
#' limit width.
#'
#' @param limits The limits these rows have already been given, as vectors
#'   named cl, ucl and lcl, one value per row of `rows`.
#' @param period The rows of the period that hold an observation.
#' @param rows The rows that hold no observation, to be given limits.
#'
#' @return `limits`, with `ucl` and `lcl` recalculated
#' @noRd
proportion_limits_for_missing_rows <- function(limits,
                                               period,
                                               rows) {
  constant <- limit_width_of(period)

  n <- denominators_for_missing_rows(period = period, rows = rows)
  usable <- !is.na(n)

  if (is.na(constant) || !any(usable)) {
    return(limits)
  }

  at_n <- limits_at_denominators(
    cl = limits$cl[usable],
    constant = constant,
    n = n[usable]
  )
  held <- clamp_percentage_limits(at_n)

  limits$ucl[usable] <- held$ucl
  limits$lcl[usable] <- held$lcl

  return(limits)
}
