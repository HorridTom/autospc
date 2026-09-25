# The statistics of the Xbar chart: sbar, and the standard deviation estimate
# formed from it.


#' Statistics for a period of an Xbar chart
#'
#' The centre line is the mean of the subgroup means weighted by subgroup size,
#' over the subgroups that are not excluded - a subgroup of one included. `sbar`
#' is the mean of the subgroup standard deviations weighted by subgroup size,
#' over the subgroups that are not excluded and hold a standard deviation. The
#' standard deviation estimate is formed from `sbar` at each subgroup's size.
#'
#' @param y The subgroup means.
#' @param n The subgroup sizes.
#' @param s The subgroup sample standard deviations, NA for a subgroup of one.
#' @param exclusion_points Positions in the period of the excluded subgroups.
#'
#' @return list of three vectors named cl, sd_estimate and sbar, each the length
#'   of `y`
#' @noRd
get_xbar_statistics <- function(y,
                                n,
                                s,
                                exclusion_points = NULL) {
  counted <- !seq_along(y) %in% exclusion_points

  cl <- sum(n[counted] * y[counted]) / sum(n[counted])

  sbar <- sbar_of(n = n[counted], s = s[counted])

  return(list(
    cl = rep(cl, length(y)),
    sd_estimate = xbar_sd_estimate(sbar = sbar, n = n),
    sbar = rep(sbar, length(y))
  ))
}


#' The mean of subgroup standard deviations weighted by subgroup size
#'
#' Over the subgroups that hold a standard deviation, which leaves out a
#' subgroup of one.
#'
#' @return number, or NA where no subgroup holds a standard deviation
#' @noRd
sbar_of <- function(n,
                    s) {
  with_s <- !is.na(s) & !is.na(n) & n >= 2

  if (!any(with_s)) {
    return(NA_real_)
  }

  return(sum(n[with_s] * s[with_s]) / sum(n[with_s]))
}


#' The standard deviation of a subgroup's observations, estimated from sbar
#'
#' `sbar / c4`, written as `aa3_constant() * sqrt(n) / 3` so that three
#' standard errors, the estimate over `sqrt(n)`, are A3 times `sbar` - the
#' published A3 where `autospc.rounded_constants` is TRUE.
#'
#' @param sbar The period's sbar.
#' @param n Subgroup sizes.
#'
#' @return numeric vector the length of `n`, NA where `n` is below 2
#' @noRd
xbar_sd_estimate <- function(sbar,
                             n) {
  return(aa3_constant(n) * sqrt(n) * sbar / 3)
}
