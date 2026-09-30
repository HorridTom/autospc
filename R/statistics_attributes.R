# Steps shared by the statistics of the attribute charts: C', P, P', U and U'.


#' Prepare the counts and denominators a period's limits are calculated from
#'
#' Shared by the P, P', U and U' statistics. Checks that the counts and
#' denominators are numeric vectors of the same, non-zero length, leaves out
#' the excluded points, and makes a row missing in both where either is
#' missing, so that the two sums are taken over the same rows.
#'
#' @param y Counts.
#' @param n Denominators.
#' @param exclusion_points Positions of the points left out of the calculation.
#'
#' @return list with `y` and `n`, numeric vectors of equal length
#' @noRd
ratio_values_for_limits <- function(y,
                                    n,
                                    exclusion_points) {
  if (length(y) == 0) {
    stop("The input data has zero observations.")
  }

  if (length(y) != length(n)) {
    stop("The input y vector is not the same length as the input n vector.")
  }

  if (!is.numeric(y) | !is.numeric(n)) {
    stop("The input data is not numeric.")
  }

  if (!is.null(exclusion_points) & length(exclusion_points) > 0) {
    y <- y[-exclusion_points]
    n <- n[-exclusion_points]
  }

  n[which(is.na(y))] <- NA
  y[which(is.na(n))] <- NA

  return(list(
    y = y,
    n = n
  ))
}


#' Calculate Laney's sigma_z: the standard deviation of the z-scores
#'
#' The mean moving range of the z-scores over d2, with the moving ranges
#' screened as an MR chart's are. Shared by the C', P' and U' statistics, which
#' multiply the standard deviation their distribution assumes by it (Laney,
#' 2002).
#'
#' @param z Each point's distance from the centre line in standard errors.
#' @param mr_screen_max_loops The most times the moving ranges are screened.
#'
#' @return number
#' @noRd
laney_sigma_z <- function(z,
                          mr_screen_max_loops) {
  mr_lims <- mr_limits(
    mr = abs(diff(z)),
    mr_screen_max_loops = mr_screen_max_loops
  )

  return(mr_lims$mean_mr / d2_constant())
}
