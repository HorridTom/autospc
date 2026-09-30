#' U chart statistics for a period
#'
#' The centre line is the total count over the total area of opportunity
#' (Provost and Murray, The Health Care Data Guide, p. 200). A Poisson count's
#' variance is its mean, so the standard deviation estimate is the square root
#' of the centre line, per unit of opportunity; the standard error at a
#' denominator is this over the square root of that denominator.
#'
#' @param y Counts.
#' @param n Areas of opportunity.
#' @param exclusion_points Positions of the points left out of the calculation.
#'
#' @return list of `cl` and `sd_estimate`, each the length of `y`
#' @noRd
get_u_statistics <- function(y,
                             n,
                             exclusion_points = NULL) {
  values <- ratio_values_for_limits(
    y = y,
    n = n,
    exclusion_points = exclusion_points
  )

  cl <- sum(values$y, na.rm = TRUE) / sum(values$n, na.rm = TRUE)

  sd_estimate <- sqrt(cl)

  return(list(
    cl = rep(cl, length(y)),
    sd_estimate = rep(sd_estimate, length(y))
  ))
}


#' U' chart statistics for a period
#'
#' As for the U chart, with the standard deviation estimate multiplied by
#' Laney's sigma_z (Laney, 2002, observation 3): each point's rate is expressed
#' as a z-score against the U chart's standard error at its denominator, and
#' sigma_z is the standard deviation of those z-scores measured from their
#' moving ranges.
#'
#' @param y Counts.
#' @param n Areas of opportunity.
#' @param exclusion_points Positions of the points left out of the calculation.
#' @param mr_screen_max_loops The most times the moving ranges are screened.
#'
#' @return list of `cl` and `sd_estimate`, each the length of `y`
#' @noRd
get_up_statistics <- function(y,
                              n,
                              exclusion_points = NULL,
                              mr_screen_max_loops = 1) {
  values <- ratio_values_for_limits(
    y = y,
    n = n,
    exclusion_points = exclusion_points
  )

  cl <- sum(values$y, na.rm = TRUE) / sum(values$n, na.rm = TRUE)

  standard_error <- sqrt(cl / values$n)
  z <- (values$y / values$n - cl) / standard_error

  sigma_z <- laney_sigma_z(
    z = z,
    mr_screen_max_loops = mr_screen_max_loops
  )

  sd_estimate <- sqrt(cl) * sigma_z

  return(list(
    cl = rep(cl, length(y)),
    sd_estimate = rep(sd_estimate, length(y))
  ))
}
