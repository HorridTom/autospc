# The SPC constants, and the option that chooses between the exact values and
# the published rounded ones.


#' Whether to use the rounded constants
#'
#' Read from an option rather than from an argument of `autospc()` so that a
#' caller can set it once for the session. Only TRUE selects the rounded
#' values, so a mis-typed option leaves the exact ones in use.
#'
#' @return TRUE or FALSE
#' @noRd
rounded_constants_enabled <- function() {
  return(isTRUE(getOption("autospc.rounded_constants")))
}


#' d2 for a subgroup of two
#'
#' The mean range of two observations from a normal distribution, in standard
#' deviations, so an estimate of the standard deviation is the mean moving
#' range over it. The range of two observations is half-normal, which gives the
#' closed form.
#'
#' @return number
#' @noRd
d2_constant <- function() {
  if (rounded_constants_enabled()) {
    return(1.128)
  }

  return(2 / sqrt(pi))
}


#' The upper limit of a moving range chart, as a multiple of the mean
#'
#' D4 for a subgroup of two: the mean moving range plus three standard
#' deviations of the moving range, over the mean moving range. d3 is the
#' standard deviation of the range of two observations.
#'
#' The rounded value is the published D4 rather than one worked out from the
#' rounded d2 and d3, which would give 3.2673.
#'
#' @return number
#' @noRd
mr_upper_limit_factor <- function() {
  if (rounded_constants_enabled()) {
    return(3.267)
  }

  d3 <- sqrt(2 * (1 - 2 / pi))

  return(1 + 3 * d3 / d2_constant())
}


#' c4 for subgroups of the given sizes
#'
#' The mean of the sample standard deviation of n observations from a normal
#' distribution, in standard deviations, so an unbiased estimate of the standard
#' deviation is a subgroup's sample standard deviation over it. Always the exact
#' value, whatever `autospc.rounded_constants` is set to.
#'
#' Computed through `lgamma()` rather than `gamma()`, which overflows for
#' subgroups of more than about 340.
#'
#' @param n Subgroup sizes.
#'
#' @return numeric vector the length of `n`, NA where `n` is NA or less than 2
#' @noRd
c4_constant <- function(n) {
  c4 <- rep(NA_real_, length(n))

  valid <- !is.na(n) & n >= 2
  m <- n[valid]

  c4[valid] <- sqrt(2 / (m - 1)) * exp(lgamma(m / 2) - lgamma((m - 1) / 2))

  return(c4)
}
