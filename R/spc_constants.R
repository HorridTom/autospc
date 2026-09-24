# The SPC constants, and the option that chooses between the exact values and
# the published rounded ones.
#
# A constant written in upper case in the literature is named with its letter
# doubled - A3 is `aa3_constant()` - because an upper-case constant and the
# lower-case one of the same letter and number are different constants: d3 and
# D3, for instance.


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


#' A3 for subgroups of the given sizes
#'
#' The half-width of an Xbar chart's control limits as a multiple of sbar:
#' three over c4 times the square root of the subgroup size. Rounded to three
#' decimal places, as the published tables give it, where
#' `autospc.rounded_constants` is TRUE.
#'
#' @param n Subgroup sizes.
#'
#' @return numeric vector the length of `n`, NA where `n` is NA or less than 2
#' @noRd
aa3_constant <- function(n) {
  a3 <- rep(NA_real_, length(n))

  valid <- !is.na(n) & n >= 2
  m <- n[valid]

  a3[valid] <- 3 / (c4_constant(m) * sqrt(m))

  if (rounded_constants_enabled()) {
    return(round(a3, 3))
  }

  return(a3)
}


#' B4 for subgroups of the given sizes
#'
#' An S chart's upper control limit as a multiple of sbar: one plus three
#' standard deviations of a subgroup's sample standard deviation,
#' `sqrt(1 - c4^2)`, over c4. Rounded to three decimal places, as the published
#' tables give it, where `autospc.rounded_constants` is TRUE.
#'
#' @param n Subgroup sizes.
#'
#' @return numeric vector the length of `n`, NA where `n` is NA or less than 2
#' @noRd
bb4_constant <- function(n) {
  c4 <- c4_constant(n)

  b4 <- 1 + 3 * sqrt(1 - c4^2) / c4

  if (rounded_constants_enabled()) {
    return(round(b4, 3))
  }

  return(b4)
}
