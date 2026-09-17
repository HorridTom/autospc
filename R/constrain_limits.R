# Constraining control limits to the range the plotted statistic can take, and
# the option that turns the constraining off.


#' Whether limits are constrained to the range the statistic can take
#'
#' Read from an option rather than from an argument of `autospc()` so that a
#' caller can set it once for the session. Only FALSE turns the constraining
#' off, so a mis-typed option leaves it in place.
#'
#' @return TRUE or FALSE
#' @noRd
limits_constrained <- function() {
  return(!isFALSE(getOption("autospc.constrain_limits")))
}


#' Constrain limits to the range the plotted statistic can take
#'
#' Replaces an upper limit above the top of the range with the top of it, and a
#' lower limit below the bottom with the bottom. An infinite or missing limit is
#' left alone, and so is everything else when the option says not to constrain.
#'
#' @param limits A list with `ucl` and `lcl` among its elements.
#' @param bounds The range, as `limit_bounds()` gives it.
#'
#' @return `limits`, with `ucl` and `lcl` constrained to `bounds`
#' @noRd
constrain_limits <- function(limits, bounds) {
  if (!limits_constrained()) {
    return(limits)
  }

  above <- is.finite(limits$ucl) & limits$ucl > bounds$high
  below <- is.finite(limits$lcl) & limits$lcl < bounds$low

  limits$ucl[above] <- bounds$high
  limits$lcl[below] <- bounds$low

  return(limits)
}
