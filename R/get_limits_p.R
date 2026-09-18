# Get p chart limits
# Input y and n data as vectors. Returns cl, ucl and lcl as named list.
get_p_limits <- function(y,
                         n,
                         exclusion_points = NULL,
                         multiply = 1) {
  # Errors if data is not in the right format
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
    # Exclude exclusion points from calculations
    y_excl <- y[-exclusion_points]
    n_excl <- n[-exclusion_points]
  } else {
    y_excl <- y
    n_excl <- n
  }

  # If there are missing y or n values then set both to NA
  n_excl[which(is.na(y_excl))] <- NA
  y_excl[which(is.na(n_excl))] <- NA

  cl <- sum(y_excl, na.rm = TRUE) / sum(n_excl, na.rm = TRUE)

  # an estimate of the standard deviation of a single observation, on the same
  # scale as the centre line. The standard error at a denominator is this over
  # the square root of that denominator
  sd_estimate <- sqrt(cl * (1 - cl)) * multiply

  cl <- cl * multiply

  return(list(
    cl = rep(cl, length(y)),
    sd_estimate = rep(sd_estimate, length(y))
  ))
}


# Get P prime limits
# Input data with x, y and n columns. Returns cl, ucl and lcl as named list.
get_pp_limits <- function(y,
                          n,
                          exclusion_points = NULL,
                          multiply = 1,
                          mr_screen_max_loops = 1) {
  # Errors if data is not in the right format
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
    # Exclude exclusion points from calculations
    y_excl <- y[-exclusion_points]
    n_excl <- n[-exclusion_points]
  } else {
    y_excl <- y
    n_excl <- n
  }

  # If there are missing y or n values then set both to NA
  n_excl[which(is.na(y_excl))] <- NA
  y_excl[which(is.na(n_excl))] <- NA

  cl <- sum(y_excl, na.rm = TRUE) / sum(n_excl, na.rm = TRUE)

  y_new <- y_excl / n_excl

  standard_error <- sqrt(cl * (1 - cl) / n_excl)
  z_i <- (y_new - cl) / standard_error


  mr <- abs(diff(z_i))
  mr_lims <- mr_limits(
    mr = mr,
    mr_screen_max_loops = mr_screen_max_loops
  )
  amr <- mr_lims$mean_mr
  ulmr <- mr_lims$ucl_mr

  sigma_z <- amr / d2_constant()

  # an estimate of the standard deviation of a single observation, on the same
  # scale as the centre line, including Laney's correction. The standard error
  # at a denominator is this over the square root of that denominator
  sd_estimate <- sqrt(cl * (1 - cl)) * sigma_z * multiply

  cl <- cl * multiply

  return(list(
    cl = rep(cl, length(y)),
    sd_estimate = rep(sd_estimate, length(y))
  ))
}
