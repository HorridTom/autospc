# Get p chart limits
# Input y and n data as vectors. Returns cl, ucl and lcl as named list.
get_p_statistics <- function(y,
                             n,
                             exclusion_points = NULL,
                             multiply = 1) {
  values <- ratio_values_for_limits(
    y = y,
    n = n,
    exclusion_points = exclusion_points
  )

  cl <- sum(values$y, na.rm = TRUE) / sum(values$n, na.rm = TRUE)

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
get_pp_statistics <- function(y,
                              n,
                              exclusion_points = NULL,
                              multiply = 1,
                              mr_screen_max_loops = 1) {
  values <- ratio_values_for_limits(
    y = y,
    n = n,
    exclusion_points = exclusion_points
  )

  cl <- sum(values$y, na.rm = TRUE) / sum(values$n, na.rm = TRUE)

  standard_error <- sqrt(cl * (1 - cl) / values$n)
  z <- (values$y / values$n - cl) / standard_error

  sigma_z <- laney_sigma_z(
    z = z,
    mr_screen_max_loops = mr_screen_max_loops
  )

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
