# Get c chart limits
# Input y data as vector. Returns cl, ucl and lcl as named list.
get_c_limits <- function(y,
                         exclusion_points = NULL,
                         na.rm = TRUE) {
  # Errors if data is not in the right format
  if (length(y) == 0) {
    stop("The input data has zero observations.")
  }

  if (!is.numeric(y)) {
    stop("The input data is not numeric.")
  }

  if (na.rm == FALSE & any(is.na(y))) {
    stop(paste(
      "There are missing values in the input data. Set na.rm to TRUE",
      "if you wish to ignore these."
    ))
  }

  if (!is.null(exclusion_points) & length(exclusion_points) > 0) {
    # Exclude exclusion points from calculations
    y_excl <- y[-exclusion_points]
  } else {
    y_excl <- y
  }

  cl <- mean(y_excl, na.rm = TRUE)

  # the standard deviation of a Poisson count is the square root of its mean
  sd_estimate <- sqrt(cl)

  return(list(
    cl = rep(cl, length(y)),
    sd_estimate = rep(sd_estimate, length(y))
  ))
}


# Get C prime limits
# This is the same as U prime with n = 1
# Input y and n data as vectors. Returns cl, ucl and lcl as named list.
get_cp_limits <- function(y,
                          exclusion_points = NULL,
                          na.rm = TRUE,
                          mr_screen_max_loops = 1) {
  # Errors if data is not in the right format
  if (length(y) == 0) {
    stop("The input data has zero observations.")
  }

  if (!is.numeric(y)) {
    stop("The input data is not numeric.")
  }

  if (na.rm == FALSE & any(is.na(y))) {
    stop(paste(
      "There are missing values in the input data. Set na.rm to TRUE",
      "if you wish to ignore these."
    ))
  }

  if (!is.null(exclusion_points) & length(exclusion_points) > 0) {
    # Exclude exclusion points from calculations
    y_excl <- y[-exclusion_points]
  } else {
    y_excl <- y
  }

  cl <- mean(y_excl, na.rm = TRUE)

  n_excl <- 1 # Makes explicit the relationship with u-prime charts
  poisson_sd <- sqrt(cl / n_excl)
  z_i <- (y_excl - cl) / poisson_sd

  mr <- abs(diff(z_i))
  mr_lims <- mr_limits(
    mr = mr,
    mr_screen_max_loops = mr_screen_max_loops
  )

  sigma_z <- mr_lims$mean_mr / d2_constant()

  sd_estimate <- poisson_sd * sigma_z

  return(list(
    cl = rep(cl, length(y)),
    sd_estimate = rep(sd_estimate, length(y))
  ))
}
