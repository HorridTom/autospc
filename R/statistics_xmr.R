# Get i limits
# Input y as a vector. returns cl, ucl and lcl as a list.
get_x_statistics <- function(y,
                             na.rm = TRUE,
                             mr_screen_max_loops = 1,
                             exclusion_points = NULL) {
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

  # Exclude exclusion points from calculations
  if (!is.null(exclusion_points) & length(exclusion_points) > 0) {
    y_excl <- y[-exclusion_points]
  } else {
    y_excl <- y
  }

  # Calculations of limits for X charts
  mr <- abs(diff(y_excl))
  mr_lims <- mr_limits(
    mr = mr,
    mr_screen_max_loops = mr_screen_max_loops
  )

  mean_x <- mean(y_excl, na.rm = TRUE)
  sd_estimate <- mr_lims$mean_mr / d2_constant()

  # Lists the results
  return(list(
    cl = rep(mean_x, length(y)),
    sd_estimate = rep(sd_estimate, length(y))
  ))
}

# Get moving ranges
moving_ranges <- function(y,
                          exclusion_points = NULL) {
  # Exclude exclusion points from calculations
  if (!is.null(exclusion_points) & length(exclusion_points) > 0) {
    y_excl <- y[-exclusion_points]
  } else {
    y_excl <- y
  }

  mr <- abs(diff(y_excl))

  mrs <- c(NA_real_, mr)

  return(mrs)
}


# Get moving range limits
get_mr_statistics <- function(mr,
                              na.rm = TRUE,
                              mr_screen_max_loops = 0,
                              exclusion_points = NULL) {
  # Exclude exclusion points from calculations
  if (!is.null(exclusion_points) & length(exclusion_points) > 0) {
    mr_excl <- mr[-exclusion_points]
  } else {
    mr_excl <- mr
  }

  mr_lims <- mr_limits(
    mr = mr_excl,
    mr_screen_max_loops = mr_screen_max_loops
  )

  cl <- mr_lims$mean_mr

  # The upper limit is D4 times the mean moving range, so the standard
  # deviation it implies is a third of the distance from the mean to it. Taken
  # from the factor in use rather than from d3 over d2, so that the published
  # D4 and the estimate agree under either setting of
  # `autospc.rounded_constants`.
  sd_estimate <- (mr_upper_limit_factor() - 1) * cl / 3

  return(list(
    cl = rep(cl, length(mr)),
    sd_estimate = rep(sd_estimate, length(mr)),
    mr = mr
  ))
}

mr_limits <- function(mr,
                      mr_screen_max_loops) {
  # Calculation of limits for mr chart
  mean_mr <- mean(mr,
    na.rm = TRUE
  )
  ucl_mr <- mr_upper_limit_factor() * mean_mr

  # Recursively removes moving ranges that are above the upper range limit and
  # recalculates the average moving range. mr_screen_max_loops sets the maximum
  # number of times this procedure is performed.
  i <- 0L

  while (any(mr > ucl_mr, na.rm = TRUE) & (i < mr_screen_max_loops)) {
    mr <- mr[mr < ucl_mr] # removes any mr values above the url
    mean_mr <- mean(mr, na.rm = TRUE)
    ucl_mr <- mr_upper_limit_factor() * mean_mr

    i <- i + 1L
  }

  return(list(
    mean_mr = mean_mr,
    ucl_mr = ucl_mr
  ))
}
