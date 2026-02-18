# Get c chart limits
# Input y data as vector. Returns cl, ucl and lcl as named list.
get_c_limits <- function(y, 
                         exclusion_points = NULL,
                         na.rm = TRUE){
  
  # Errors if data is not in the right format
  if(length(y) == 0){
    stop("The input data has zero observations.")
  }
  
  if(!is.numeric(y)){
    stop("The input data is not numeric.")
  }
  
  if(na.rm == FALSE & any(is.na(y))){
    stop(paste("There are missing values in the input data. Set na.rm to TRUE",
               "if you wish to ignore these."))
  }
  
  if(!is.null(exclusion_points) & length(exclusion_points) > 0){
    # Exclude exclusion points from calculations
    y_excl <- y[-exclusion_points]
  } else {
    y_excl <- y
  }
  
  cl <- mean(y_excl, na.rm = TRUE)
  stdev <- sqrt(cl)
  
  cl <- cl
  ucl <- cl + 3 * stdev
  lcl <- cl - 3 * stdev
  
  lcl[lcl < 0 & is.finite(lcl)] <- 0
  
  list(cl = rep(cl, length(y)),
       ucl = rep(ucl, length(y)),
       lcl = rep(lcl, length(y)))
}


# Get C prime limits
# This is the same as U prime with n = 1
# Input y and n data as vectors. Returns cl, ucl and lcl as named list.
get_cp_limits <- function(y,
                          exclusion_points = NULL,
                          na.rm = TRUE,
                          mr_screen_max_loops = 1){
  
  # Errors if data is not in the right format
  if(length(y) == 0){
    stop("The input data has zero observations.")
  }
  
  if(!is.numeric(y)){
    stop("The input data is not numeric.")
  }
  
  if(na.rm == FALSE & any(is.na(y))){
    stop(paste("There are missing values in the input data. Set na.rm to TRUE",
               "if you wish to ignore these."))
  }
  
  if(!is.null(exclusion_points) & length(exclusion_points) > 0){
    # Exclude exclusion points from calculations
    y_excl <- y[-exclusion_points]
  } else {
    y_excl <- y
  }
  
  cl <- mean(y_excl, na.rm = TRUE)
  
  n_excl <- 1 # Makes explicit the relationship with u-prime charts
  cl <- cl
  stdev <- sqrt(cl / n_excl)
  z_i <- (y_excl - cl) / stdev
  
  mr  <- abs(diff(z_i))
  mr_lims <- mr_lims_calc(mr = mr,
                          mr_screen_max_loops = mr_screen_max_loops)
  
  sigma_z <- mr_lims$mean_mr / 1.128
  
  stdev <- stdev * sigma_z
  ucl <- cl + 3 * stdev
  lcl <- cl - 3 * stdev
  
  lcl[lcl < 0 & is.finite(lcl)] <- 0
  
  list(cl = rep(cl, length(y)),
       ucl = rep(ucl, length(y)),
       lcl = rep(lcl, length(y)))
}