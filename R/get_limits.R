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


# Get p chart limits
# Input y and n data as vectors. Returns cl, ucl and lcl as named list.
get_p_limits <- function(y, 
                         n,
                         exclusion_points = NULL,
                         multiply = 1,
                         na.rm = TRUE){
  
  # Errors if data is not in the right format
  if(length(y) == 0){
    stop("The input data has zero observations.")
  }
  
  if(length(y) != length(n)){
    stop("The input y vector is not the same length as the input n vector.")
  }
  
  if(!is.numeric(y) | !is.numeric(n)){
    stop("The input data is not numeric.")
  }
  
  if(na.rm == FALSE & (any(is.na(y)) | any(is.na(y)) )){
    stop(paste("There are missing values in the input data. Set na.rm to TRUE",
               "if you wish to ignore these."))
  }
  
  if(!is.null(exclusion_points) & length(exclusion_points) > 0){
    # Exclude exclusion points from calculations
    y_excl <- y[-exclusion_points]
    n_excl <- n[-exclusion_points]
  }else{
    y_excl <- y
    n_excl <- n
  }
  
  # If there are missing y or n values then set both to NA
  n_excl[which(is.na(y_excl))] <- NA
  y_excl[which(is.na(n_excl))] <- NA
  
  cl <- sum(y_excl, na.rm = TRUE) / sum(n_excl, na.rm = TRUE)
  
  stdev <- sqrt(cl * (1 - cl) / n)
  cl <- cl * multiply
  ucl <- cl + 3 * stdev * multiply
  lcl <- cl - 3 * stdev * multiply
  
  lcl[lcl < 0 & is.finite(lcl)] <- 0
  
  list(cl = rep(cl, length(y)), ucl = ucl, lcl = lcl)
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


# Get P prime limits
# Input data with x, y and n columns. Returns cl, ucl and lcl as named list.
get_pp_limits <- function(y, 
                          n,
                          exclusion_points = NULL, 
                          multiply = 1, 
                          na.rm = TRUE,
                          mr_screen_max_loops = 1,
                          use_nbar_for_stdev = FALSE){
  
  # Errors if data is not in the right format
  if(length(y) == 0){
    stop("The input data has zero observations.")
  }
  
  if(length(y) != length(n)){
    stop("The input y vector is not the same length as the input n vector.")
  }
  
  if(!is.numeric(y) | !is.numeric(n)){
    stop("The input data is not numeric.")
  }
  
  if(na.rm == FALSE & (any(is.na(y)) | any(is.na(n)) )){
    stop(paste("There are missing values in the input data. Set na.rm to TRUE",
               "if you wish to ignore these."))
  }
  
  if(!is.null(exclusion_points) & length(exclusion_points) > 0){
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
  
  if(use_nbar_for_stdev) {
    n_excl <- mean(n_excl,
                   na.rm = TRUE)
  }
  
  stdev <- sqrt(cl * (1 - cl) / n_excl)
  z_i <- (y_new - cl) / stdev
  
  
  mr  <- abs(diff(z_i))
  mr_lims <- mr_lims_calc(mr = mr,
                          mr_screen_max_loops = mr_screen_max_loops)
  amr <- mr_lims$mean_mr
  ulmr <- mr_lims$ucl_mr
  
  sigma_z <- amr / 1.128
  
  # Recalculate stdev with excluded data
  if(use_nbar_for_stdev) {
    n <- mean(n,
              na.rm = TRUE)
  }
  stdev <- sqrt(cl * (1 - cl) / n)
  stdev <- stdev * sigma_z
  
  cl <- cl * multiply
  ucl <- cl + 3 * stdev * multiply
  lcl <- cl - 3 * stdev * multiply
  
  lcl[lcl < 0 & is.finite(lcl)] <- 0
  
  list(cl = rep(cl, length(y)), ucl = ucl, lcl = lcl)
}


# Get i limits
# Input y as a vector. returns cl, ucl and lcl as a list. 
get_i_limits <- function(y, 
                         na.rm = TRUE,
                         mr_screen_max_loops = 1,
                         exclusion_points = NULL){
  
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
  
  # Exclude exclusion points from calculations
  if(!is.null(exclusion_points) & length(exclusion_points) > 0){
    y_excl <- y[-exclusion_points]
  }else{
    y_excl <- y
  }
  
  # Calculations of limits for I charts
  mr <- abs(diff(y_excl))
  mr_lims <- mr_lims_calc(mr = mr,
                          mr_screen_max_loops = mr_screen_max_loops)
  
  mean_i <- mean(y_excl, na.rm = TRUE)
  sigma <- mr_lims$mean_mr/1.128 
  ucl_i <- mean_i + (3 * sigma)
  lcl_i <- mean_i - (3 * sigma)
  
  # Lists the results
  return(list(cl = rep(mean_i, length(y)),
              ucl = rep(ucl_i, length(y)),
              lcl = rep(lcl_i, length(y))))
}

# Get moving ranges
get_mrs <- function(y,
                    exclusion_points = NULL) {
  # Exclude exclusion points from calculations
  if(!is.null(exclusion_points) & length(exclusion_points) > 0){
    y_excl <- y[-exclusion_points]
  }else{
    y_excl <- y
  }
  
  mr <- abs(diff(y_excl))
  
  mrs <- c(NA_real_, mr)
  
  return(mrs)
}


# Get moving range limits 
get_mr_limits <- function(mr,
                          na.rm = TRUE,
                          mr_screen_max_loops = 0,
                          exclusion_points = NULL) {
  
  # Exclude exclusion points from calculations
  if(!is.null(exclusion_points) & length(exclusion_points) > 0){
    mr_excl <- mr[-exclusion_points]
  }else{
    mr_excl <- mr
  }
  
  mr_lims <- mr_lims_calc(mr = mr_excl,
                          mr_screen_max_loops = mr_screen_max_loops)
  
  cl <- mr_lims$mean_mr 
  ucl <- mr_lims$ucl_mr
  lcl <- 0
  
  return(list(cl = rep(cl, length(mr)),
              ucl = rep(ucl, length(mr)),
              lcl = rep(lcl, length(mr)),
              mr = mr
  ))
}

mr_lims_calc <- function(mr,
                         mr_screen_max_loops) {
  
  # Calculation of limits for mr chart
  mean_mr <- mean(mr,
                  na.rm = TRUE)
  ucl_mr <- 3.267 * mean_mr
  
  # Recursively removes moving ranges that are above the upper range limit and
  # recalculates the average moving range. mr_screen_max_loops sets the maximum
  # number of times this procedure is performed.
  i <- 0L
  
  while(any(mr > ucl_mr, na.rm = TRUE) & (i < mr_screen_max_loops)){
    
    mr <- mr[mr < ucl_mr] # removes any mr values above the url
    mean_mr <- mean(mr, na.rm = TRUE)
    ucl_mr <- 3.267 * mean_mr
    
    i <- i+1L
  }
  
  return(list(mean_mr = mean_mr,
              ucl_mr = ucl_mr))
}

