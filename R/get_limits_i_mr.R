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

