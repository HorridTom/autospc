#get c chart limits
#Input y data as vector. Returns cl, ucl and lcl as named list.
get_c_limits <- function(y, 
                         exclusion_points = NULL,
                         na.rm = TRUE){
  
  #send error messages if data is not in the right format
  if(length(y) == 0){
    stop("The input data has zero observations.")
  }
  
  if(!is.numeric(y)){
    stop("The input data is not numeric.")
  }
  
  if(na.rm == FALSE & any(is.na(y))){
    stop("There are missing values in the input data. Set na.rm to TRUE if you wish to ignore these.")
  }

  if(!is.null(exclusion_points)){
    #exclude exclusion points from calculations
    y_excl <- y[-exclusion_points]
  }else{
    y_excl <- y
  }
  
  cl <- mean(y_excl, na.rm = TRUE)
  stdev <- sqrt(cl)
  
  cl <- cl
  ucl <- cl + 3 * stdev
  lcl <- cl - 3 * stdev
  
  lcl[lcl < 0 & is.finite(lcl)] <- 0
  
  list(cl = rep(cl, length(y)), ucl = rep(ucl, length(y)), lcl = rep(lcl, length(y)))
}

#get p chart limits
#Input y and n data as vectors. Returns cl, ucl and lcl as named list.
get_p_limits <- function(y, 
                         n,
                         exclusion_points = NULL,
                         multiply = 1,
                         na.rm = TRUE){
  
  #send error messages if data is not in the right format
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
    stop("There are missing values in the input data. Set na.rm to TRUE if you wish to ignore these.")
  }
  
  if(!is.null(exclusion_points)){
    #exclude exclusion points from calculations
    y_excl <- y[-exclusion_points]
    n_excl <- n[-exclusion_points]
  }else{
    y_excl <- y
    n_excl <- n
  }
  
  #if there are missing y or n values then set both to NA
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

#get C prime limits
#this is the same as U prime with n = 1
#Input y and n data as vectors. Returns cl, ucl and lcl as named list.
get_cp_limits <- function(y,
                          exclusion_points = NULL,
                          na.rm = TRUE){
  
  #send error messages if data is not in the right format
  if(length(y) == 0){
    stop("The input data has zero observations.")
  }
  
  if(!is.numeric(y)){
    stop("The input data is not numeric.")
  }
  
  if(na.rm == FALSE & any(is.na(y))){
    stop("There are missing values in the input data. Set na.rm to TRUE if you wish to ignore these.")
  }

  if(!is.null(exclusion_points)){
    #exclude exclusion points from calculations
    y_excl <- y[-exclusion_points]
  }else{
    y_excl <- y
  }

  cl <- mean(y_excl, na.rm = TRUE)
  
  n_excl <- 1 #######
  cl <- cl
  stdev <- sqrt(cl / n_excl)
  z_i <- (y_excl - cl) / stdev

  mr  <- abs(diff(z_i))
  amr <- mean(mr, na.rm = TRUE)
  
  # Upper limit for moving ranges
  ulmr <- 3.267 * amr
  
  # Remove moving ranges greater than ulmr and recalculate amr, Nelson 1982
  mr  <- mr[mr < ulmr]
  amr <- mean(mr, na.rm = TRUE)
  
  sigma_z <- amr / 1.128
  
  stdev <- stdev * sigma_z
  ucl <- cl + 3 * stdev
  lcl <- cl - 3 * stdev
  
  lcl[lcl < 0 & is.finite(lcl)] <- 0

  list(cl = rep(cl, length(y)), ucl = rep(ucl, length(y)), lcl = rep(lcl, length(y)))
}


#get P prime limits
#Input data with x, y and n columns. Returns cl, ucl and lcl as named list.
get_pp_limits <- function(y, 
                          n,
                          exclusion_points = NULL, 
                          multiply = 1, 
                          na.rm = TRUE){
  
  #send error messages if data is not in the right format
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
    stop("There are missing values in the input data. Set na.rm to TRUE if you wish to ignore these.")
  }
  
  if(!is.null(exclusion_points)){
    #exclude exclusion points from calculations
    y_excl <- y[-exclusion_points]
    n_excl <- n[-exclusion_points]
  }else{
    y_excl <- y
    n_excl <- n
  }
  
  #if there are missing y or n values then set both to NA
  n_excl[which(is.na(y_excl))] <- NA
  y_excl[which(is.na(n_excl))] <- NA
  
  cl <- sum(y_excl, na.rm = TRUE) / sum(n_excl, na.rm = TRUE) 
  
  y_new <- y_excl / n_excl
  stdev <- sqrt(cl * (1 - cl) / n_excl)
  z_i <- (y_new - cl) / stdev

  mr  <- abs(diff(z_i))
  amr <- mean(mr, na.rm = TRUE)

  # Upper limit for moving ranges
  ulmr <- 3.267 * amr

  # Remove moving ranges greater than ulmr and recalculate amr, Nelson 1982
  mr  <- mr[mr < ulmr]
  amr <- mean(mr, na.rm = TRUE)

  sigma_z <- amr / 1.128
  
  #recalc stdev with excluded data
  stdev <- sqrt(cl * (1 - cl) / n)
  stdev <- stdev * sigma_z
  
  cl <- cl * multiply
  ucl <- cl + 3 * stdev * multiply
  lcl <- cl - 3 * stdev * multiply

  lcl[lcl < 0 & is.finite(lcl)] <- 0

  list(cl = rep(cl, length(y)), ucl = ucl, lcl = lcl)
}

#get i limits
#Input y as a vector. returns cl, ucl and lcl as a list. 

get_i_limits <- function(y, 
                         na.rm = TRUE,
                         mr_screen_max_loops = 1,
                         exclusion_points = NULL){
  
  #sends error messages if data is not in the correct format
  if(length(y) == 0){
    stop("The input data has zero observations.")
  }
  
  if(!is.numeric(y)){
    stop("The input data is not numeric.")
  }
  
  if(na.rm == FALSE & any(is.na(y))){
    stop("There are missing values in the input data. Set na.rm to TRUE if you wish to ignore these.")
  }
  
  #exclude exclusion points from calculations
  if(!is.null(exclusion_points)){
    y_excl <- y[-exclusion_points]
  }else{
    y_excl <- y
  }
  
  # calculations of limits for i charts
  mean_i <- mean(y_excl, na.rm = TRUE)
  mr <- abs(diff(y_excl))
  mean_mr <- mean(mr, na.rm = TRUE)
  ucl_mr <- 3.267 * mean_mr
  sigma <- mean_mr/1.128 
  ucl_i <- mean_i + (3 * sigma)
  lcl_i <- mean_i - (3 * sigma)
  
  #removes moving ranges that are above the ucl_mr and recalculates the mean_mr, ucl_i and lcl_i  
  i <- 0L
  
  #calculate limits in cases where there are mRs above the upper range limit
  while(any(mr > ucl_mr) & (i < mr_screen_max_loops)){
    mr <- mr[mr < ucl_mr] #removes any mR values above the ucl
    mean_mr <- mean(mr, na.rm = TRUE)
    ucl_mr <- 3.267 * mean_mr
    sigma <- mean_mr/1.128 
    ucl_i <- mean_i + (3 * sigma)
    lcl_i <- mean_i - (3 * sigma)
    i <- i+1L
  }
  
  #lists the results
  return(list(cl = rep(mean_i, length(y)), ucl = rep(ucl_i, length(y)), lcl = rep(lcl_i, length(y))))
}

# Get moving ranges
get_mrs <- function(y,
                    exclusion_points = NULL) {
  #exclude exclusion points from calculations
  if(!is.null(exclusion_points)){
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
                          exclusion_points = NULL) {
  
  #exclude exclusion points from calculations
  if(!is.null(exclusion_points)){
    mr_excl <- mr[-exclusion_points]
  }else{
    mr_excl <- mr
  }
  
  mean_mr <- mean(mr_excl, na.rm = TRUE)
  ucl_mr <- 3.267 * mean_mr 
  
  cl <- mean_mr 
  ucl <- ucl_mr
  lcl <- 0
  
  return(list(cl = rep(cl, length(mr)),
              ucl = rep(ucl, length(mr)),
              lcl = rep(lcl, length(mr)),
              mr = mr
  ))
}
