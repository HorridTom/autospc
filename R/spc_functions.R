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