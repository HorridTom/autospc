#get c chart limits
#Input data with x, y and n columns. Returns cl, ucl and lcl as named list.
get_c_limits <- function(data, exclusion_points = NULL){
  
  #send error messages if data is not in the right format
  if(length(data) == 0){
    stop("The input data has zero observations.")
  }
  
  if(!exists(data$x) | !exists(data$y)){
    stop("The data does not contain the necessary columns, x and y.")
  }
  
  if(!is.null(exclusion_points)){
    #exclude exclusion points from calculations
    data_excl <- data[-exclusion_points,]
  }else{
    data_excl <- data
  }
  
  cl <- mean(data_excl$y, na.rm = TRUE)
  stdev <- sqrt(cl)
  
  cl <- cl
  ucl <- cl + 3 * stdev
  lcl <- cl - 3 * stdev
  
  lcl[lcl < 0 & is.finite(lcl)] <- 0
  
  list(cl = rep(cl, nrow(data)), ucl = rep(ucl, nrow(data)), lcl = rep(lcl, nrow(data)))
}

#get p chart limits
#Input data with x, y and n columns. Returns cl, ucl and lcl as named list.
get_p_limits <- function(data, exclusion_points = NULL){
  
  if(!is.null(exclusion_points)){
    #exclude exclusion points from calculations
    data_excl <- data[-exclusion_points,]
  }else{
    data_excl <- data
  }
  
  cl <- sum(data_excl$y, na.rm = TRUE) / sum(data_excl$n, na.rm = TRUE)
  
  cl <- cl
  stdev <- sqrt(cl * (1 - cl) / data_excl$n)
  ucl <- cl + 3 * stdev
  lcl <- cl - 3 * stdev

  lcl[lcl < 0 & is.finite(lcl)] <- 0

  list(cl = rep(cl, nrow(data)), ucl = ucl, lcl = lcl)
}

#get C prime limits
#this is the same as U prime with n = 1
#Input data with x, y columns. Returns cl, ucl and lcl as named list.
get_cp_limits <- function(data, exclusion_points = NULL){
  
  if(!is.null(exclusion_points)){
    #exclude exclusion points from calculations
    data_excl <- data[-exclusion_points,]
  }else{
    data_excl <- data
  }

  cl <- mean(data_excl$y, na.rm = TRUE)
  
  n <- 1
  cl <- cl
  stdev <- sqrt(cl / n)
  z_i <- (data_excl$y - cl) / stdev

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

  list(cl = rep(cl, nrow(data)), ucl = rep(ucl, nrow(data)), lcl = rep(lcl, nrow(data)))
}


#get P prime limits
#Input data with x, y and n columns. Returns cl, ucl and lcl as named list.
get_pp_limits <- function(data, exclusion_points = NULL, multiply = 1){
  
  if(!is.null(exclusion_points)){
    #exclude exclusion points from calculations
    data_excl <- data[-exclusion_points,]
  }else{
    data_excl <- data
  }
  
  cl <- sum(data_excl$y, na.rm = TRUE) / sum(data_excl$n, na.rm = TRUE) 
  
  y_new <- data_excl$y / data_excl$n
  stdev <- sqrt(cl * (1 - cl) / data_excl$n)
  z_i <- (y_new - cl) / stdev

  mr  <- abs(diff(z_i))
  amr <- mean(mr, na.rm = TRUE)

  # Upper limit for moving ranges
  ulmr <- 3.267 * amr

  # Remove moving ranges greater than ulmr and recalculate amr, Nelson 1982
  mr  <- mr[mr < ulmr]
  amr <- mean(mr, na.rm = TRUE)

  sigma_z <- amr / 1.128
  
  stdev <- stdev * sigma_z
  
  cl <- cl * multiply
  ucl <- cl + 3 * stdev * multiply
  lcl <- cl - 3 * stdev * multiply

  lcl[lcl < 0 & is.finite(lcl)] <- 0

  list(cl = rep(cl, nrow(data)), ucl = ucl, lcl = lcl)
}