
#get c chart limits
#Input data with x, y and n columns. Returns cl, ucl and lcl as named list.
get_c_limits <- function(data){
  
  cl <- mean(data$y, na.rm = TRUE)
  stdev <- sqrt(cl)
  
  cl <- cl
  ucl <- cl + 3 * stdev
  lcl <- cl - 3 * stdev
  
  lcl[lcl < 0 & is.finite(lcl)] <- 0
  
  list(cl = rep(cl, nrow(data)), ucl = rep(ucl, nrow(data)), lcl = rep(lcl, nrow(data)))
}

#get p chart limits
#Input data with x, y and n columns. Returns cl, ucl and lcl as named list.
get_p_limits <- function(data){
  
  cl <- sum(data$y, na.rm = TRUE) / sum(data$n, na.rm = TRUE)
  
  cl <- cl
  stdev <- sqrt(cl * (1 - cl) / data$n)
  ucl <- cl + 3 * stdev
  lcl <- cl - 3 * stdev

  lcl[lcl < 0 & is.finite(lcl)] <- 0

  list(cl = rep(cl, nrow(data)), ucl = ucl, lcl = lcl)
}

#get C prime limits
#this is the same as U prime with n = 1
#Input data with x, y columns. Returns cl, ucl and lcl as named list.
get_cp_limits <- function(data){

  cl <- mean(data$y, na.rm = TRUE)
  
  n <- 1
  cl <- cl
  stdev <- sqrt(cl / n)
  z_i <- (data$y - cl) / stdev

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
get_pp_limits <- function(data, multiply = 1){
  
  cl <- sum(data$y, na.rm = TRUE) / sum(data$n, na.rm = TRUE) 
  
  y_new <- data$y / data$n
  stdev <- sqrt(cl * (1 - cl) / data$n)
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