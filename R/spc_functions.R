#get centre line
get_cl <- function(y, 
                   cl_method = "mean"){
  
  if(cl_method == "mean"){
    cl  <- mean(y, na.rm = TRUE)
  }else if(cl_method == "median"){
    cl  <- stats::median(y, na.rm = TRUE)
  }else{
    print("Please enter either 'mean' or 'median' in the 'method' argument.")
  }
  
  cl
}

#get c chart limits
get_c_limits <- function(data){
  
  cl <- get_cl(data$y, "mean")
  stdev <- sqrt(cl)
  
  data <- data %>%
    dplyr::mutate(cl = cl) %>%
    dplyr::mutate(ucl = cl + 3 * stdev) %>%
    dplyr::mutate(lcl = cl - 3 * stdev)
  
  data$lcl[data$lcl < 0 & is.finite(data$lcl)] <- 0
  
  data
}

#get p chart limits
# need multiply = 100, 
get_p_limits <- function(data){
  
  cl <- sum(data$y, na.rm = TRUE) / sum(data$n, na.rm = TRUE)

  data <- data %>%
    dplyr::mutate(cl = cl) %>%
    dplyr::mutate(stdev = sqrt(cl * (1 - cl) / n)) %>%
    dplyr::mutate(ucl = cl + 3 * stdev) %>%
    dplyr::mutate(lcl = cl - 3 * stdev)
  
  data$lcl[data$lcl < 0 & is.finite(data$lcl)] <- 0

  data
}

#get C prime limits
#this is the same as U prime with n = 1
get_cp_limits <- function(data){

  cl <- get_cl(data$y, "mean")

  data <- data %>%
    dplyr::mutate(n = 1) %>%
    dplyr::mutate(cl = cl) %>%
    dplyr::mutate(stdev = sqrt(cl / n)) %>%
    dplyr::mutate(z_i = (y - cl) / stdev) 
  
  mr  <- abs(diff(data$z_i))
  amr <- mean(mr, na.rm = TRUE)
  
  # Upper limit for moving ranges
  ulmr <- 3.267 * amr
  
  # Remove moving ranges greater than ulmr and recalculate amr, Nelson 1982
  mr  <- mr[mr < ulmr]
  amr <- mean(mr, na.rm = TRUE)
  
  sigma_z <- amr / 1.128
  
  data <- data %>%
    dplyr::mutate(stdev = stdev * sigma_z) %>%
    dplyr::mutate(ucl = cl + 3 * stdev) %>%
    dplyr::mutate(lcl = cl - 3 * stdev)
  
  data$lcl[data$lcl < 0 & is.finite(data$lcl)] <- 0

  data
}


#get P prime limits
get_pp_limits <- function(data, multiply = NULL){
  
  cl <- sum(data$y, na.rm = TRUE) / sum(data$n, na.rm = TRUE) 
  
  data <- data %>%
    dplyr::mutate(cl = cl) %>%
    dplyr::mutate(y_new = y / n) %>%
    dplyr::mutate(stdev = sqrt(cl * (1 - cl) / n)) %>%
    dplyr::mutate(z_i = (y_new - cl) / stdev) 
  
  mr  <- abs(diff(data$z_i))
  amr <- mean(mr, na.rm = TRUE)

  # Upper limit for moving ranges
  ulmr <- 3.267 * amr

  # Remove moving ranges greater than ulmr and recalculate amr, Nelson 1982
  mr  <- mr[mr < ulmr]
  amr <- mean(mr, na.rm = TRUE)

  sigma_z <- amr / 1.128
  
  data <- data %>%
    dplyr::mutate(stdev = stdev * sigma_z) %>%
    dplyr::mutate(ucl = cl + 3 * stdev) %>%
    dplyr::mutate(lcl = cl - 3 * stdev)


  data$lcl[data$lcl < 0 & is.finite(data$lcl)] <- 0
  
  if(!is.null(multiply)){
    data <- data %>%
      dplyr::mutate(cl = cl * multiply,
                    ucl = ucl * multiply,
                    lcl = lcl * multiply)
  }

  data
}