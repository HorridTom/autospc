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
  
  data
}

#get p chart limits
# need multiply = 100, 
get_p_limits <- function(data){
  
  cl <- sum(data$y, na.rm = TRUE) / sum(data$n, na.rm = TRUE)
  stdev <- sqrt(cl * (1 - cl) / n)

  data <- data %>%
    dplyr::mutate(cl = cl) %>%
    dplyr::mutate(stdev = sqrt(cl * (1 - cl) / n)) %>%
    dplyr::mutate(ucl = cl + 3 * stdev) %>%
    dplyr::mutate(lcl = cl - 3 * stdev)

  data
}

# #get C prime limits 
# #this is the same as U prime with n = 1
# get_up_limits <- function(data){
#   
#   cl <- get_cl(data$y, "mean")
#   stdev <- sqrt(cl)
#   
#   z_i <- 
#   
#   data <- data %>%
#     dplyr::mutate(cl = cl) %>%
#     dplyr::mutate(ucl = cl + 3 * stdev) %>%
#     dplyr::mutate(lcl = cl - 3 * stdev)
#   
#   data
# }