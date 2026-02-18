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