#get i limits
#input y as a vector. returns cl, ucl and lcl as a list. 

get_i_limits <- function(y, 
                         na.rm = TRUE,
                         recursive_mr_screen =  FALSE,
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
  max_loops <- if(recursive_mr_screen){
    Inf
  }else{
    1L
  }
  
  i <- 0L
  
  #calculate limits in cases where there are mRs above the upper range limit
  while(any(mr > ucl_mr) & (i < max_loops)){
    mr <- mr[mr < ucl_mr]
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
