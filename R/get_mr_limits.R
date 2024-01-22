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
                          recursive_mr_screen = FALSE,
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
