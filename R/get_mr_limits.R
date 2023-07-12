#get moving range limits 
get_mr_limits <- function(y, 
                          na.rm = TRUE,
                          recursive_mr_screen = FALSE,
                          exclusion_points = NULL){
  
  #exclude exclusion points from calculations
  if(!is.null(exclusion_points)){
    y_excl <- y[-exclusion_points]
  }else{
    y_excl <- y
  }
  
  #loop function to remove all the extreme mr ranges 
  mr <- abs(diff(y_excl))
  mean_mr <- mean(mr, na.rm = TRUE)
  ucl_mr <- 3.267 * mean_mr 
  
  cl <- mean_mr 
  ucl <- ucl_mr
  
  return(list(cl = rep(cl, length(y)), ucl = rep(ucl, length(y))))
}
