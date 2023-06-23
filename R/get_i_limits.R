#function to get the i limits

get_i_limits <- function(y){
  i_mean <- mean(y)
  mean_mr <- mean(abs(diff(y)))
  sigma <- mean_mr/1.128 
  ucl_i <- i_mean + (3 * sigma)
  lcl_i <- i_mean - (3 * sigma)
  return(list(cl = rep(i_mean, length(y)), ucl = rep(ucl_i, length(y)), lcl = rep(lcl_i, length(y))))
}
