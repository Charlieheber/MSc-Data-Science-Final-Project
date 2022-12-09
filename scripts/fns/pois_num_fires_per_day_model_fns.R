
pois_num_fires_per_day_model <- function(lambda, n){
  
  # want to count from 0 (ie. 0 is one fire per day)
  lambda = lambda - 1
  
  num_ignitions <- rpois(n, lambda)
  num_ignitions = num_ignitions + 1 
  
  
  return(num_ignitions)  
}