get_lognormal_fire_size_r_vars <- function(ave_fire_size, sd_fire_size, n, landtype){
  
  location <- log(ave_fire_size^2/sqrt(sd_fire_size^2 + ave_fire_size^2))
  shape <- sqrt(log(1 +  (sd_fire_size^2/ave_fire_size^2)))
  
  # generate random variables
  this_fire_size_LN_pred_fit <- data.frame(
    "FIRE_SIZE" = rlnormTrunc(n, 
                              mean=location, 
                              sd=shape, 
                              min=0),
    "FBFM13" = landtype,
    "source" = "ln-dist")
  
  return(this_fire_size_LN_pred_fit)
  
}
get_lognormal_fire_size_r_vars_wrapper <- function(agg_fire_size_dat_dist_fit){
  
  # generate lognormal dist random values for each landtype 
  this_fire_size_LN_pred_fit_lst <- apply(this_agg_fire_size_dat_dist_fit, 1, 
                                          function(x) get_lognormal_fire_size_r_vars(
                                            as.numeric(x[["ave_fire_size"]]), 
                                            as.numeric(x[["sd_fire_size"]]), 
                                            500, 
                                            x[["FBFM13"]]))
  this_fire_size_LN_pred_fit <- rbindlist(this_fire_size_LN_pred_fit_lst) 
  
  return(agg_fire_size_dat_dist_fit)
  
}
