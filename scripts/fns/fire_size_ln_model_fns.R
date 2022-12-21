get_lognormal_fire_size_r_vars <- function(ave_fire_size, sd_fire_size, n, landtype){
  
  location <- log(ave_fire_size^2/sqrt(sd_fire_size^2 + ave_fire_size^2))
  shape <- sqrt(log(1 +  (sd_fire_size^2/ave_fire_size^2)))
  
  # generate random variables
  this_fire_size_LN_pred_fit <- data.frame(
    "FIRE_SIZE" = rlnormTrunc(n, 
                              mean=location, 
                              sd=shape, 
                              min=10),
    "FBFM13" = landtype,
    "source" = "ln-dist")
  
  return(this_fire_size_LN_pred_fit)
  
}
get_lognormal_fire_size_r_vars_wrapper <- function(agg_fire_size_dat_dist_fit, n){
  
  # generate lognormal dist random values for each landtype 
  fire_size_LN_pred_fit_lst <- apply(agg_fire_size_dat_dist_fit, 1, 
                                          function(x) get_lognormal_fire_size_r_vars(
                                            as.numeric(x[["ave_fire_size"]]), 
                                            as.numeric(x[["sd_fire_size"]]), 
                                            n, 
                                            x[["FBFM13"]]))
  fire_size_LN_pred_fit <- rbindlist(fire_size_LN_pred_fit_lst) 
  
  fire_size_LN_pred_fit[which(fire_size_LN_pred_fit$FBFM13 == "Water"), "FIRE_SIZE"] <- 0 
  
  return(fire_size_LN_pred_fit)
  
}
get_LN_PL_fire_size_r_vars <- function(ave_fire_size, sd_fire_size, pl_fit_lst, n_fires, landtype, dist="power-law"){
  
  # ave_fire_size <- as.numeric(x[["ave_fire_size"]])
  # sd_fire_size <- as.numeric(x[["sd_fire_size"]])
  # pl_fit_lst
  # n_fires
  # landtype <- x[["FBFM13"]]

  
  if(dist=="lnorm"){
    
    location <- log(ave_fire_size^2/sqrt(sd_fire_size^2 + ave_fire_size^2))
    shape <- sqrt(log(1 +  (sd_fire_size^2/ave_fire_size^2)))
    
    # generate random variables
    this_fire_size_LN_pred_fit <- data.frame(
      "FIRE_SIZE" = rlnormTrunc(n_fires, 
                                mean=location, 
                                sd=shape, 
                                min=10),
      "FBFM13" = landtype,
      "source" = "ln-dist")
    
    return(this_fire_size_LN_pred_fit)
    
    
  } else if(dist =="power-law"){
    
    # landtype = "Water"
    alpha = pl_fit_lst[which(names(pl_fit_lst) == landtype)][[1]]$alpha
    
    this_fire_size_PL_pred_fit <- data.frame(
      "FIRE_SIZE" = rpldis(n_fires,
                           alpha=alpha, xmin=10),
      "FBFM13" = landtype,
      "source" = "power-law")
    return(this_fire_size_PL_pred_fit)
    
    
  }
  
}
get_LN_PL_fire_size_r_vars_wrapper <- function(agg_fire_size_dat_dist_fit, n_fires){
  
  # agg_fire_size_dat_dist_fit <- this_agg_fire_sizes[!(this_agg_fire_sizes$FBFM13=="all fires"),]
  # n_fires <- 1
  # x <- agg_fire_size_dat_dist_fit[1,]
  
  # generate lognormal dist random values for each landtype 
  fire_size_LN_PL_fit_lst <- apply(agg_fire_size_dat_dist_fit, 1, 
                                          function(x) get_LN_PL_fire_size_r_vars(
                                            as.numeric(x[["ave_fire_size"]]), 
                                            as.numeric(x[["sd_fire_size"]]), 
                                            pl_fit_lst,
                                            n_fires,
                                            x[["FBFM13"]]))
  fire_size_LN_PL_pred_fit <- rbindlist(fire_size_LN_PL_fit_lst) 
  
  return(fire_size_LN_PL_pred_fit)
  
}





