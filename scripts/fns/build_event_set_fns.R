
get_emp_df_val <- function(ecdf, n){
  
  vals = knots(ecdf)
  
  x <- data.frame(
    "vals" = vals,
    "prob" = ecdf(vals) - c(0, ecdf(vals)[1:(length(vals)-1)])
  )
  
  return(sample(x$vals, n, prob=x$prob))
  
}

build_event_set <- function(n_sim_years, 
                            agg_fire_sizes, 
                            TSA_ERC_modl,
                            windspeed_ecdf,
                            wind_direction_ecdf,
                            n_fires_ecdf,
                            LANDFIRE_shp,
                            fire_ignition_day_modl,
                            centroid_study_area,
                            study_area_radius,
                            modelled_ERC = FALSE,
                            ERC){
  

  
  # SET UP RES DF
  res_df <- data.table(
    "sim_day" = 1:(n_sim_years*365),
    "sim_year" = 1:n_sim_years,
    "DOY" = rep(1:365, n_sim_years)
  )
  
  
  # GENERATE DAILY ERC: ERC TBATS
  message("Simulating Daily Energy Release Component (ERC)")
  ERC_TBATS_pred <-c()
  for(i in 1:n_sim_years){
    
    if(i%%10==0) message(paste0("simulated year: ", i)) 
    ERC_TBATS_pred <- append(ERC_TBATS_pred, simulate(TSA_ERC_modl, nsim = 365))
  }
  res_df$ERC <- ERC_TBATS_pred
    

  # GENERATE DAILY WINDSPEED/DIRECTION 
  message("Simulate wind speed and direction")
  res_df$sim_windspeed <- apply(matrix(rep(1,length(res_df$sim_day))), 1, function(x) get_emp_df_val(windspeed_ecdf, x))
  res_df$sim_wind_direction <- apply(matrix(rep(1,length(res_df$sim_day))), 1, function(x) get_emp_df_val(wind_direction_ecdf, x))
  
  # MODEL DAILY FIRE IGNITIONS 
  message("Modelling probability of fire ignition w/ prediction interval")
  res_df$P_daily_ignition <- stats::predict(generate_fire_igntion_days, res_df[, c("ERC")], type="response")

  # GET 95% CONFIDENCE BOUNDS (PREDICTION INTERVAL)
  logit_p <- stats::predict(generate_fire_igntion_days, res_df[, c("ERC")])
  Bigma <- vcov(generate_fire_igntion_days)
  sig=Vectorize(function(x) sqrt(Bigma[1,1] + x^2*Bigma[2,2] + 2*x*Bigma[1,2]))
  
  theta_L = logit_p - 1.96*sig(res_df[, c("ERC")])
  theta_U = logit_p + 1.96*sig(res_df[, c("ERC")])
  p_L = plogis(theta_L)
  p_U = plogis(theta_U)
  
  res_df$P_daily_ignition_Lower <- p_L
  res_df$P_daily_ignition_Upper <- p_U


  res_df$fire_ignition_day <-  apply(res_df, 1, function(x){
    
    prob_ignition = runif(1, min=x["P_daily_ignition_Lower"], max=x["P_daily_ignition_Upper"])
    sample(c(FALSE,TRUE), 
           size = 1, 
           replace = TRUE, 
           prob = c(1-prob_ignition, prob_ignition))
    
  }) 
  
  
  # MODEL NUMBER OF FIRES
  message("Simulate number of fires in day")
  
  res_df$num_fires = 0
  num_ignition_days <- sum(res_df$fire_ignition_day)
  res_df[which(res_df$fire_ignition_day),]$num_fires <- apply(matrix(rep(1,num_ignition_days)), 1, function(x) get_emp_df_val(n_fires_ecdf, 1))
  
  


  # GET DF BY EVENT (RATHER THAN DAY)
  message("Reformat data")
  
  res_by_fire_df <- res_df[rep(seq_len(dim(res_df)[1]), res_df$num_fires), ]
  res_by_fire_df$num_fires <- NULL
  res_by_fire_df$EVENTID <- 1:length(res_by_fire_df$sim_day)
  
  # RANDOMLY GENERATE FIRE LOCATION
  message("Randomly generate ignition location")
  
  bb_scaling <- 1.1
  study_area_bounding_box <- c("lon" = c(centroid_study_area[1]-bb_scaling, centroid_study_area[1]+bb_scaling),
                               "lat" = c(centroid_study_area[2]-bb_scaling, centroid_study_area[2]+bb_scaling))
  
  x_bounding_box <- runif(sum(res_df$num_fires)*1.8, study_area_bounding_box["lon1"], study_area_bounding_box["lon2"])
  y_bounding_box <- runif(sum(res_df$num_fires)*1.8, study_area_bounding_box["lat1"], study_area_bounding_box["lat2"])
  
  z <- geosphere::distHaversine(data.table(x_bounding_box, y_bounding_box), c(-120.06, 36.03))/1000
  
  simulated_fire_positions <- data.frame(
    "lon" = x_bounding_box, "lat" = y_bounding_box, "dist_from_study_area" = z
  )
  simulated_fire_positions$in_study_area = FALSE
  simulated_fire_positions[which(simulated_fire_positions$dist_from_study_area < 100), "in_study_area"] = TRUE
  
  simulated_fire_positions_study_area <- simulated_fire_positions[simulated_fire_positions$in_study_area, ]
  
  res_by_fire_df$lon <- simulated_fire_positions_study_area[, "lon"][1:length(res_by_fire_df$EVENTID)]
  res_by_fire_df$lat <- simulated_fire_positions_study_area[, "lat"][1:length(res_by_fire_df$EVENTID)]
  
  
  # GET LAND TYPE OF EACH 
  message("Get land type for each fire ignition location")
  
  # turn to spatial points
  res_by_fire_shp <- SpatialPoints(res_by_fire_df[,c("lon", "lat")], LANDFIRE_shp@proj4string)
  
  fire_LANDFIRE_intersect <- gIntersects(res_by_fire_shp, LANDFIRE_shp, byid=TRUE, returnDense=FALSE)
  res_by_fire_df$LANDFIRE_indx <- rbindlist(lapply(fire_LANDFIRE_intersect, function(x) data.frame("LANDFIRE"=x)))
  
  LANDFIRE_shp$LANDFIRE_indx <- as.integer(row.names(LANDFIRE_shp@data))
  
  # join landtype to fires 
  res_by_fire_w_landfire_df <- res_by_fire_df %>% 
    left_join(LANDFIRE_shp@data[,c("lon", "lat", "LC22_F1", "LANDFIRE_indx")], by="LANDFIRE_indx", suffix=c("", "_LANDFIRE")) %>%
    left_join(landtype_key[,c("Value", "FBFM13")], by=c("LC22_F1"="Value")) %>%
    left_join(this_agg_fire_sizes[,c("FBFM13", "ave_fire_size", "sd_fire_size")], by=c("FBFM13"="FBFM13"), )
  
  # SIMULATE FIRE SIZES
  message("Simulate Fire Sizes")
  # sim_FIRE_SIZEs <- get_lognormal_fire_size_r_vars_wrapper(res_by_fire_w_landfire_df, 1)
  sim_FIRE_SIZEs <- get_LN_PL_fire_size_r_vars_wrapper(res_by_fire_w_landfire_df, 1)
  
  res_by_fire_w_landfire_df$FIRE_SIZE <- sim_FIRE_SIZEs$FIRE_SIZE
  
  max(res_by_fire_w_landfire_df$FIRE_SIZE)
  
  return(res_by_fire_w_landfire_df)
  
}
