build_event_set <- function(n_sim_years, 
                            pois_lambda, 
                            agg_fire_sizes, 
                            TSA_ERC_modl,
                            LANDFIRE_shp,
                            fire_ignition_day_modl,
                            centroid_study_area,
                            study_area_radius){
  
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
  
  
  # MODEL DAILY FIRE IGNITIONS 
  message("Modelling probability of fire ignition")
  res_df$P_daily_ignition <- predict(generate_fire_igntion_days, res_df[, c("ERC")], type="response")
  
  res_df$fire_ignition_day <-  apply(res_df, 1, function(x) sample(c(FALSE,TRUE), 
                                                                   size = 1, 
                                                                   replace = TRUE, 
                                                                   prob = c(1-x["P_daily_ignition"], x["P_daily_ignition"])))
  
  
  # MODEL NUMBER OF FIRES
  message("Modelling number of fires in day")
  
  res_df$num_fires = 0
  res_df[which(res_df$fire_ignition_day),]$num_fires <- pois_num_fires_per_day_model(pois_lambda, sum(res_df$fire_ignition_day))
  
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
  sim_FIRE_SIZEs <- get_lognormal_fire_size_r_vars_wrapper(res_by_fire_w_landfire_df, 1)
  
  res_by_fire_w_landfire_df$FIRE_SIZE <- sim_FIRE_SIZEs$FIRE_SIZE
  
  max(res_by_fire_w_landfire_df$FIRE_SIZE)
  
  return(res_by_fire_w_landfire_df)
  
}
