# ROTHERMEL ROS
get_Rothermel <- function(grid_sq, 
                          EVENT, 
                          fuel_models, 
                          modeltype, 
                          u,
                          slope,
                          agri_ROS,
                          urban_ROS,
                          water_ROS,
                          barren_ROS,
                          no_data_ROS){
  
  # grid_sq <- this_grid_sq
  # EVENT <- this_EVENT
  # fuel_models
  # modeltype <- this_modeltype
  # u <- this_u
  # slope <- this_slope
  
  if(grid_sq$FBFM13=="Agriculture") return(agri_ROS)
  if(grid_sq$FBFM13=="Urban") return(urban_ROS)
  if(grid_sq$FBFM13=="Water") return(water_ROS)
  if(grid_sq$FBFM13=="Barren") return(barren_ROS)
  if(grid_sq$FBFM13=="Fill-NoData") return(no_data_ROS)
  
  MC  <- EVENT[,c("MC_1hr", "MC_10hr", "MC_100hr", "MC_herb", "MC_wood")]
  grid_sq_fuel_mod <- fuel_models[which(fuel_models$FBFM13==grid_sq$FBFM13),]
  
  if(MC$MC_herb < 30) MC$MC_herb = 30
  
  res <- Rothermel::ros(modeltype,
                        grid_sq_fuel_mod[, c("load1hr", "load10hr", "load100hr",
                                             "loadLiveHerb", "loadLiveWoody")],
                        grid_sq_fuel_mod[, c("sav1hr", "sav10hr", "sav100hr",
                                             "savLiveHerb", "savLiveWoody")],
                        grid_sq_fuel_mod$fuelBedDepth,
                        grid_sq_fuel_mod$mxDead,
                        rep(grid_sq_fuel_mod$heat,5),
                        MC,
                        u,
                        slope)
  
  if(res$`ROS [m/min]` == 0) return(0.01)
  
  return(res$`ROS [m/min]`)
  
  
}
