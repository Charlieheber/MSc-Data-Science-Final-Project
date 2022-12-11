rm(list=ls())
library(here)

##### LOAD PACKAGES ######################
##########################################

script_loc <- paste0(here::here(), "/scripts/R/")
req_packages = c("general",  "visualisation", "statistical", "spatial", "fire modelling")
source(paste0(script_loc, "libraries_and_file_locs.R"))

##### GET MODELS ########################
#########################################

file_input_loc <- paste0(input_file_loc, "/wildfire simulation model/models/")

# MODELLING SPREAD
source(paste0(script_loc, "/fns/modelling_fire_spread_fns.R"))

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


get_proxy_fuel_moisture_from_historical_record <- function(sim_ERC, historical_ERC){
  
  colnames <- c("id", "date", "MC_1hr", "MC_10hr", "MC_100hr",
                "MC_herb", "MC_wood", "ERC")
  
  historical_ERC[which.min(abs(sim_ERC - historical_ERC$ERC)), ..colnames]
  
}

#### GET INPUT DATA ####################
########################################
seed = 123
set.seed(seed)

# EVENTSET
EVENTSET_cols <- c("EVENTID", "ERC", "lon", "lat", "lon_LANDFIRE", "lat_LANDFIRE", "FBFM13", "FIRE_SIZE")
EVENTSET_name <- "EVENT_SET_100_yrs_SEED_123"
this_EVENTSET <- fread(paste0(input_file_loc, "/EVENTSET/", EVENTSET_name, ".csv"))

this_EVENTSET <- this_EVENTSET[,..EVENTSET_cols]

# LAND TYPE
LANDFIRE_shp_name <- "fuel_models_longlat_100agg_in_study_area_10km_buffer_shp_PREP_4_fire_spread_model"
this_LANDFIRE_shp <- readOGR(paste0(input_file_loc, "/shp/"), layer=LANDFIRE_shp_name)

names(this_LANDFIRE_shp)[names(this_LANDFIRE_shp) == "grd_lng"] <- "grid_length"

# LANDFIRE key
landtype_key <- fread(paste0(input_file_loc, "/LANDFIRE/LF2022_FBFM13_220_CONUS/CSV_Data/LF20_F13_220.csv"))

# Daily fuel moisture
MC_daily <- fread(paste0(input_file_loc, "/wildfire simulation model/fuel moisture contents/station_data_kettleman_hills_00_22_w_ERC.csv"))

#### PARAMS ###############################################
###########################################################
acres_to_km_sq <- 0.00404686
grid_sq_size_km_sq <- 9

this_modeltype <- "D"
this_u <- 4
this_slope <- 0
fuel_models <- firebehavioR::fuelModels[which(fuelModels$source == "Anderson (1982)"),]
fuel_models <- cbind(fuel_models, landtype_key[1:13, c("Value", "FBFM13", "typical_fuel_complex")])

this_agri_ROS <- 1.2
this_urban_ROS <- 0.8
this_water_ROS <- 0.01
this_barren_ROS <- 1.2
this_no_data_ROS <- 1.2

#### GET PROXY FUEL MOISTURE VALUES FOR EACH IGNITION #####
###########################################################

this_proxy_fuel_moistures <- rbindlist(
  apply(this_EVENTSET, 
        1,
        function(x) get_proxy_fuel_moisture_from_historical_record(as.numeric(x["ERC"]), MC_daily))
)

this_EVENTSET <- cbind(this_EVENTSET, this_proxy_fuel_moistures[,c("MC_1hr", "MC_10hr", "MC_100hr",
                                                                   "MC_herb", "MC_wood")])
this_proxy_fuel_moistures <- NULL

#### RUN MODEL ###############################################
##############################################################

this_EVENTSET_model_spread <- this_EVENTSET[which(this_EVENTSET$FIRE_SIZE > grid_sq_size_km_sq/acres_to_km_sq),]

num_events <- length(this_EVENTSET_model_spread$EVENTID)
this_grid_res_sim_lst <- list()
for(i in 1:num_events){
  
  #### GET FIRE SPREAD RATES #################################
  ############################################################
  
  this_EVENT <- this_EVENTSET_model_spread[i,]
  this_EVENT$FIRE_SIZE_grid_sq <- round(this_EVENT$FIRE_SIZE*acres_to_km_sq/grid_sq_size_km_sq)
  
  message(paste0("modelling EVENTID ", this_EVENT$EVENTID, " (event ", i, " of ", num_events, ")"))
  message(paste0("ignition coords: ", this_EVENT$lon, ", ", this_EVENT$lat))
  message(paste("fire size:", this_EVENT$FIRE_SIZE_grid_sq, "grid squares"))
  
  # grid sq ignition is in 
  this_LANDFIRE_EVENT <- this_LANDFIRE_shp[which.min(geosphere::distHaversine(this_EVENT[, c("lon", "lat")], this_LANDFIRE_shp@data[, c("lon", "lat")])),]
  
  # subset around ignition grid sq
  subset_diameter <- ceiling(this_EVENT$FIRE_SIZE_grid_sq/10+1)
  
  this_LANDFIRE_subset_shp <- this_LANDFIRE_shp[which(this_LANDFIRE_shp$colNum %in% (this_LANDFIRE_EVENT$colNum-subset_diameter):(this_LANDFIRE_EVENT$colNum+subset_diameter) &
                                                        this_LANDFIRE_shp$rowNum %in% (this_LANDFIRE_EVENT$rowNum-subset_diameter):(this_LANDFIRE_EVENT$rowNum+subset_diameter)),]
  
  this_ROS_lst <- list()
  message("getting rate of spread for grid squares in vicinity")
  for(j in 1:length(this_LANDFIRE_subset_shp)){
    
    if(j %% 100 == 0) message(paste("grid square:", j))
    
    this_grid_sq <- this_LANDFIRE_subset_shp@data[j,]
    this_ROS_lst[[j]] <- get_Rothermel(
      this_grid_sq,
      this_EVENT, 
      fuel_models, 
      this_modeltype, 
      this_u, 
      this_slope,
      this_agri_ROS,
      this_urban_ROS,
      this_water_ROS,
      this_barren_ROS,
      this_no_data_ROS
    )
    
  }
  
  this_LANDFIRE_subset_shp$fire_spread_rate <- rbindlist(lapply(this_ROS_lst, 
                                                                function(x) data.frame(x)))[[1]]
  this_LANDFIRE_subset_shp@data
  this_LANDFIRE_mod_shp <- this_LANDFIRE_subset_shp[,c("locnum", "lon", "lat",
                                                       "colNum", "rowNum", "grid_length", "fire_spread_rate")]
  
  ##### MODEL FIRE SPREAD #################################
  #########################################################
  
  message("modelling fire spread\n")
  this_grid_sim <- run_burn_simulation(this_LANDFIRE_mod_shp@data, this_LANDFIRE_EVENT$locnum, this_EVENT$FIRE_SIZE_grid_sq)
  this_grid_sim <- this_grid_sim[this_grid_sim$burning,]
  
  this_grid_sim$EVENTID <- this_EVENT$EVENTID
  this_grid_sim$FIRE_SIZE_grid_sq <- this_EVENT$FIRE_SIZE_grid_sq
  
  this_grid_sim_ordered <- this_grid_sim[order(this_grid_sim$burn_delay),]
  
  this_grid_sim_res <- this_grid_sim_ordered[1:unique(this_grid_sim_ordered$FIRE_SIZE_grid_sq),]
  
  this_grid_res_sim_lst[[i]] <- this_grid_sim_res[,c("EVENTID",  "locnum", "lon", "lat",
                                                     "colNum", "rowNum", "grid_length",
                                                     "fire_spread_rate", "burn_delay",
                                                     "burning", "found_NNs", 
                                                     "FIRE_SIZE_grid_sq")]
}

this_grid_sim_res <- rbindlist(this_grid_res_sim_lst)

#### SAVE RESULT ###################################
####################################################

write.csv(this_grid_sim_res, 
          paste0(output_file_loc, "/", EVENTSET_name,  "_HAZARD_FOOTPRINTS.csv"),
          row.names=FALSE)





