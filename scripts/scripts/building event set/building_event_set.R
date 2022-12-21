rm(list=ls())
library(here)

##### LOAD PACKAGES ######################
##########################################

script_loc <- paste0(here::here(), "/scripts/R/")
req_packages = c("general",  "visualisation", "statistical", "spatial")
source(paste0(script_loc, "libraries_and_file_locs.R"))

##### GET MODELS ########################
#########################################

file_input_loc <- paste0(input_file_loc, "/wildfire simulation model/models/")

generate_ERC_ARIMA <- read_rds(paste0(file_input_loc, "ERC_TSM_ARIMA.rds")) # TS Analysis - GET ERC 
generate_ERC_TBATS <- read_rds(paste0(file_input_loc, "ERC_TSM_TBATS.rds")) # TS Analysis - GET ERC 
generate_fire_igntion_days <- read_rds(paste0(file_input_loc, "fire_ignition_day_vars_ERC.rds")) # Logistic Model - Get  Fire Ignition Days
generate_num_ignitions <- read_rds(paste0(file_input_loc, "ecdf_num_ignitions.rds"))
generate_ave_wind_speed <- read_rds(paste0(file_input_loc, "ecdf_ave_wind_speed.rds"))
generate_mean_wind_direction <- read_rds(paste0(file_input_loc, "ecdf_mean_wind_direction.rds"))
pl_fit_lst <- read_rds(paste0(file_input_loc, "fire_size_power_law_alphas.rds"))

source(paste0(script_loc, "fns/fire_size_ln_model_fns.R")) # Lognormal Fire Size Distribution
# source(paste0(script_loc, "fns/pois_num_fires_per_day_model_fns.R")) # Poisson num fires per day 

# MODEL
source(paste0(script_loc, "fns/build_event_set_fns.R")) 

#### GET INPUT DATA ####################
########################################
seed = 123
set.seed(seed)

this_agg_fire_sizes <- fread(paste0(input_file_loc, "/wildfire simulation model/fire sizes/agg_fire_sizes_all_years_by_landtype.csv"))
this_LANDFIRE_shp <- readOGR(paste0(input_file_loc, "/shp/"),
                             layer="fuel_models_longlat_100agg_in_study_area_10km_buffer_shp")
# LANDFIRE key
landtype_key <- fread(paste0(input_file_loc, "/LANDFIRE/LF2022_FBFM13_220_CONUS/CSV_Data/LF20_F13_220.csv"))
landtype_key$Value <- as.character(landtype_key$Value)

#### MODEL PARAMS ######################
########################################

this_study_area_radius <- 100 # km
this_centroid_study_area <- c(-120.06, 36.03) # lon/lat

this_n_sim_years = 100
sq_km_to_acres = 247.105
n_runs = 10

#### RUN MODEL ########################
#######################################

for(j in 1:n_runs){
  
  message(paste("run number:", j))
  
  EVENTSET <- build_event_set(
    n_sim_years = this_n_sim_years,
    agg_fire_sizes = this_agg_fire_sizes[this_agg_fire_sizes$FBFM13 != "all fires",],
    TSA_ERC_modl = generate_ERC_TBATS,
    windspeed_ecdf = generate_ave_wind_speed,
    wind_direction_ecdf = generate_mean_wind_direction,
    n_fires_ecdf = generate_num_ignitions,                                                   
    LANDFIRE_shp = this_LANDFIRE_shp,
    fire_ignition_day_modl = generate_fire_igntion_days,
    centroid_study_area = this_centroid_study_area,
    study_area_radius = this_study_area_radius
  )
  
  # SAVE RESULT ##################################
  ################################################
  
  EVENTSET$sim_year <- EVENTSET$sim_year + (j-1)*this_n_sim_years  
  
  cols_to_keep <- c("EVENTID", "sim_day", "sim_year", "DOY", "ERC", "sim_windspeed",
                    "sim_wind_direction", "P_daily_ignition",
                    "lon", "lat", "lon_LANDFIRE", "lat_LANDFIRE",
                    "FBFM13", "FIRE_SIZE")
  
  write.csv(data.frame(EVENTSET[, ..cols_to_keep]),
            paste0(output_file_loc, "/EVENT_SET_", this_n_sim_years, "_yrs_SEED_", seed, "_run_", j, ".csv"),
            row.names=FALSE)

  EVENTSET <- FALSE

}


