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
source(paste0(script_loc, "fns/fire_size_ln_model_fns.R")) # Lognormal Fire Size Distribution
source(paste0(script_loc, "fns/pois_num_fires_per_day_model_fns.R")) # Poisson num fires per day 

# MODEL
source(paste0(script_loc, "fns/build_event_set_fns.R")) # Poisson num fires per day 

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
this_lambda = 1.225 # from pois_num_fires_per_day_model_fns.R
sq_km_to_acres = 247.105

#### RUN MODEL ########################
#######################################

EVENTSET <- build_event_set(
  n_sim_years = this_n_sim_years,
  pois_lambda = this_lambda,
  agg_fire_sizes = this_agg_fire_sizes,
  TSA_ERC_modl = generate_ERC_TBATS,
  LANDFIRE_shp = this_LANDFIRE_shp,
  fire_ignition_day_modl = generate_fire_igntion_days,
  centroid_study_area = this_centroid_study_area,
  study_area_radius = this_study_area_radius
)


# VISUALISE #####################################
#################################################

# NUM FIRES PER DAY
res_num_fires_by_day <- EVENTSET %>%
  group_by(DOY) %>%
  summarise(
    num_fires = n()
  )

ggplot(res_num_fires_by_day, aes(x=DOY, y=num_fires)) +
  geom_bar(stat="identity", position="dodge")+
  theme_minimal()


# EP CURVE: AREA BURNED BY YEAR
res_area_burned_by_year <- EVENTSET %>%
  group_by(sim_year) %>%
  summarise(
    tot_area_burned_acres = sum(FIRE_SIZE)
  )
res_area_burned_by_year <- res_area_burned_by_year[order(res_area_burned_by_year$tot_area_burned_acres),] 

res_area_burned_by_year$RP <- 1:max(res_area_burned_by_year$sim_year) 
res_area_burned_by_year$EP <- 1/res_area_burned_by_year$RP
res_area_burned_by_year$nonEP <- 1-res_area_burned_by_year$EP

ggplot(res_area_burned_by_year, aes(x=tot_area_burned_acres, y=EP)) +
  ylim(c(0,0.1)) +
  # geom_point(stat="identity")+
  geom_line(size=1) +
  theme_minimal()

# MAP FIRE YEARS (ASSUMING SPHERICAL)
# PLOT FIRE SIZE BY YEAR (ASSUMING SPHERICAL)
map_spherical_fires_by_year <- function(res_fires_df, sim_year){
  
  res_fires_df <- data.frame(res_fires_df)
  year_res_fires_df <- res_fires_df[which(res_fires_df$sim_year == sim_year),]
  year_res_fires_df$FIRE_SIZE_sq_km <- year_res_fires_df$FIRE_SIZE/sq_km_to_acres
  
  spherical_fire_radii <-  sqrt(year_res_fires_df$FIRE_SIZE_sq_km/pi)
  
  leaflet(year_res_fires_df) %>%
    addProviderTiles("Esri.WorldImagery") %>%
    addCircles(lng=~lon, lat=~lat, radius=~spherical_fire_radii*1000, weight=2, fillOpacity=1, opacity=1,
               fillColor="grey", color="black") %>%
    addLegend(colors = "grey", labels="simulated spherical fire radii", title = paste("Spherical Fire Radii: year", sim_year),
              position="bottomleft")
  
  
}
map_spherical_fires_by_year(EVENTSET, 2)

# SAVE RESULT ##################################
################################################

cols_to_keep <- c("EVENTID", "sim_day", "sim_year", "DOY", "ERC", "P_daily_ignition",
                  "lon", "lat", "lon_LANDFIRE", "lat_LANDFIRE",
                  "FBFM13", "FIRE_SIZE")

write.csv(data.frame(EVENTSET[, ..cols_to_keep]), 
          paste0(output_file_loc, "/EVENT_SET_", this_n_sim_years, "_yrs_SEED_", seed, ".csv"),
          row.names=FALSE)

