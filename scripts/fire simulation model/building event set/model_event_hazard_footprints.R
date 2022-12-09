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

# MODEL
source(paste0(script_loc, "fns/build_event_set_fns.R")) # Poisson num fires per day 

#### GET INPUT DATA ####################
########################################
seed = 123
set.seed(seed)

# EVENTSET
EVENTSET_cols <- c("EVENTID", "ERC", "lon", "lat", "lon_LANDFIRE", "lat_LANDFIRE", "FBFM13", "FIRE_SIZE")
this_EVENTSET <- fread(paste0(input_file_loc, "/EVENTSET/EVENT_SET_100_yrs_SEED_123.csv"))

this_EVENTSET <- this_EVENTSET[,..EVENTSET_cols]

# LAND TYPE 
this_LANDFIRE_shp <- readOGR(paste0(input_file_loc, "/shp/"),
                             layer="fuel_models_longlat_100agg_in_study_area_10km_buffer_shp")
this_LANDFIRE_shp@data

# LANDFIRE key
landtype_key <- fread(paste0(input_file_loc, "/LANDFIRE/LF2022_FBFM13_220_CONUS/CSV_Data/LF20_F13_220.csv"))
landtype_key$Value <- as.character(landtype_key$Value)

this_LANDFIRE_shp <- sp::merge(this_LANDFIRE_shp, landtype_key[, c("Value", "FBFM13", "typical_fuel_complex")], by.x="LC22_F1", by.y="Value")

# ANDERSON FUEL MODELS 
fuel_models <- fread(paste0(input_file_loc, "/Anderson_fuel_models.csv"))

# Daily fuel moisture
MC_daily <- fread(paste0(input_file_loc, "/wildfire simulation model/fuel moisture contents/station_data_kettleman_hills_00_22_w_ERC.csv"))

#### GET PROXY FUEL MOISTURE VALUES FOR EACH IGNITION #####
###########################################################

get_proxy_fuel_moisture_from_historical_record <- function(sim_ERC, historical_ERC){
  
  colnames <- c("id", "date", "MC_1hr", "MC_10hr", "MC_100hr",
                "MC_herb", "MC_wood", "ERC")
  
  historical_ERC[which.min(abs(sim_ERC - historical_ERC$ERC)), ..colnames]
  
}

this_proxy_fuel_moistures <- rbindlist(
  apply(this_EVENTSET, 
        1,
        function(x) get_proxy_fuel_moisture_from_historical_record(as.numeric(x["ERC"]), MC_daily))
  )

this_EVENTSET <- cbind(this_EVENTSET, this_proxy_fuel_moistures[,c("MC_1hr", "MC_10hr", "MC_100hr",
                                                  "MC_herb", "MC_wood")])


#### VISUALISE FUELS ######################################
###########################################################

colours <- c("grass" = "forestgreen", 
             "shrub" = "darksalmon",
             "timber_litter" = "burlywood4",
             "slash" = "purple",
             "urban" = "grey",
             "snow" = "white",
             "agriculture" = "darkgoldenrod1",
             "water" = "blue",
             "barren" = "darkorange4")

fuel_pal <- colorFactor(
  palette = colours,
  domain = names(colours),
  ordered = TRUE
)


leaflet(this_LANDFIRE_shp[which(as.logical(this_LANDFIRE_shp$in_std_)),]) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addProviderTiles("Stamen.TonerLabels") %>%
  addPolygons(fillOpacity=0.8, opacity=0,
              fillColor=~fuel_pal(typical_fuel_complex), color="black",
              label=~typical_fuel_complex) %>%
  addLegend(
    pal = fuel_pal, title = "Fueltypes in study area", values = names(colours),
    opacity = 1
  )

#### GET FIRE SPREAD RATES #################################
############################################################

this_EVENT <- this_EVENTSET[this_EVENTSET$EVENTID == 1,]

Rothermel:ros




