rm(list=ls())
library(here)

##### LOAD PACKAGES ######################
##########################################

script_loc <- paste0(here::here(), "/scripts/R/")
req_packages = c("general",  "visualisation", "spatial")
source(paste0(script_loc, "libraries_and_file_locs.R"))

##### GET MODELS ########################
#########################################

file_input_loc <- paste0(input_file_loc, "/wildfire simulation model/models/")

#### GET INPUT DATA ####################
########################################

# DUMMY PORTFOLIO
EDM_name <- "EDM_100properties_1e+06Mn_TIV"
this_EDM <- fread(paste0(input_file_loc, "/dummy portfolio/", EDM_name, ".csv"))

# EVENTSET
EVENTSET_cols <- c("EVENTID", "ERC", "lon", "lat", "lon_LANDFIRE", "lat_LANDFIRE", "FBFM13", "FIRE_SIZE")
EVENTSET_name <- "EVENT_SET_100_yrs_SEED_123"
this_EVENTSET <- fread(paste0(input_file_loc, "/EVENTSET/", EVENTSET_name, ".csv"))

this_EVENTSET <- this_EVENTSET[,..EVENTSET_cols]

# HAZARD FOOTPRINTS
HAZARD_FOOTPRINTS_name <- "EVENT_SET_100_yrs_SEED_123_HAZARD_FOOTPRINTS"
this_HAZARD_FOOTPRINTS <- fread(paste0(input_file_loc, "/EVENTSET/", HAZARD_FOOTPRINTS_name, ".csv"))

#### PARAMS ############################
########################################
acres_to_km_sq <- 0.00404686
damage_ratio <- 0.6

#### GET LOSSES #######################
#######################################

this_EVENTSET_dist <- this_EVENTSET[,c("EVENTID", "lon", "lat", "FIRE_SIZE")]
this_EVENTSET_dist$FIRE_RADIUS_km <- sqrt((this_EVENTSET_dist$FIRE_SIZE*acres_to_km_sq)/pi)

this_locs_in_EVENTS_lst <- list()
for(i in 1:length(this_EDM$LOCID)){
  
  if(i %% 10 ==0) message(paste("Location", i, "of", length(this_EDM$LOCID)))
  
  this_location <- this_EDM[i, ]
  
  this_EVENTSET_dist$loc_dist_to_fires_km <- distHaversine(this_location[, c("lon", "lat")], this_EVENTSET[, c("lon", "lat")])/1000
  this_EVENTSET_dist$loc_in_fire <-  this_EVENTSET_dist$loc_dist_to_fires_km < this_EVENTSET_dist$FIRE_RADIUS_km                                   
  
  this_locs_in_EVENTS <- this_EVENTSET_dist[this_EVENTSET_dist$loc_in_fire,]
  this_locs_in_EVENTS$LOCID <- this_location$LOCID
  this_locs_in_EVENTS$TIV <- this_location$TIV
  this_locs_in_EVENTS$GU_loss <- this_location$TIV*damage_ratio
  this_locs_in_EVENTS$locnum <- this_location$locnum
  
  this_locs_in_EVENTS_lst[[i]] <- this_locs_in_EVENTS[,c("LOCID", "locnum", "TIV", "GU_loss", "EVENTID")]
}

this_locs_in_EVENTS <- rbindlist(this_locs_in_EVENTS_lst)

##### SAVE RESULT ##################
####################################

write.csv(this_locs_in_EVENTS, 
          paste0(output_file_loc, "/", EVENTSET_name, "_", EDM_name, "_losses.csv"),
          row.names=FALSE)

