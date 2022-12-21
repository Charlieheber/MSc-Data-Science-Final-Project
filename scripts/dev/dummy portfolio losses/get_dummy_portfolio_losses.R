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

get_location_losses_circular_WFS <- function(EVENTSET_w_FIRE_RADIUS, location, damage_ratio){
  
  EVENTSET_w_FIRE_RADIUS$loc_dist_to_fires_km <- distHaversine(location[, c("lon", "lat")], EVENTSET_w_FIRE_RADIUS[, c("lon", "lat")])/1000
  EVENTSET_w_FIRE_RADIUS$loc_in_fire <-  EVENTSET_w_FIRE_RADIUS$loc_dist_to_fires_km < EVENTSET_w_FIRE_RADIUS$FIRE_RADIUS_km                                   
  
  locs_in_EVENTS <- EVENTSET_w_FIRE_RADIUS[EVENTSET_w_FIRE_RADIUS$loc_in_fire,]
  locs_in_EVENTS$LOCID <- location$LOCID
  locs_in_EVENTS$TIV <- location$TIV
  locs_in_EVENTS$GU_loss <- location$TIV*damage_ratio
  locs_in_EVENTS$locnum <- location$locnum
  
  return(locs_in_EVENTS)
  
  
}
get_location_losses_circular_plus_hazard_ftprint_WFS <- function(EVENTSET_w_FIRE_RADIUS, location, damage_ratio){
  
  EVENTSET_w_FIRE_RADIUS <- this_EVENTSET_dist
  location <- this_location
  damage_ratio <- this_damage_ratio
  
  EVENTSET_w_FIRE_RADIUS$loc_dist_to_fires_km <- distHaversine(location[, c("lon", "lat")], EVENTSET_w_FIRE_RADIUS[, c("lon", "lat")])/1000
  
  whic()
  
  EVENTSET_w_FIRE_RADIUS$loc_in_fire <-  EVENTSET_w_FIRE_RADIUS$loc_dist_to_fires_km < EVENTSET_w_FIRE_RADIUS$FIRE_RADIUS_km                                   
  
  locs_in_EVENTS <- EVENTSET_w_FIRE_RADIUS[EVENTSET_w_FIRE_RADIUS$loc_in_fire,]
  locs_in_EVENTS$LOCID <- location$LOCID
  locs_in_EVENTS$TIV <- location$TIV
  locs_in_EVENTS$GU_loss <- location$TIV*damage_ratio
  locs_in_EVENTS$locnum <- location$locnum
  
  return(locs_in_EVENTS)
  
  
}

#### GET INPUT DATA ####################
########################################

# DUMMY PORTFOLIO
EDM_name <- "EDM_500properties_187Mn_TIV"
this_EDM <- fread(paste0(input_file_loc, "/dummy portfolio/", EDM_name, ".csv"))

# EVENTSET
EVENTSET_cols <- c("EVENTID", "ERC", "lon", "lat", "lon_LANDFIRE", "lat_LANDFIRE", "FBFM13", "FIRE_SIZE")
EVENTSET_name <- "EVENT_SET_1000_yrs_SEED_123"
this_EVENTSET <- fread(paste0(input_file_loc, "/EVENTSET/", EVENTSET_name,".csv"))


#### PARAMS ############################
########################################
acres_to_km_sq <- 0.00404686
this_damage_ratio <- 0.6

#### GET LOSSES JUST CIRCULAR FIRES  ##
#######################################

this_locs_in_EVENTS_df <- data.table("LOCID" = NA, "locnum" = NA, "sim_year" = NA,
                                     "TIV" = NA, "GU_loss" = NA, "EVENTID" = NA)
this_locs_in_EVENTS_df <- this_locs_in_EVENTS_df[!is.na(this_locs_in_EVENTS_df$LOCID),]

this_EVENTSET_dist <- this_EVENTSET[,c("EVENTID", "sim_year", "lon", "lat", "FIRE_SIZE")]
this_EVENTSET_dist$FIRE_RADIUS_km <- sqrt((this_EVENTSET_dist$FIRE_SIZE*acres_to_km_sq)/pi)

for(i in 1:length(this_EDM$LOCID)){
  
  if(i %% 10 ==0) message(paste("Location", i, "of", length(this_EDM$LOCID)))
  
  this_location <- this_EDM[i, ]
  
  this_locs_in_EVENTS <- get_location_losses_circular_WFS(this_EVENTSET_dist, this_location, this_damage_ratio)
  this_locs_in_EVENTS_df <- rbind(this_locs_in_EVENTS_df, this_locs_in_EVENTS[,c("LOCID", "locnum", "sim_year", "EVENTID", "TIV", "GU_loss")])

}


sum(this_locs_in_EVENTS_df$GU_loss)/1000

this_YLT <- this_locs_in_EVENTS_df %>%
  group_by(sim_year) %>%
  summarise(
    "GU_loss" = sum(GU_loss)
  )
this_YLT <- this_YLT[order(this_YLT$GU_loss),]

##### SAVE RESULT ##################
####################################

write.csv(this_YLT, 
          paste0(output_file_loc, "/", EVENTSET_name, "_", EDM_name, "_circularWFs_YLT.csv"),
          row.names=FALSE)

