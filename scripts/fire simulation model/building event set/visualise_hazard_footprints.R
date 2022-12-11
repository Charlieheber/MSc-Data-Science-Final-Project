rm(list=ls())
library(here)

##### LOAD PACKAGES ######################
##########################################

script_loc <- paste0(here::here(), "/scripts/R/")
req_packages = c("general",  "visualisation", "spatial")
source(paste0(script_loc, "libraries_and_file_locs.R"))

##### GET DATA ##########################
#########################################

HAZARD_FOOTPRINTS_name <- "EVENT_SET_100_yrs_SEED_123_HAZARD_FOOTPRINTS"
this_HAZARD_FOOTPRINTS <- fread(paste0(input_file_loc, "/EVENTSET/", HAZARD_FOOTPRINTS_name, ".csv"))

LANDFIRE_shp_name <- "fuel_models_longlat_100agg_in_study_area_10km_buffer_shp_PREP_4_fire_spread_model"
this_LANDFIRE_shp <- readOGR(paste0(input_file_loc, "/shp/"), layer=LANDFIRE_shp_name)

names(this_LANDFIRE_shp)[names(this_LANDFIRE_shp) == "grd_lng"] <- "grid_length"

##### PARAMS ############################
#########################################

this_study_area_radius <- 100 # km
this_centroid_study_area <- c(-120.06, 36.03) # lon/lat

##### VISUALISE BURN SIMULATION #########
#########################################
this_LANDFIRE_plot <- this_LANDFIRE_shp
this_grid_sim <- this_HAZARD_FOOTPRINTS[this_HAZARD_FOOTPRINTS$FIRE_SIZE_grid_sq==max(this_HAZARD_FOOTPRINTS$FIRE_SIZE_grid_sq),] 

max(this_grid_sim$burn_delay)

take_burn_sim_snapshot <- function(grid, time){
  grid[(grid$burn_delay < time &
          grid$burn_delay != 0),] 
}  

this_snapshot_times <- matrix(1:9*rep(round(max(this_grid_sim$burn_delay),-4)/9, 9))

this_grid_sim_snapshots <- apply(this_snapshot_times, 1, function(x) take_burn_sim_snapshot(this_grid_sim, x))

fire_pal <- colorNumeric(c("blue", "red"), c(TRUE, FALSE))
fire_map_list <- list()
for (i in 1:length(this_grid_sim_snapshots)){
  
  print(i)
  this_LANDFIRE_plot$on_fire <- this_LANDFIRE_plot$locnum %in% this_grid_sim_snapshots[[i]]$locnum
  shp_df <- broom::tidy(this_LANDFIRE_plot, region = "on_fire")
  fire_map_list[[i]] <- ggplot() + 
    geom_polygon(data = shp_df, aes(x = long, y = lat, group = group, fill = id), colour = "black")
  
}

plot_grid(plotlist = fire_map_list)

##### BURN SIMULATION HEAT MAP #########
########################################
this_LANDFIRE_shp_map <- this_LANDFIRE_shp

this_grid_n_times_burned <- this_HAZARD_FOOTPRINTS %>%
  group_by(locnum) %>%
  summarise(
    n_times_burned = n()
  )

this_LANDFIRE_shp_map <- sp::merge(this_LANDFIRE_shp, this_grid_n_times_burned, by="locnum")

burn_pal <- colorQuantile(
  palette=c("lightgreen", "green", "yellow", "orange", "red", "brown"),
  domain=this_LANDFIRE_shp_map@data[!is.na(this_LANDFIRE_shp_map$n_times_burned), "n_times_burned"],
  n=6
)

burn_pal <- colorNumeric(
  palette = c("lightgreen", "green", "yellow", "orange", "red", "brown"),
  domain=1:6
)

leaflet(this_LANDFIRE_shp_map[!is.na(this_LANDFIRE_shp_map$n_times_burned), "n_times_burned"]) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addProviderTiles("Stamen.TonerLabels") %>%
  addPolygons(fillOpacity=0.8, opacity=0,
              fillColor=~burn_pal(n_times_burned), color="black") %>%
  addCircles(lng=this_centroid_study_area[1], lat=this_centroid_study_area[2],
             radius=this_study_area_radius*1000, color="blue", opacity = 1, fillOpacity=0) %>%
  addLegend(
    pal = burn_pal, title = "Number of times burned by very large fires (100 sim years)", values = 1:6,
    opacity = 1, position="bottomleft"
  ) %>%
  addLegend(
    colors = "blue", title = "Study Area", labels = "study area",
    opacity = 1, position="bottomleft"
  )


