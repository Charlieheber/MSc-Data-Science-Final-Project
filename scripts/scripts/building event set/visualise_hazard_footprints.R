rm(list=ls())
library(here)

##### LOAD PACKAGES ######################
##########################################

script_loc <- paste0(here::here(), "/scripts/R/")
req_packages = c("general",  "visualisation", "spatial")
source(paste0(script_loc, "libraries_and_file_locs.R"))

##### PARAMS ############################
#########################################
acres_to_km_sq <- 0.00404686
grid_sq_size_km_sq <- 9

this_study_area_radius <- 100 # km
this_centroid_study_area <- c(-120.06, 36.03) # lon/lat

##### GET DATA ##########################
#########################################

run <- 1:10
HAZARD_FOOTPRINTS_name <- paste0("EVENT_SET_1000_yrs_SEED_123_run_", run,"_HAZARD_FOOTPRINTS")
EVENTSET_name <- paste0("EVENT_SET_1000_yrs_SEED_123_run_", run)

# EVENTSET
this_EVENTSET_lst <- lapply(EVENTSET_name,
                            function(x){
                              res <-fread(paste0(input_file_loc, "/EVENTSET/", x, ".csv"))
                              return(res[which(res$FIRE_SIZE > grid_sq_size_km_sq/acres_to_km_sq),])
                            } 
  
)
# HAZARD FOOTPRINT
this_HAZARD_FOOTPRINTS_lst <- lapply(HAZARD_FOOTPRINTS_name,
                                     function(x) fread(paste0(input_file_loc, "/EVENTSET/hazard footprints/", x,  ".csv"))
)

this_EVENTSET <- rbindlist(this_EVENTSET_lst)
this_HAZARD_FOOTPRINTS <- rbindlist(this_HAZARD_FOOTPRINTS_lst)

#  LANDFIRE SHP
LANDFIRE_shp_name <- "fuel_models_longlat_100agg_in_study_area_10km_buffer_shp_PREP_4_fire_spread_model"
this_LANDFIRE_shp <- readOGR(paste0(input_file_loc, "/shp/"), layer=LANDFIRE_shp_name)

names(this_LANDFIRE_shp)[names(this_LANDFIRE_shp) == "grd_lng"] <- "grid_length"

# LANDFIRE key
landtype_key <- fread(paste0(input_file_loc, "/LANDFIRE/LF2022_FBFM13_220_CONUS/CSV_Data/LF20_F13_220.csv"))
landtype_key$Value <- as.character(landtype_key$Value)

this_LANDFIRE_shp <- sp::merge(this_LANDFIRE_shp, landtype_key[, c("FBFM13", "typical_fuel_complex")], by="FBFM13")

# join fuel type data
this_HAZARD_FOOTPRINTS <- left_join(this_HAZARD_FOOTPRINTS, this_LANDFIRE_shp@data[,c("locnum", "FBFM13")], by="locnum")



##### VISUALISE BURN SIMULATION #########
#########################################
this_LANDFIRE_plot <- this_LANDFIRE_shp

# visualise biggest fire and fire with most fuel types
this_grid_sim <- this_HAZARD_FOOTPRINTS[this_HAZARD_FOOTPRINTS$FIRE_SIZE_grid_sq==max(this_HAZARD_FOOTPRINTS$FIRE_SIZE_grid_sq),] 

take_burn_sim_snapshot <- function(grid, time){
  grid[(grid$burn_delay < time &
          grid$burn_delay != 0),] 
}  


this_snapshot_times <- matrix(round(stats::quantile(this_grid_sim$burn_delay,probs=seq(0,1, by=1/11))))

this_grid_sim_snapshots <- apply(this_snapshot_times, 1, function(x) take_burn_sim_snapshot(this_grid_sim, x))

fire_pal <- colorNumeric(c("blue", "red"), c(TRUE, FALSE))
fire_map_list <- list()
for (i in 1:length(this_grid_sim_snapshots)){
  
  print(i)
  this_LANDFIRE_plot$on_fire <- this_LANDFIRE_plot$locnum %in% this_grid_sim_snapshots[[i]]$locnum
  shp_df <- broom::tidy(this_LANDFIRE_plot, region = "on_fire")
  fire_map_list[[i]] <- ggplot() + 
    geom_polygon(data = shp_df, aes(x = long, y = lat, group = group, fill = id), colour = "black") +
    theme(legend.position =  "none",
          text =element_blank())
  
  
}

plot_grid(plotlist = fire_map_list)

##### VISUALISE FIRE WITH FUEL TYPE ####
########################################
# which fire has most different fuel types?
n_fueltypes <- this_HAZARD_FOOTPRINTS %>%
  group_by(EVENTID) %>%
  summarise(
    "n_FBFM13" = length(unique(FBFM13))
  )
n_fueltypes[which(n_fueltypes$n_FBFM13 %in% max(n_fueltypes$n_FBFM13)),]

this_grid_sim <- this_HAZARD_FOOTPRINTS[this_HAZARD_FOOTPRINTS$EVENTID == 19707,] 
this_grid_snapshots <- stats::quantile(this_grid_sim$burn_delay,probs=seq(0,1, by=1/6))

this_LANDFIRE_shp_fuel_map <- sp::merge(this_LANDFIRE_shp, this_grid_sim[, c("locnum", "burning", "burn_delay")], by="locnum")

# fuel type key
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

this_LANDFIRE_shp_fuel_map@data
leaflet(this_LANDFIRE_shp_fuel_map) %>%
  addTiles(group="map") %>%
  addProviderTiles("Esri.WorldImagery", group="satellite") %>%
  addProviderTiles("Stamen.TonerLabels", group="satellite") %>%
  addPolygons(fillOpacity=0.8, opacity=0,
              fillColor=~fuel_pal(typical_fuel_complex), color="black",
              label=~typical_fuel_complex, group = "land type") %>%
  addPolygons(data=this_LANDFIRE_shp_fuel_map[which(this_LANDFIRE_shp_fuel_map$burning &
                                                      this_LANDFIRE_shp_fuel_map$burn_delay < this_grid_snapshots[2]/2),],
              fillOpacity=0.8, opacity=0,
              fillColor="red", color="black",
              group = "fire_0") %>%
  addPolygons(data=this_LANDFIRE_shp_fuel_map[which(this_LANDFIRE_shp_fuel_map$burning &
                                                      this_LANDFIRE_shp_fuel_map$burn_delay < this_grid_snapshots[2]),],
              fillOpacity=0.8, opacity=0,
              fillColor="red", color="black",
              group = "fire_1") %>%
  addPolygons(data=this_LANDFIRE_shp_fuel_map[which(this_LANDFIRE_shp_fuel_map$burning &
                                                      this_LANDFIRE_shp_fuel_map$burn_delay < this_grid_snapshots[3]),],
              fillOpacity=0.8, opacity=0,
              fillColor="red", color="black",
              group = "fire_2") %>%
  addPolygons(data=this_LANDFIRE_shp_fuel_map[which(this_LANDFIRE_shp_fuel_map$burning &
                                                      this_LANDFIRE_shp_fuel_map$burn_delay < this_grid_snapshots[4]),],
              fillOpacity=0.8, opacity=0,
              fillColor="red", color="black",
              group = "fire_3") %>%
  addPolygons(data=this_LANDFIRE_shp_fuel_map[which(this_LANDFIRE_shp_fuel_map$burning &
                                                      this_LANDFIRE_shp_fuel_map$burn_delay < this_grid_snapshots[5]),],
              fillOpacity=0.8, opacity=0,
              fillColor="red", color="black",
              group = "fire_4") %>%
  addPolygons(data=this_LANDFIRE_shp_fuel_map[which(this_LANDFIRE_shp_fuel_map$burning &
                                                      this_LANDFIRE_shp_fuel_map$burn_delay < this_grid_snapshots[6]),],
              fillOpacity=0.8, opacity=0,
              fillColor="red", color="black",
              group = "fire_5") %>%
  addPolygons(data=this_LANDFIRE_shp_fuel_map[which(this_LANDFIRE_shp_fuel_map$burning &
                                                      this_LANDFIRE_shp_fuel_map$burn_delay < this_grid_snapshots[7]),],
              fillOpacity=0.8, opacity=0,
              fillColor="red", color="black",
              group = "fire_6") %>%
  addLegend(
    pal = fuel_pal, title = "Fueltypes in study area", values = names(colours),
    opacity = 1
  ) %>%
  addCircles(lng=this_centroid_study_area[1], lat=this_centroid_study_area[2],
             radius=this_study_area_radius*1000, color="blue", opacity = 1, fillOpacity=0,
             group = "study area") %>%
  addLegend(
    colors = "blue", title = "Study Area", labels = "study area",
    opacity = 1, position="bottomleft"
  ) %>%
  addLayersControl(
    baseGroups = c("map", "satellite"), overlayGroups = c("study area", "land type", "fire_0", "fire_1",
                                                          "fire_2", "fire_3", "fire_4", "fire_5", "fire_6")
  )


##### VISUALISE A YEAR OF FIRES ########
########################################
this_sim_year <- 1

this_year_EVENTIDs <- this_EVENTSET[sim_year == this_sim_year, "EVENTID"]
this_sim_year_HAZARD_FOOTPRINTS <- this_HAZARD_FOOTPRINTS[which(this_HAZARD_FOOTPRINTS$EVENTID %in% this_year_EVENTIDs[[1]]),]

this_grid_n_times_yr_burned <- this_sim_year_HAZARD_FOOTPRINTS %>%
  group_by(locnum) %>%
  summarise(
    n_times_burned = n()
  )


this_LANDFIRE_shp_yr_map <- sp::merge(this_LANDFIRE_shp, this_grid_n_times_yr_burned, by="locnum")
this_LANDFIRE_shp_yr_map@data

leaflet(this_LANDFIRE_shp_yr_map[!is.na(this_LANDFIRE_shp_yr_map$n_times_burned),]) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addProviderTiles("Stamen.TonerLabels") %>%
  addPolygons(fillOpacity=0.8, opacity=0,
              color="black", fillColor = "red") %>%
  addCircles(lng=this_centroid_study_area[1], lat=this_centroid_study_area[2],
             radius=this_study_area_radius*1000, color="blue", opacity = 1, fillOpacity=0) %>%
  addLegend(
    colors = "red", title = paste("Area burned by large fire (sim year", this_sim_year ,")"), labels="area burned",
    opacity = 1, position="bottomleft"
  ) %>%
  addLegend(
    colors = "blue", title = "Study Area", labels = "study area",
    opacity = 1, position="bottomleft"
  )


##### BURN SIMULATION HEAT MAP #########
########################################
this_LANDFIRE_shp_map <- this_LANDFIRE_shp
this_LANDFIRE_shp_map@data

this_grid_n_times_burned <- this_HAZARD_FOOTPRINTS %>%
  group_by(locnum) %>%
  summarise(
    n_times_burned = n()
  )
max(this_grid_n_times_burned$n_times_burned)

this_LANDFIRE_shp_map <- sp::merge(this_LANDFIRE_shp, this_grid_n_times_burned, by="locnum")

burn_pal <- colorQuantile(
  palette=c("grey", "green", "yellow", "orange", "red", "brown"),
  domain=this_LANDFIRE_shp_map@data[!is.na(this_LANDFIRE_shp_map$n_times_burned), "n_times_burned"],
  n=6
)

leaflet(this_LANDFIRE_shp_map[!is.na(this_LANDFIRE_shp_map$n_times_burned), "n_times_burned"]) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addProviderTiles("Stamen.TonerLabels") %>%
  addPolygons(fillOpacity=0.8, opacity=0,
              fillColor=~burn_pal(n_times_burned), color="black") %>%
  addCircles(lng=this_centroid_study_area[1], lat=this_centroid_study_area[2],
             radius=this_study_area_radius*1000, color="blue", opacity = 1, fillOpacity=0) %>%
  addLegend(
    pal = burn_pal, title = "Number of times burned by large fires <br> (1000 sim years)", values =~n_times_burned,
    opacity = 1, position="bottomleft"
  ) %>%
  addLegend(
    colors = "blue", title = "Study Area", labels = "study area",
    opacity = 1, position="bottomleft"
  )

##### 

this_HAZARD_FOOTPRINTS$fire_spread_rate
this_HAZARD_FOOTPRINTS <- merge(this_HAZARD_FOOTPRINTS, landtype_key[, c("FBFM13", "typical_fuel_complex")], by="FBFM13")

# Scatter plot by group
ggplot(this_HAZARD_FOOTPRINTS, aes(x=FBFM13, y=fire_spread_rate, color=typical_fuel_complex)) +
      geom_density()    


max(this_EVENTSET$FIRE_SIZE)

ggplot(this_EVENTSET, aes(x=log10(FIRE_SIZE))) +
  geom_density()    








