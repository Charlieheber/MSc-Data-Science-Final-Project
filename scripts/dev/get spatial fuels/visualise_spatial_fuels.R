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

# LAND TYPE 
this_LANDFIRE_raw_shp <- readOGR(paste0(input_file_loc, "/shp/"),
                                 layer="fuel_models_longlat_100agg_in_study_area_10km_buffer_shp")
this_LANDFIRE_raw_shp@data
this_LANDFIRE_shp <- this_LANDFIRE_raw_shp[which(as.logical(this_LANDFIRE_raw_shp$in_std_)),]

# LANDFIRE key
landtype_key <- fread(paste0(input_file_loc, "/LANDFIRE/LF2022_FBFM13_220_CONUS/CSV_Data/LF20_F13_220.csv"))
landtype_key$Value <- as.character(landtype_key$Value)

this_LANDFIRE_shp <- sp::merge(this_LANDFIRE_shp, landtype_key[, c("Value", "FBFM13", "typical_fuel_complex")], by.x="LC22_F1", by.y="Value")

#### VISUALISE FUELS ######################################
###########################################################

this_study_area_radius <- 100 # km
this_centroid_study_area <- c(-120.06, 36.03) # lon/lat


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


leaflet(this_LANDFIRE_shp) %>%
  addTiles(group="map") %>%
  addProviderTiles("Esri.WorldImagery", group="satellite") %>%
  addProviderTiles("Stamen.TonerLabels", group="satellite") %>%
  addPolygons(fillOpacity=0.8, opacity=0,
              fillColor=~fuel_pal(typical_fuel_complex), color="black",
              label=~typical_fuel_complex, group = "land type") %>%
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
    baseGroups = c("map", "satellite"), overlayGroups = c("study area", "land type")
  )
  


