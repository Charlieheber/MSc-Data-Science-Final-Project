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
this_LANDFIRE_shp <- readOGR(paste0(input_file_loc, "/shp/"),
                                 layer="fuel_models_longlat_100agg_in_study_area_10km_buffer_shp_PREP_4_fire_spread_model")

names(this_LANDFIRE_shp)[names(this_LANDFIRE_shp) == "grd_lng"] <- "grid_length"

# LANDFIRE key
landtype_key <- fread(paste0(input_file_loc, "/LANDFIRE/LF2022_FBFM13_220_CONUS/CSV_Data/LF20_F13_220.csv"))
landtype_key$Value <- as.character(landtype_key$Value)

this_LANDFIRE_shp <- sp::merge(this_LANDFIRE_shp, landtype_key[, c("FBFM13", "typical_fuel_complex")], by="FBFM13")


#### PARAMS ############################
########################################

n_properties = 100
property_TIV_Mn = 1000000

this_study_area_radius <- 100 # km
this_centroid_study_area <- c(-120.06, 36.03) # lon/lat


### GENERATE PTF #######################
########################################
set.seed(123)

# only want urban areas
this_LANDFIRE_urban <- this_LANDFIRE_shp@data[this_LANDFIRE_shp@data$FBFM13=="Urban",]

# randomly pick grid squares for 100 locations
prop_grid_sqs_locnums <-  data.frame("locnum"=sample(this_LANDFIRE_urban$locnum, 100, replace=TRUE))
prop_grid_sqs <- left_join(prop_grid_sqs_locnums, this_LANDFIRE_urban[,c("locnum", "lon", "lat")], by="locnum")

# jitter a bit
jittered_prop_grid_sqs <- prop_grid_sqs
jittered_prop_grid_sqs$lon <- jitter(prop_grid_sqs$lon, 2)
jittered_prop_grid_sqs$lat <- jitter(prop_grid_sqs$lat, 2)

jittered_prop_grid_sqs$LOCID <- 1:n_properties
jittered_prop_grid_sqs$TIV <- property_TIV_Mn

this_EDM <- jittered_prop_grid_sqs

### VISUALISE #########################
#######################################

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

this_LANDFIRE_shp@data
leaflet(this_LANDFIRE_shp) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addProviderTiles("Stamen.TonerLabels") %>%
  addPolygons(fillOpacity=0.8, opacity=0,
              fillColor=~fuel_pal(typical_fuel_complex), color="black",
              label=~typical_fuel_complex) %>%
  addCircleMarkers(data=jittered_prop_grid_sqs,
                   lng=~lon, lat=~lat, color="black", fillColor="red",
                   radius=5, opacity=1, fillOpacity=1, weight=2, stroke=TRUE) %>%
  addCircles(lng=this_centroid_study_area[1], lat=this_centroid_study_area[2],
             radius=this_study_area_radius*1000, color="blue", opacity = 1, fillOpacity=0) %>%
  addLegend(
    pal = fuel_pal, title = "Fueltypes in study area", values = names(colours),
    opacity = 1, position="bottomleft"
  ) %>%
  addLegend(
    colors = "blue", title = "Study Area", labels = "study area",
    opacity = 1, position="bottomleft"
  ) %>%
  addLegend(
    colors = "red", title = "Locations in Dummy Portfolio", labels = "locations",
    opacity = 1, position="bottomleft"
  )


### SAVE RESULT ######################
######################################

write.csv(this_EDM[,c("locnum", "LOCID", "lon","lat", "TIV")],
          paste0(output_file_loc, "/EDM_", n_properties, "properties_", property_TIV_Mn, "Mn_TIV.csv"),
          row.names=FALSE)




