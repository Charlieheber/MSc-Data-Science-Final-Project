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
LANDFIRE_shp_name <- "fuel_models_longlat_100agg_in_study_area_10km_buffer_shp"
this_LANDFIRE_raw_shp <- readOGR(paste0(input_file_loc, "/shp/"),
                                 layer=LANDFIRE_shp_name)
this_LANDFIRE_raw_shp@data
this_LANDFIRE_shp <- this_LANDFIRE_raw_shp[which(as.logical(this_LANDFIRE_raw_shp$in_std_)),]
this_LANDFIRE_raw_shp <- NULL

# LANDFIRE key
landtype_key <- fread(paste0(input_file_loc, "/LANDFIRE/LF2022_FBFM13_220_CONUS/CSV_Data/LF20_F13_220.csv"))
landtype_key$Value <- as.character(landtype_key$Value)

this_LANDFIRE_shp <- sp::merge(this_LANDFIRE_shp, landtype_key[, c("Value", "FBFM13", "typical_fuel_complex")], by.x="LC22_F1", by.y="Value")

##### PREP LANDFIRE FOR NN FIRE MODEL ######################
############################################################
this_LANDFIRE_prep_shp <- this_LANDFIRE_shp

lon_colNum <- data.frame(
  "lon" = sort(unique(round(this_LANDFIRE_prep_shp@data$lon,3))),
  "colNum" = 1:length(unique(round(this_LANDFIRE_prep_shp@data$lon,3)))
)
lat_rowNum <- data.frame(
  "lat" = sort(unique(round(this_LANDFIRE_prep_shp@data$lat,3)), decreasing = TRUE),
  "rowNum" = 1:length(unique(round(this_LANDFIRE_prep_shp@data$lat,3)))
)

this_LANDFIRE_prep_shp$lat <- round(this_LANDFIRE_prep_shp$lat,3)
this_LANDFIRE_prep_shp$lon <- round(this_LANDFIRE_prep_shp$lon,3)

this_LANDFIRE_prep_shp <- sp::merge(this_LANDFIRE_prep_shp, lon_colNum) 
this_LANDFIRE_prep_shp <- sp::merge(this_LANDFIRE_prep_shp, lat_rowNum) 

leaflet(this_LANDFIRE_prep_shp) %>%
  addPolygons(popup=~paste("RowNum:", rowNum, "ColNum:", colNum, "<br>",
                           "lon:", round(lon,3), "lat:", round(lat,3)))

this_LANDFIRE_prep_shp@data$grid_length <- 3000
this_LANDFIRE_prep_shp@data$locnum <- 1:length(this_LANDFIRE_prep_shp)

this_LANDFIRE_prep_shp <- this_LANDFIRE_prep_shp[, c("locnum", "lon", "lat", 
                                                     "colNum", "rowNum",
                                                     "FBFM13", "grid_length")]
# save result
writeOGR(obj=this_LANDFIRE_prep_shp,
         dsn=output_file_loc, 
         layer=paste0(LANDFIRE_shp_name, "_PREP_4_fire_spread_model"),
         driver="ESRI Shapefile")
