rm(list=ls())
library(here)

##### LOAD PACKAGES ######################
##########################################

script_loc <- paste0(here::here(), "/scripts/R/")
req_packages = c("general",  "visualisation", "spatial")
source(paste0(script_loc, "libraries_and_file_locs.R"))

#### PARAMS #############################
#########################################

this_study_area_lon_lat <- c(-120.06, 36.03)
this_study_area_radius_km <- 50

# study area
this_lon_extent <- c(-122, -117)
this_lat_extent <- c(34, 38)

#### GET LANDFIRE SPATIAL FUELS DATA ####
#########################################

CONUS_fuel_models <- raster(paste0(input_file_loc, "/LANDFIRE/LF2022_FBFM13_220_CONUS/Tif/LC22_F13_220.tif"))
CONUS_fuel_models

this_ref_lon <- 96
this_ref_lat <- 23
this_radius <- 6378137

this_standard_parallel_1 <- 29.5
this_standard_parallel_2 <- 45.5

this_aggregation_factor <- 10
this_study_area_buffer <- 0 # km 

# output shape filename
output_name <- paste0("fuel_models_longlat_", this_aggregation_factor, "x_agg_in_", this_study_area_radius_km, "_study_area_10km_buffer_shp")

#### CONVERT FROM LAT/LON TO ALBERS EQUAL AREA ########
#######################################################

# ref = 'map projections - a working manual"
convert_latlon_to_AEA_coords <- function(lat_deg, lon_deg, ref_lat_deg, ref_lon_deg, standard_parallel_1_deg, standard_parallel_2_deg, radius){
  
  degrees_to_rads <- function(degrees){
    
    return((degrees/180)*pi)
    
  }
  
  # radius <- 1
  # standard_parallel_1_deg <- 29.5
  # standard_parallel_2_deg <- 45.5
  # ref_lat_deg <- 23
  # ref_lon_deg <- 96
  # lat_deg <- 34
  # lon_deg <- 122
  
  lat <- degrees_to_rads(lat_deg)
  lon <- degrees_to_rads(lon_deg)
  
  ref_lat <- degrees_to_rads(ref_lat_deg)
  ref_lon <- degrees_to_rads(ref_lon_deg)
  
  standard_parallel_1 <- degrees_to_rads(standard_parallel_1_deg)
  standard_parallel_2 <- degrees_to_rads(standard_parallel_2_deg)
  
  
  n <- 0.5*(sin(standard_parallel_1) + sin(standard_parallel_2))
  theta <- n*(lon - ref_lon)
  
  C <- cos(standard_parallel_1)^2 + 2*n*sin(standard_parallel_1)
  
  rho <- (radius/n)*sqrt(C - 2*n*sin(lat))
  rho_0 <- (radius/n)*sqrt(C - 2*n*sin(ref_lat))
  
  x <- rho*sin(theta)
  y <- rho_0 - rho*cos(theta)
  
  x*this_radius
  y*this_radius
  
  return(c(x, y))
}

study_area_coords <- convert_latlon_to_AEA_coords(34, 122, this_ref_lat, this_ref_lon, this_standard_parallel_1, this_standard_parallel_2, this_radius)
study_area_coords2 <- convert_latlon_to_AEA_coords(38, 117, this_ref_lat, this_ref_lon, this_standard_parallel_1, this_standard_parallel_2, this_radius)

#### CROP/PROJECT INTO LAT/LON ###############
##############################################

CONUS_fuel_models_crop <- raster::crop(CONUS_fuel_models,
                                       extent(matrix(c(-study_area_coords[1], study_area_coords[2], -study_area_coords2[1], study_area_coords2[2]), nrow=2)))

CONUS_fuel_models_longlat <- projectRaster(CONUS_fuel_models_crop, crs = crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


# CONUS_fuel_models_longlat_agg <- raster::aggregate(CONUS_fuel_models_longlat, 100, mean)

### AGGREGATE (TAKE MODE OF EACH AREA SO GET MOST COMMON FUEL MODEL)
####################################################################

CONUS_fuel_models_longlat_mode_agg <- raster::aggregate(CONUS_fuel_models_longlat, this_aggregation_factor, fun=modal)

### TURN TO SHAPEFILE #############################################
###################################################################

CONUS_fuel_models_longlat_agg_shp <- as(CONUS_fuel_models_longlat_mode_agg, "SpatialPolygonsDataFrame")
head(CONUS_fuel_models_longlat_agg_shp@data)

unique(as.factor(CONUS_fuel_models_longlat_agg_shp$LC22_F13_220))
CONUS_fuel_models_longlat_agg_shp$LC22_F13_220 <- as.factor(CONUS_fuel_models_longlat_agg_shp$LC22_F13_220)

# add lat/lons to spdf
CONUS_fuel_models_longlat_agg_shp@data <- data.frame("lon"=coordinates(CONUS_fuel_models_longlat_agg_shp)[,1], 
                                                     "lat"=coordinates(CONUS_fuel_models_longlat_agg_shp)[,2],
                                                     "LC22_F13_220"=CONUS_fuel_models_longlat_agg_shp@data$LC22_F13_220)

### CALCULATE HAVERSINE DIST AND FIND POLYS IN STUDY AREA ########
##################################################################
head(CONUS_fuel_models_longlat_agg_shp@data)

CONUS_fuel_models_longlat_agg_shp@data$dist_from_study_area <-  geosphere::distHaversine(this_study_area_lon_lat, CONUS_fuel_models_longlat_agg_shp@data[,c("lon", "lat")])/1000
CONUS_fuel_models_longlat_agg_shp@data$in_study_area <- CONUS_fuel_models_longlat_agg_shp@data$dist_from_study_area < (this_study_area_radius_km + this_study_area_buffer)




LC22_pal <- colorFactor(topo.colors(13), CONUS_fuel_models_longlat_agg_shp$LC22_F13_220)

### MAP #########################################################
#################################################################

leaflet(CONUS_fuel_models_longlat_agg_shp[CONUS_fuel_models_longlat_agg_shp$in_study_area,]) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addProviderTiles("Stamen.TonerLabels") %>%
  addPolygons(fillOpacity=1, opacity=1,
              fillColor=~LC22_pal(LC22_F13_220), color="black")
  # addLegend(
  #   "bottomleft", colors=fire_size_col, labels=fire_size_labels, title=fire_size_title, 
  #   opacity=1
  # )

### SAVE RESULT #################################################
#################################################################

writeOGR(CONUS_fuel_models_longlat_agg_shp, paste0(output_file_loc, "/shp"), layer=output_name, driver = "ESRI Shapefile")

