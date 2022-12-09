rm(list=ls())
library(here)

##### LOAD PACKAGES ######################
##########################################

script_loc <- paste0(here::here(), "/scripts/R/")
req_packages = c("general",  "visualisation", "spatial", "statistical")
source(paste0(script_loc, "libraries_and_file_locs.R"))

# FUNCTIONS ########################
####################################

get_lognormal_fire_size_r_vars <- function(ave_fire_size, sd_fire_size, n, landtype){
  
  location <- log(ave_fire_size^2/sqrt(sd_fire_size^2 + ave_fire_size^2))
  shape <- sqrt(log(1 +  (sd_fire_size^2/ave_fire_size^2)))
  
  # generate random variables
  this_fire_size_LN_pred_fit <- data.frame(
    "FIRE_SIZE" = rlnormTrunc(n, 
                              mean=location, 
                              sd=shape, 
                              min=0),
    "FBFM13" = landtype,
    "source" = "ln-dist")
  
  return(this_fire_size_LN_pred_fit)
  
}
get_lognormal_fire_size_r_vars_wrapper <- function(agg_fire_size_dat_dist_fit){
  
  # generate lognormal dist random values for each landtype 
  this_fire_size_LN_pred_fit_lst <- apply(this_agg_fire_size_dat_dist_fit, 1, 
                                          function(x) get_lognormal_fire_size_r_vars(
                                            as.numeric(x[["ave_fire_size"]]), 
                                            as.numeric(x[["sd_fire_size"]]), 
                                            500, 
                                            x[["FBFM13"]]))
  this_fire_size_LN_pred_fit <- rbindlist(this_fire_size_LN_pred_fit_lst) 
  
  return(agg_fire_size_dat_dist_fit)
  
}

##### GET INPUT DATA ####################
#########################################

this_input_file_loc <- paste0(input_file_loc, "/hazard data/historical wildfires/")
this_output_file_loc <- paste0(output_file_loc, "/")

# HISTORICAL FIRE SIZES
this_hist_wildfire_DB <- DBI::dbConnect(RSQLite::SQLite(), paste0(this_input_file_loc, "US_WildfireRecords.sqlite"))

this_fire_dat <- data.table(dbGetQuery(this_hist_wildfire_DB, paste(
                            "SELECT FIRE_SIZE, LATITUDE, LONGITUDE 
                             FROM Fires WHERE 
                             LATITUDE > 34 AND LATITUDE < 38 AND
                             LONGITUDE > -122 AND LONGITUDE < -117"))
                                  )

odbc::dbDisconnect(this_hist_wildfire_DB)

# LANDFIRE SHAPEFILE
this_land_shape <- readOGR(paste0(input_file_loc, "/shp/"),  "fuel_models_longlat_100agg_in_study_area_10km_buffer_shp")
head(this_land_shape@data)
this_land_shape$OBJECTID <- 1:dim(this_land_shape@data)[1]

# get land area of each grid square
this_land_shape$land_area_km_2 <- raster::area(this_land_shape)/1000000

# LANDFIRE key
landtype_key <- fread(paste0(input_file_loc, "/LANDFIRE/LF2022_FBFM13_220_CONUS/CSV_Data/LF20_F13_220.csv"))
landtype_key$Value <- as.character(landtype_key$Value)

##### GET LAND TYPE FOR EACH FIRE IGNITION POINT #############
##############################################################

# create fire shapefile
this_fire_shp <- SpatialPointsDataFrame(coords=this_fire_dat[,c("LONGITUDE", "LATITUDE")], 
                                        proj4string=this_land_shape@proj4string,
                                        data=this_fire_dat[,c("LATITUDE", "LONGITUDE")])

# get intersect lst between fires and land type
this_fire_landtype_intersect <- rgeos::gIntersects(this_fire_shp, this_land_shape, byid=TRUE, returnDense=FALSE)
head(this_fire_landtype_intersect)

# add land type data onto fire data
this_land_row_indx_lst <- lapply(this_fire_landtype_intersect, function(x){ 
                                        if(is.null(x)){
                                          return(data.table("this_land_shape_row_indx" = NA))
                                        } else{
                                          return(data.table("this_land_shape_row_indx" = x))
                                        }
                                 })

this_fire_dat$this_land_shape_row_indx <- rbindlist(this_land_row_indx_lst)

# only take fires w/ known landtype and above 10 acres
this_fire_dat <- this_fire_dat[!is.na(this_fire_dat$this_land_shape_row_indx),]
this_fire_dat <- this_fire_dat[this_fire_dat$FIRE_SIZE >= 10,]

this_fire_dat_w_landtype <- this_fire_dat %>% left_join(this_land_shape@data[,c("OBJECTID", "lon", "lat", "LC22_F1")], by=c("this_land_shape_row_indx"="OBJECTID"), keep=FALSE) 

# add key
this_fire_dat_w_landtype <- this_fire_dat_w_landtype %>% left_join(landtype_key[,c("Value", "FBFM13")], by=c("LC22_F1"="Value"), keep=FALSE)

this_fire_dat_w_landtype %>%
  group_by(FBFM13) %>%
  summarise(
    n_fires = n(),
    ave_fire_size = mean(FIRE_SIZE, na.rm=TRUE),
    sd_fire_size = sd(FIRE_SIZE, na.rm=TRUE),
    max_fire_size = max(FIRE_SIZE, na.rm=TRUE),
    min_fire_size = min(FIRE_SIZE, na.rm=TRUE)
  )

#### LANDTYPE AGAINST FIRE SIZE ############################
############################################################

# DENSITY PLOT
ggplot(this_fire_dat_w_landtype, 
       aes(x=FIRE_SIZE, y=FBFM13, fill=FBFM13)) +
  geom_density_ridges() + xlim(1,100)

#### BUILD LOGNORMAL/POWER DISTS ##################################
###################################################################
this_fire_size_dat_dist_fit <- this_fire_dat_w_landtype[,c("FIRE_SIZE", "FBFM13")]

this_all_fire_size_dat_dist_fit <- this_fire_size_dat_dist_fit
this_all_fire_size_dat_dist_fit$FBFM13 <- "all fires"

# dataframe split by landtype inc 'all fires'
this_fire_size_dat_dist <- rbind(this_fire_size_dat_dist_fit, this_all_fire_size_dat_dist_fit)
this_fire_size_dat_dist$source <- "actual"

# agg by landtype
this_agg_fire_size_dat_dist_fit <- this_fire_size_dat_dist %>%
  group_by(FBFM13) %>%
  summarise(
    n_fires = n(),
    ave_fire_size = mean(FIRE_SIZE, na.rm=TRUE),
    sd_fire_size = sd(FIRE_SIZE, na.rm=TRUE),
    max_fire_size = max(FIRE_SIZE, na.rm=TRUE),
    min_fire_size = min(FIRE_SIZE, na.rm=TRUE)
  )

# generate lognormal dist random values for each landtype 
this_fire_size_LN_pred_fit_lst <- apply(this_agg_fire_size_dat_dist_fit, 1, 
                                        function(x) get_lognormal_fire_size_r_vars(
                                          as.numeric(x[["ave_fire_size"]]), 
                                          as.numeric(x[["sd_fire_size"]]), 
                                          500, 
                                          x[["FBFM13"]]))
this_fire_size_LN_pred_fit <- rbindlist(this_fire_size_LN_pred_fit_lst) 

this_fire_size_dat_dist_w_pred <- rbindlist(list(this_fire_size_dat_dist, this_fire_size_LN_pred_fit))

###### SUMMARISE/VIUALISE #####################
###############################################

ggplot(this_fire_size_dat_dist_w_pred[this_fire_size_dat_dist_w_pred$FBFM13 == "all fires"], 
       aes(x=FIRE_SIZE, y=source, fill=source)) +
  xlim(0,1000) +
  geom_density_ridges()

this_agg_fire_size_dat_dist_fit <- this_fire_size_dat_dist_w_pred %>%
  group_by(source, FBFM13) %>%
  summarise(
    "min_size" = min(FIRE_SIZE),
    "max_size" = max(FIRE_SIZE),
    "mean_size" = mean(FIRE_SIZE),
    "sd_size" = sd(FIRE_SIZE)
  )

#### SAVE RESULT ##############################
###############################################

write.csv(this_fire_size_dat_dist, paste0(output_file_loc, "/wildfire simulation model/fire_sizes_all_years_by_landtype.csv"), row.names=FALSE)
write.csv(this_agg_fire_size_dat_dist_fit, paste0(output_file_loc, "/wildfire simulation model/agg_fire_sizes_all_years_by_landtype.csv"), row.names=FALSE)
