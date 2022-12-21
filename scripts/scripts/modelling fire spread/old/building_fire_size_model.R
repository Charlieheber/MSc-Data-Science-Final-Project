rm(list=ls())
library(here)

##### LOAD PACKAGES ######################
##########################################

script_loc <- paste0(here::here(), "/scripts/R/")
req_packages = c("general",  "visualisation", "spatial")
source(paste0(script_loc, "libraries_and_file_locs.R"))

# FUNCTIONS ########################
####################################

# fns_loc <- paste0(here::here(), "/scripts/R/fns/")
# source(paste0(fns_loc, ".R"))

##### GET INPUT DATA ####################
#########################################
acres_to_km <- 1/247.1

this_fire_dat <- fread(paste0(input_file_loc, "/wildfire simulation model/historical fire data/kettleman_hils_2000_2015_100_km_fires_w_ERC_fire_igntions.csv"))
this_fire_dat <- this_fire_dat[!(this_fire_dat$FIRE_SIZE_CLASS %in% c("A", "B")),]

this_station_dat <- fread(paste0(input_file_loc, "/wildfire simulation model/station data/station_data_kettleman_hills_00_22.csv"))


this_fire_dat$DOY_id <- paste0(this_fire_dat$FIRE_YEAR, "_", this_fire_dat$DISCOVERY_DOY) 
this_station_dat$DOY_id <- paste0(this_station_dat$year, "_", this_station_dat$day_of_year)
this_fire_dat$FIRE_SIZE_km_2 <- this_fire_dat$FIRE_SIZE * acres_to_km

# LANDFIRE shapefile
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
                                        data=this_fire_dat[,c("OBJECTID", "LATITUDE", "LONGITUDE")])

# get intersect lst between fires and land type
this_fire_landtype_intersect <- rgeos::gIntersects(this_fire_shp, this_land_shape, byid=TRUE, returnDense=FALSE)

# add land type data onto fire data
this_fire_dat$this_land_shape_row_indx <- rbindlist(lapply(this_fire_landtype_intersect, function(x) data.table("this_land_shape_row_indx" = x)))

this_fire_dat_w_landtype <- this_fire_dat %>% left_join(this_land_shape@data[,c("OBJECTID", "lon", "lat", "LC22_F1")], by=c("this_land_shape_row_indx"="OBJECTID"), keep=FALSE) 

# add key
this_fire_dat_w_landtype <- this_fire_dat_w_landtype %>% left_join(landtype_key[,c("Value", "FBFM13")], by=c("LC22_F1"="Value"), keep=FALSE)

this_land_shape@data <- this_land_shape@data %>% left_join(landtype_key[,c("Value", "FBFM13")], by=c("LC22_F1"="Value"), keep=FALSE)

# CALCULATE ERC+WINDSPEED 7 DAY/MONTH INTO FUTURE AVE ########
##############################################################

calculate_ave_given_time_window <- function(vec, starting_point_indx, time_window_days){
  
  mean(vec[starting_point_indx:(starting_point_indx+time_window_days)], na.rm=TRUE)
  
} 

# ERC
this_station_dat$seven_day_ERC_ave <-  apply(as.matrix(1:dim(this_station_dat)[1]), 1, function(x) calculate_ave_given_time_window(this_station_dat[,c("ERC")][[1]], x, 7))
this_station_dat$thirty_day_ERC_ave <-  apply(as.matrix(1:dim(this_station_dat)[1]), 1, function(x) calculate_ave_given_time_window(this_station_dat[,c("ERC")][[1]], x, 30))

# WINDSPEED
this_station_dat$seven_day_windspeed_ave <-  apply(as.matrix(1:dim(this_station_dat)[1]), 1, function(x) calculate_ave_given_time_window(this_station_dat[,c("ave_wind_speed")][[1]], x, 7))
this_station_dat$thirty_day_windspeed_ave <-  apply(as.matrix(1:dim(this_station_dat)[1]), 1, function(x) calculate_ave_given_time_window(this_station_dat[,c("ave_wind_speed")][[1]], x, 30))

# JOIN STATION DATA TO WEATHER DATA ############################
################################################################

this_fire_dat_w_landtype_station_dat <- this_fire_dat_w_landtype %>%  left_join(this_station_dat[,c("DOY_id", "ERC", "ave_wind_speed",
                                                                                                    "seven_day_ERC_ave", "thirty_day_ERC_ave",
                                                                                                    "seven_day_windspeed_ave", "thirty_day_windspeed_ave")], by="DOY_id", drop=TRUE)
#### ERC/WINDSPEED AGAINST FIRE SIZE ##########################
###############################################################

fire_color = "lightcoral"
large_fire_color = "lightsteelblue"
multi_fire_color = RColorBrewer::brewer.pal(3, "Set2")[1]

# ERC
ERC_fire_size_plt <- ggplot(this_fire_dat_w_landtype_station_dat[!(this_fire_dat_w_landtype_station_dat$FIRE_SIZE_CLASS %in%  c("A", "B")),], aes(x=ERC, y=log10(FIRE_SIZE))) +
  geom_jitter(width = 0, height = 0.1, color=fire_color, size=2) +
  labs(title = "Kettleman Hills 2000-2015: ERC on Fire Ignition Days")

# SEVEN DAY AVE ERC
seven_day_ERC_fire_size_plt <- ggplot(this_fire_dat_w_landtype_station_dat[!(this_fire_dat_w_landtype_station_dat$FIRE_SIZE_CLASS %in%  c("A", "B")),], aes(x=seven_day_ERC_ave, y=log10(FIRE_SIZE))) +
  geom_jitter(width = 0, height = 0.1, color=large_fire_color, size=2) +
  labs(title = "Kettleman Hills 2000-2015: ERC on Fire Ignition Days")

# MONTH AVE ERC
thirty_day_ERC_fire_size_plt <- ggplot(this_fire_dat_w_landtype_station_dat[!(this_fire_dat_w_landtype_station_dat$FIRE_SIZE_CLASS %in%  c("A", "B")),], aes(x=thirty_day_ERC_ave, y=log10(FIRE_SIZE))) +
  geom_jitter(width = 0, height = 0.1, color=multi_fire_color, size=2) +
  labs(title = "Kettleman Hills 2000-2015: ERC on Fire Ignition Days")


plot_grid(ERC_fire_size_plt, seven_day_ERC_fire_size_plt, thirty_day_ERC_fire_size_plt)

# WINDSPEED

WINDSPEED_fire_size_plt <- ggplot(this_fire_dat_w_landtype_station_dat[!(this_fire_dat_w_landtype_station_dat$FIRE_SIZE_CLASS %in%  c("A", "B")),], aes(x=ave_wind_speed, y=log10(FIRE_SIZE))) +
  geom_jitter(width = 0, height = 0.1, color=fire_color, size=2) +
  labs(title = "Kettleman Hills 2000-2015: WINDSPEED on Fire Ignition Days")

# SEVEN DAY AVE WINDSPEED
seven_day_WINDSPEED_fire_size_plt <- ggplot(this_fire_dat_w_landtype_station_dat[!(this_fire_dat_w_landtype_station_dat$FIRE_SIZE_CLASS %in%  c("A", "B")),], aes(x=seven_day_windspeed_ave, y=log10(FIRE_SIZE))) +
  geom_jitter(width = 0, height = 0.1, color=large_fire_color, size=2) +
  labs(title = "Kettleman Hills 2000-2015: WINDSPEED on Fire Ignition Days")

# MONTH AVE WINDSPEED
thirty_day_WINDSPEED_fire_size_plt <- ggplot(this_fire_dat_w_landtype_station_dat[!(this_fire_dat_w_landtype_station_dat$FIRE_SIZE_CLASS %in%  c("A", "B")),], aes(x=thirty_day_windspeed_ave, y=log10(FIRE_SIZE))) +
  geom_jitter(width = 0, height = 0.1, color=multi_fire_color, size=2) +
  labs(title = "Kettleman Hills 2000-2015: WINDSPEED on Fire Ignition Days")

plot_grid(WINDSPEED_fire_size_plt, seven_day_WINDSPEED_fire_size_plt, thirty_day_WINDSPEED_fire_size_plt)


#### LANDTYPE AGAINST FIRE SIZE ############################
############################################################

this_fire_dat_num_fires_by_landtype <- this_fire_dat_w_landtype_station_dat %>%
  group_by(FBFM13) %>%
  summarise(sum_area_burned_km_2 = sum(FIRE_SIZE_km_2),
            ave_fire_size_km_2 = mean(FIRE_SIZE_km_2, na.rm=TRUE),
            sd_fire_size_km_2 = sd(FIRE_SIZE_km_2, na.rm=TRUE)) 
# this_fire_dat_num_fires_by_landtype$pct_area_burned <- this_fire_dat_num_fires_by_landtype$sum_area_burned_km_2/sum(this_fire_dat_num_fires_by_landtype$sum_area_burned_km_2) 
landtype_tot_area_burned_above_threshold <- this_fire_dat_num_fires_by_landtype[which(this_fire_dat_num_fires_by_landtype$sum_area_burned_km_2 > 5), "FBFM13"][[1]]

this_land_shape_area_by_landtype <- this_land_shape@data %>%
  group_by(FBFM13) %>%
  summarise(sum_area_of_grid_sqs_km_2 = sum(land_area_km_2)) 
# this_land_shape_area_by_landtype$pct_area_of_grid_sqs_km_2 <- this_land_shape_area_by_landtype$sum_area_of_grid_sqs_km_2/sum(this_land_shape_area_by_landtype$sum_area_of_grid_sqs_km_2)   

this_fire_land_area_by_landtype <- left_join(this_land_shape_area_by_landtype, this_fire_dat_num_fires_by_landtype, by="FBFM13")
this_fire_land_area_by_landtype$area_burned_as_pct_of_ttl <- this_fire_land_area_by_landtype$sum_area_burned_km_2/this_fire_land_area_by_landtype$sum_area_of_grid_sqs_km_2*100

ggplot(this_fire_dat_w_landtype_station_dat, aes(x=FBFM13, y=log2(FIRE_SIZE), fill=FBFM13)) +
  geom_boxplot()

ggplot(this_fire_dat_w_landtype_station_dat[which(this_fire_dat_w_landtype_station_dat$FBFM13 %in% landtype_tot_area_burned_above_threshold),], 
       aes(x=log10(FIRE_SIZE), y=FBFM13, fill=FBFM13)) +
  geom_density_ridges()



#### BUILD REGRESSION MODEL ###############################
###########################################################

this_fire_dat_model <- this_fire_dat_w_landtype_station_dat[, c("FIRE_SIZE_km_2", "FBFM13", 
                                                                "ave_wind_speed",
                                                                "ERC")]


this_fire_dat_model$FIRE_SIZE_km_2 <- log10(this_fire_dat_model$FIRE_SIZE_km_2) 
cor_matrix_regression_model <- cor(this_fire_dat_model[, !c("FBFM13")], use="complete.obs", method="pearson")

corrplot(cor_matrix_regression_model, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)









