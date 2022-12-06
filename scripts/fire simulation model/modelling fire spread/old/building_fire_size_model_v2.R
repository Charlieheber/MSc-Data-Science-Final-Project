rm(list=ls())
library(here)

##### LOAD PACKAGES ######################
##########################################

script_loc <- paste0(here::here(), "/scripts/R/")
req_packages = c("general",  "visualisation", "spatial", "statistical")
source(paste0(script_loc, "libraries_and_file_locs.R"))

# FUNCTIONS ########################
####################################

# fns_loc <- paste0(here::here(), "/scripts/R/fns/")
# source(paste0(fns_loc, ".R"))

##### GET INPUT DATA ####################
#########################################
acres_to_km <- 1/247.1

this_fire_dat <- fread(paste0(input_file_loc, "/wildfire simulation model/historical fire data/kettleman_hils_2000_2015_100_km_fires.csv"))
this_fire_dat <- this_fire_dat[!(this_fire_dat$FIRE_SIZE_CLASS %in% c("A", "B")),]

this_station_dat <- fread(paste0(input_file_loc, "/wildfire simulation model/station data/station_data_kettleman_hills_00_22_W_ERC_fire_ignitions.csv"))


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


# JOIN FIRE DATA TO WEATHER DATA ############################
################################################################

this_fire_dat_w_landtype_station_dat <- this_fire_dat_w_landtype %>%  left_join(this_station_dat[,c("DOY_id", "ERC", "ave_wind_speed")], by="DOY_id", drop=TRUE)

#### ERC/WINDSPEED AGAINST FIRE SIZE ##########################
###############################################################

fire_color = "lightcoral"
large_fire_color = "lightsteelblue"
multi_fire_color = RColorBrewer::brewer.pal(3, "Set2")[1]

# ERC
ERC_fire_size_plt <- ggplot(this_fire_dat_w_landtype_station_dat[!(this_fire_dat_w_landtype_station_dat$FIRE_SIZE_CLASS %in%  c("A", "B")),], aes(x=ERC, y=log10(FIRE_SIZE))) +
  geom_jitter(width = 0, height = 0.1, color=fire_color, size=2) +
  labs(title = "Kettleman Hills 2000-2015: ERC on Fire Ignition Days")

plot_grid(ERC_fire_size_plt)

# WINDSPEED
WINDSPEED_fire_size_plt <- ggplot(this_fire_dat_w_landtype_station_dat[!(this_fire_dat_w_landtype_station_dat$FIRE_SIZE_CLASS %in%  c("A", "B")),], aes(x=ave_wind_speed, y=log10(FIRE_SIZE))) +
  geom_jitter(width = 0, height = 0.1, color=fire_color, size=2) +
  labs(title = "Kettleman Hills 2000-2015: WINDSPEED on Fire Ignition Days")

plot_grid(WINDSPEED_fire_size_plt)

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

# DENSITY PLOT
ggplot(this_fire_dat_w_landtype_station_dat[which(this_fire_dat_w_landtype_station_dat$FBFM13 %in% landtype_tot_area_burned_above_threshold),], 
       aes(x=log10(FIRE_SIZE), y=FBFM13, fill=FBFM13)) +
  geom_density_ridges()

ggplot(this_fire_dat_w_landtype_station_dat[which(this_fire_dat_w_landtype_station_dat$FBFM13 %in% landtype_tot_area_burned_above_threshold),], 
       aes(x=log10(FIRE_SIZE))) +
  geom_density()

this_fire_dat_w_landtype_station_dat %>%
  group_by(FIRE_SIZE_CLASS) %>%
  summarise(sum_area_burned_km_2 = sum(FIRE_SIZE_km_2),
            ave_fire_size_km_2 = mean(FIRE_SIZE_km_2, na.rm=TRUE),
            sd_fire_size_km_2 = sd(FIRE_SIZE_km_2, na.rm=TRUE))

#### ERC+WINDSPEEND AGAINST FIRE SIZE CLASS ################
############################################################
this_fire_dat_NN_model <- this_fire_dat_w_landtype_station_dat[!is.na(this_fire_dat_w_landtype_station_dat$ERC), c("FIRE_SIZE_CLASS", "ERC", "ave_wind_speed", "FBFM13")]
this_fire_dat_NN_model <- this_fire_dat_NN_model %>% replace_na(list(ave_wind_speed = mean(this_fire_dat_NN_model$ave_wind_speed, na.rm=TRUE)))

# combine fire size classes F+G
this_fire_dat_NN_model <- this_fire_dat_NN_model %>%
  mutate(
    NN_model_FIRE_SIZE_CLASS = case_when(
      FIRE_SIZE_CLASS == "C" ~ "C",
      FIRE_SIZE_CLASS == "D" ~ "D",
      FIRE_SIZE_CLASS == "E" ~ "E",
      FIRE_SIZE_CLASS %in% c("F", "G") ~ "F+",
    )
  )
this_fire_dat_NN_model$FIRE_SIZE_CLASS <- NULL

this_fire_dat_NN_model %>%
  group_by(NN_model_FIRE_SIZE_CLASS) %>%
  summarise(
    num_fires = n(),
    mean_ERC = mean(ERC, na.rm=TRUE),
    mean_wind_speed = mean(ave_wind_speed, na.rm=TRUE))


ggplot(this_fire_dat_NN_model, aes(x=NN_model_FIRE_SIZE_CLASS, y=ERC, fill=ERC)) +
  geom_boxplot(coef=2.5)

ggplot(this_fire_dat_NN_model, aes(x=NN_model_FIRE_SIZE_CLASS, y=ave_wind_speed, fill=ave_wind_speed)) +
  geom_boxplot(coef=2.5)

#### BUILD NN MODEL ########################################
############################################################
set.seed(123)
# 
# ### PREDICT FUNCTION FOR NN MODEL ###
# predict <- function(data, nn){
#   
#   prediction <- data.frame(neuralnet::compute(nn, data.frame(data[,!c("NN_model_FIRE_SIZE_CLASS")]))$net.result)*100
#   colnames(prediction) <- colnames(nn$response)
#   
#   prediction$pred <- colnames(prediction)[apply(prediction,1,which.max)]
#   
#   res <- cbind(data, prediction)
#  
#   return(res)
#    
# }
# 
# # SMOTE - DON'T WANT TO DO THIS AS INTERESTED IN PROBs 
# # this_fire_dat_NN_model$NN_model_FIRE_SIZE_CLASS <- as.factor(this_fire_dat_NN_model$NN_model_FIRE_SIZE_CLASS)
# 
# # SMOTE(NN_model_FIRE_SIZE_CLASS ~ ., this_fire_dat_NN_model, perc.over=200, perc.under=100)
# 
# ### ONE HOT ENCODING ###
# # landtype var 
# unique(this_fire_dat_NN_model$FBFM13)
# 
# NN_model_hot_encoding <- function(dat){
#   
#   res <- dat %>% mutate(
#     FBFM5 = case_when(
#       FBFM13 == "FBFM5" ~ TRUE,
#       FBFM13 != "FBFM5" ~ FALSE
#     ),
#     FBFM9 = case_when(
#       FBFM13 == "FBFM9" ~ TRUE,
#       FBFM13 != "FBFM9" ~ FALSE
#     ),
#     Barren = case_when(
#       FBFM13 == "Barren" ~ TRUE,
#       FBFM13 != "Barren" ~ FALSE
#     ),
#     FBFM2 = case_when(
#       FBFM13 == "FBFM2" ~ TRUE,
#       FBFM13 != "FBFM2" ~ FALSE
#     ),
#     Agriculture = case_when(
#       FBFM13 == "Agriculture" ~ TRUE,
#       FBFM13 != "Agriculture" ~ FALSE
#     ),
#     Urban = case_when(
#       FBFM13 == "Urban" ~ TRUE,
#       FBFM13 != "Urban" ~ FALSE
#     ),
#     FBFM8 = case_when(
#       FBFM13 == "FBFM8" ~ TRUE,
#       FBFM13 != "FBFM8" ~ FALSE
#     ),
#     Water = case_when(
#       FBFM13 == "Water" ~ TRUE,
#       FBFM13 != "Water" ~ FALSE
#     )
#   )
#   res$FBFM13 = NULL
#   
#   return(res)
#   
# }
# NN_training_data_hot_encoding <- NN_model_hot_encoding(this_fire_dat_NN_model)
# 
# ### SPLIT DATA ###
# training_data_rows <- floor(0.70 * nrow(NN_training_data_hot_encoding))         
# training_indices <- sample(c(1:nrow(NN_training_data_hot_encoding)), training_data_rows)
# 
# NN_training_data <- NN_training_data_hot_encoding[training_indices,]
# NN_test_data <- NN_training_data_hot_encoding[-training_indices,]
# 
# ### NORMALISE DATA? ###
# 
# # this_fire_dat_NN_model <- this_fire_dat_NN_model %>%
# #   mutate(across(1:2, scale))
# 
# ### BUILD NN ###
# this_NN_model <- neuralnet(NN_model_FIRE_SIZE_CLASS~ERC+ave_wind_speed+FBFM5+FBFM9+Barren+FBFM2+Agriculture+Urban+FBFM8+Water, 
#                            data=NN_training_data_hot_encoding, linear.output=FALSE, hidden=5)
# 
# ### NN PREDICTIONS ###
# this_NN_pred <- predict(NN_test_data, this_NN_model)

#### BUILD LOGNORMAL/POWER DISTS ##################################
###################################################################
# install.packages("EnvStats")
# install.packages("poweRlaw")
library(poweRlaw)
library(EnvStats)

# 1) 1 DIST FOR ALL FIRES
# this_fire_size_dat_dist_fit <- this_rough_fire_size_dat[, c("FIRE_SIZE")]
# this_fire_size_dat_dist_fit$FBFM13 <- NA
this_fire_size_dat_dist_fit <- this_fire_dat_w_landtype_station_dat[!is.na(this_fire_dat_w_landtype_station_dat$ERC),
                                                                    c("FIRE_SIZE", "FBFM13")]

this_fire_size_dat_dist_fit$source <- "actual"

# LOGNORMAL FIT
this_fire_size_LN_dist <- fitdistr(this_fire_size_dat_dist_fit$FIRE_SIZE, "lognormal")

# https://msalganik.wordpress.com/2017/01/21/making-sense-of-the-rlnorm-function-in-r/
mean <- mean(this_fire_size_dat_dist_fit$FIRE_SIZE)
sd <- sd(this_fire_size_dat_dist_fit$FIRE_SIZE)

location <- log(mean^2/sqrt(sd^2 + mean^2))
shape <- sqrt(log(1 +  (sd^2/mean^2)))

# generate random variables
this_fire_size_LN_pred_fit <- data.frame(
  "FIRE_SIZE" = rlnormTrunc(1000, 
                            mean=location, 
                            sd=shape, 
                            min=0),
  "FBFM13" = NA,
  "source" = "ln-dist")


# POWER LAW
PL_dist_object = displ$new(round(this_fire_size_dat_dist_fit$FIRE_SIZE,0))
PL_params <- poweRlaw::estimate_pars(PL_dist_object)
# PL_xmin <- poweRlaw::estimate_xmin(PL_dist_object)

PL_dist_object$setXmin(10)
PL_dist_object$setPars(PL_params$pars) 

# generate random variables 
this_fire_size_PL_pred_fit <- data.frame(
  "FIRE_SIZE" = dist_rand(PL_dist_object, 1000),
  "FBFM13" = NA,
  "source" = "pl-dist")

# COMPARE DIST
this_fire_size_dat_dist <- rbindlist(list(this_fire_size_dat_dist_fit, 
                                          this_fire_size_LN_pred_fit, 
                                          this_fire_size_PL_pred_fit))

ggplot(this_fire_size_dat_dist, 
       aes(x=FIRE_SIZE, y=source, fill=source)) +
  xlim(0,500) +
  geom_density_ridges()

this_fire_size_dat_dist %>%
  group_by(source) %>%
  summarise(
    "min_size" = min(FIRE_SIZE),
    "max_size" = max(FIRE_SIZE),
    "mean_size" = mean(FIRE_SIZE),
    "sd_size" = sd(FIRE_SIZE)
  )


# 2) DIFFERENT DIST FOR EACH LANDTYPE

this_fire_dat_LN_fit_lst <- lapply(unique(this_fire_dat_LN_fit$FBFM13), function(x) this_fire_dat_LN_fit[this_fire_dat_LN_fit$FBFM13 == x,])

this_fit_dist_lst <- lapply(this_fire_dat_LN_fit_lst, function(x) fitdistr(x$FIRE_SIZE, "lognormal"))       

this_fit_dist_plt_lst <- list()
for(i in 1:length(this_fit_dist_lst)){
  
  this_fit_dist_plt_lst[[i]] <- ggplot(this_fire_dat_LN_fit_lst[[i]], aes(FIRE_SIZE)) + 
    xlim(0,500) +
    stat_function(fun = function(x) {
      dlnorm(x, this_fit_dist_lst[[i]]$estimate['meanlog'], 
             this_fit_dist_lst[[i]]$estimate['sdlog'])}, 
      geom="polygon", color="red", fill="blue", alpha=0.3, size=1) + 
    geom_density(size=1)
  
}








