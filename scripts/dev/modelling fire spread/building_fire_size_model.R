rm(list=ls())
library(here)

##### LOAD PACKAGES ######################
##########################################

script_loc <- paste0(here::here(), "/scripts/R/")
req_packages = c("general",  "visualisation", "spatial", "statistical", "SQL")
source(paste0(script_loc, "libraries_and_file_locs.R"))

# FUNCTIONS ########################
####################################

get_lognormal_fire_size_r_vars <- function(ave_fire_size, sd_fire_size, pl_fit_lst, n, landtype, dist="power-law"){
  
  if(dist=="lnorm"){
    
    location <- log(ave_fire_size^2/sqrt(sd_fire_size^2 + ave_fire_size^2))
    shape <- sqrt(log(1 +  (sd_fire_size^2/ave_fire_size^2)))
    
    # generate random variables
    this_fire_size_LN_pred_fit <- data.frame(
      "FIRE_SIZE" = rlnormTrunc(n, 
                                mean=location, 
                                sd=shape, 
                                min=10),
      "FBFM13" = landtype,
      "source" = "ln-dist")
    
    return(this_fire_size_LN_pred_fit)
    
    
  } else if(dist =="power-law"){
    
    # landtype = "Water"
    alpha = pl_fit_lst[which(names(pl_fit_lst) == landtype)][[1]]$alpha
    
    this_fire_size_PL_pred_fit <- data.frame(
      "FIRE_SIZE" = rpldis(n,
                           alpha=alpha, xmin=10),
      "FBFM13" = landtype,
      "source" = "power-law")
    return(this_fire_size_PL_pred_fit)
    

  }
  
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

agg <- this_fire_dat_w_landtype %>%
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

ggplot(this_fire_dat_w_landtype, 
       aes(x=log10(FIRE_SIZE), y=FBFM13, fill=FBFM13)) +
  geom_density_ridges() +  theme(legend.position =  "none",
                                 text =element_text(size=34))


###### BIN DATA ###############################
###############################################

get_fire_size_dat_w_lnorm_pl <- function(fire_size_dat_dist_fit, bin_cuts, pl_fit){
  
  # fire_size_dat_dist_fit <- this_fire_size_dat_dist_fit_lst[[1]]
  # bin_cuts <- this_bin_cuts
  
  # bin_cuts <- c(bin_cuts, max(fire_size_dat_dist_fit$FIRE_SIZE))
  
  bin_delta <- c(diff(bin_cuts), max(fire_size_dat_dist_fit$FIRE_SIZE) - max(bin_cuts)) 
  # bin_delta <- c(bin_cuts[1:length(bin_cuts)], max(fire_size_dat_dist_fit$FIRE_SIZE))
  # bin_delta <- c(bin_cuts[-length(bin_cuts)] + diff(bin_cuts)/2, (max(fire_size_dat_dist_fit$FIRE_SIZE)-max(bin_cuts))/2)
  
  fire_size_dat_dist_fit$bin <- findInterval(fire_size_dat_dist_fit$FIRE_SIZE, bin_cuts, all.inside = TRUE)
  
  fire_size_dat_bin <- fire_size_dat_dist_fit %>%
    group_by(bin) %>%
    summarise(
      FBFM13 = unique(FBFM13),
      delta_n = n(),
      N = length(fire_size_dat_dist_fit$FIRE_SIZE) 
    )
  
  # length(bin_delta)
  if(length(which(!(1:length(bin_delta) %in% fire_size_dat_bin$bin))) != 0){
    
    missing_bins <- c(1:length(bin_delta))[which(!(1:length(bin_delta) %in% fire_size_dat_bin$bin))]
    missing_bin_deltas <- bin_delta[missing_bins]
    missing_bins_df <- data.frame(
      "bin" = missing_bins,
      "delta_n" = 0,
      "N" = length(fire_size_dat_dist_fit$FIRE_SIZE),
      "FBFM13" = unique(fire_size_dat_bin$FBFM13)
    )
    fire_size_dat_bin <- rbind(fire_size_dat_bin, missing_bins_df)
    
    
  }
  fire_size_dat_bin <- fire_size_dat_bin[order(fire_size_dat_bin$bin),]
  
  fire_size_dat_bin$A <- bin_delta
  
  
  fire_size_dat_bin$f_A <- (1/fire_size_dat_bin$N)*(fire_size_dat_bin$delta_n/fire_size_dat_bin$A)
  fire_size_dat_bin$fit <- "empirical"
  fire_size_dat_bin <- fire_size_dat_bin[, c("A", "f_A", "fit", "FBFM13")]
  
  fire_size_dat_bin_wlnorm <- rbind(fire_size_dat_bin, 
                                    data.frame(
                                      "A" = fire_size_dat_bin$A,
                                      "f_A" = dlnormTrunc(fire_size_dat_bin$A, 
                                                          mean(log(fire_size_dat_dist_fit$FIRE_SIZE)), 
                                                          sd(log(fire_size_dat_dist_fit$FIRE_SIZE)), 
                                                          min=10),
                                      "fit" = "lnorm",
                                      "FBFM13" = unique(fire_size_dat_bin$FBFM13)
                                    ),
                                    data.frame(
                                      "A" = fire_size_dat_bin$A,
                                      "f_A" = dpldis(fire_size_dat_bin$A, pl_fit$alpha, xmin=10),
                                      "fit" = "power-law",
                                      "FBFM13" = unique(fire_size_dat_bin$FBFM13)
                                    ))
  
  return(fire_size_dat_bin_wlnorm)
  
}

this_fire_size_dat_dist_fit <- this_fire_dat_w_landtype[,c("FIRE_SIZE", "FBFM13")]

this_all_fire_size_dat_dist_fit <- this_fire_size_dat_dist_fit
this_all_fire_size_dat_dist_fit$FBFM13 <- ".All fires"

this_fire_size_dat_bin <- rbind(this_fire_size_dat_dist_fit, this_all_fire_size_dat_dist_fit)
this_fire_size_dat_dist_fit_lst <- split(this_fire_size_dat_bin, this_fire_size_dat_bin$FBFM13)

this_bin_cuts <- c(10, 2^4, 2^5, 2^6, 2^7, 2^8, 2^9, 2^10, 2^11, 2^12, 2^13, 2^14, 2^15, 2^16)
length(this_bin_cuts)

res_lst <- list()
plot_lst <- list()
pl_fit_lst <- list()
for(i in 1:length(this_fire_size_dat_dist_fit_lst)){
  
  pl_fit_lst[[i]] <- power.law.fit(this_fire_size_dat_dist_fit_lst[[i]]$FIRE_SIZE,
                                   implementation = "plfit")
  
  res_lst[[i]] <- get_fire_size_dat_w_lnorm_pl(this_fire_size_dat_dist_fit_lst[[i]], this_bin_cuts, pl_fit_lst[[i]])
  
  plot_lst[[i]] <- ggplot(res_lst[[i]] %>% arrange(desc(fit)), 
                          aes(x=log10(A), y=log10(f_A), fill=fit, group=fit, color=fit, size=fit, shape=fit)) + geom_point() + 
                          scale_color_manual(values=c("black", "#F8766D", "#00BFC4"))+
                          scale_size_manual(values=c(2,2,2))+
                          scale_shape_manual(values=c(3,19,19))+
                          theme(legend.position = "none")
    
  
  
}
names(plot_lst) <- names(this_fire_size_dat_dist_fit_lst)
names(pl_fit_lst) <- names(this_fire_size_dat_dist_fit_lst)

plot_grid(plotlist=plot_lst, labels=names(plot_lst), label_x = 0.6, label_y = 0.9, label_size = 11)

##### CHI SQUARED TEST ########################
##############################################

ks_list = list("lnorm"=list(), "pl"=list())
for(i in 1:length(this_fire_size_dat_dist_fit_lst)){
  
  x <- this_fire_size_dat_dist_fit_lst[[i]]
  
  
  
  lnorm <- rlnormTrunc(length(x$FIRE_SIZE),
                       mean(log(x$FIRE_SIZE)), 
                       sd(log(x$FIRE_SIZE)), 
                       min=10)
  
  pl <- rpldis(length(x$FIRE_SIZE),
               alpha=pl_fit_lst[[i]]$alpha, xmin=10)
  
  message("lnorm")
  ks_list$lnorm[[i]] <- ks.test(x$FIRE_SIZE, 
          lnorm,
          alternative='two.sided'
  )
  
  message("pl")
  ks_list$pl[[i]] <- pl_fit_lst[[i]]
  
}
names(ks_list$lnorm) = names(this_fire_size_dat_dist_fit_lst)
names(ks_list$pl) = names(this_fire_size_dat_dist_fit_lst)

mean(this_fire_size_dat_dist_fit_lst$`.All fires`$FIRE_SIZE)
sd(this_fire_size_dat_dist_fit_lst$`.All fires`$FIRE_SIZE)

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

fuel_types <- unique(this_fire_size_dat_dist_w_pred$FBFM13)

chi_sq_lst <- list()
for(i in 1:length(fuel_types)){
  
  chi_sq_lst[[i]] <- chisq.test(this_fire_size_dat_dist_w_pred[this_fire_size_dat_dist_w_pred$source == "actual" & 
                                              this_fire_size_dat_dist_w_pred$FBFM13 == fuel_types[i], "FIRE_SIZE"][[1]],
             this_fire_size_dat_dist_w_pred[this_fire_size_dat_dist_w_pred$source == "ln-dist" &
                                              this_fire_size_dat_dist_w_pred$FBFM13 == fuel_types[i], "FIRE_SIZE"][[1]])
  
  
}

#### SAVE RESULT ##############################
###############################################

saveRDS(pl_fit_lst, paste0(output_file_loc, "/wildfire simulation model/fire_size_power_law_alphas.rds"))
write.csv(agg, paste0(output_file_loc, "/wildfire simulation model/agg_fire_sizes_all_years_by_landtype.csv"), row.names=FALSE)
write.csv(this_agg_fire_size_dat_dist_fit, paste0(output_file_loc, "/wildfire simulation model/agg_fire_sizes_all_years_by_landtype.csv"), row.names=FALSE)
