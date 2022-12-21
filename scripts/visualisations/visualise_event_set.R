rm(list=ls())
library(here)

##### LOAD PACKAGES ######################
##########################################

script_loc <- paste0(here::here(), "/scripts/R/")
req_packages = c("general",  "visualisation", "spatial")
source(paste0(script_loc, "libraries_and_file_locs.R"))

#### GET INPUT DATA ####################
########################################
seed = 123
set.seed(seed)
run <- 1:10
EVENTSET_name <- paste0("EVENT_SET_1000_yrs_SEED_123")

EVENTSET <- fread(paste0(input_file_loc, "/EVENTSET/", EVENTSET_name, ".csv"))

this_YLT <- fread(paste0(input_file_loc, "/dummy portfolio/EVENT_SET_1000_yrs_SEED_123_EDM_500properties_187Mn_TIV_circularWFs_YLT.csv"))

this_YLT <- this_YLT[order(this_YLT$GU_loss),]
this_YLT$RP <- 1:999
this_YLT$EP <- 1/this_YLT$RP
this_YLT$nonEP <- 1-this_YLT$EP

# HISTORICAL FIRES TO COMPARE WITH 
this_fire_dat <- fread(paste0(input_file_loc, "/wildfire simulation model/historical fire data/kettleman_hils_2000_2015_100_km_fires.csv"))
calfire_comp <- fread(paste0(input_file_loc, "/EVENTSET/CalFire_comp.csv"))

##### COMPARE AREA BURNED/NUM FIRES ####
########################################

this_fire_dat %>% 
  group_by(FIRE_YEAR) %>%
  summarise(
    "area burned" = sum(FIRE_SIZE),
    "ave fire size" = mean(FIRE_SIZE),
    "num fires" = n()
  )

area_burned_by_year <- EVENTSET %>%
  group_by(sim_year) %>%
  summarise(
    "area burned" = sum(FIRE_SIZE),
    "ave fire size" = mean(FIRE_SIZE),
    "num fires" = n()
  )
area_burned_by_year_unordered <- area_burned_by_year
area_burned_by_year[order(area_burned_by_year$`area burned`),] 

##### BOX AND WHISKER ##################
#######################################

mean(this_fire_dat$FIRE_SIZE)
mean(EVENTSET$FIRE_SIZE)
median(this_fire_dat$FIRE_SIZE)
median(EVENTSET$FIRE_SIZE)


sd(this_fire_dat$FIRE_SIZE)
sd(EVENTSET$FIRE_SIZE)

fire_res_compare <- data.frame("FIRE_SIZE" = c(this_fire_dat$FIRE_SIZE, EVENTSET$FIRE_SIZE),
                               "source" = c(rep("empirical", length(this_fire_dat$FIRE_SIZE)),
                                            rep("simulated", length(EVENTSET$FIRE_SIZE)))
                               )


ggplot(fire_res_compare, aes(x=log(FIRE_SIZE), color=source)) +
  geom_density()

ggplot(fire_res_compare, aes(x=source, y=log(FIRE_SIZE), color=source)) +
  geom_boxplot(coef=5, size = 2) + ylab("log(fire size (acres))")+
  theme(text =element_text(size=34))


#### MODEL PARAMS ######################
########################################

this_study_area_radius <- 100 # km
this_centroid_study_area <- c(-120.06, 36.03) # lon/lat

sq_km_to_acres = 247.105

# VISUALISE #####################################
#################################################

# NUM FIRES PER DAY
res_num_fires_by_day <- EVENTSET %>%
  group_by(DOY) %>%
  summarise(
    num_fires = n()
  )

ggplot(res_num_fires_by_day, aes(x=DOY, y=num_fires)) +
  geom_density(stat="identity", position="dodge")+
  theme_minimal()

# CALFIRE COMPARISON ##############################
###################################################

EVENTSET_calfire_comp <- area_burned_by_year_unordered[1:100,]
EVENTSET_calfire_comp$study_area <- pi*(100)^(2)*247.105
EVENTSET_calfire_comp$area_burned_per_acre <- EVENTSET_calfire_comp$`area burned`/EVENTSET_calfire_comp$study_area

res <- EVENTSET_calfire_comp[,c("sim_year", "area_burned_per_acre")]
names(res) <- c("YEAR", "area_burned_per_acre")

res$dist = "simulated"
calfire_comp$dist <- "CALFIRE 1992-2018"

res_2 <- rbind(res, calfire_comp[,c("YEAR", "area_burned_per_acre", "dist")])
calfire_comp


res_2$id <- 1:length(res_2$YEAR)
ggplot(res_2, aes(id, area_burned_per_acre, fill = dist)) +     # Using default colors
  geom_bar(stat = "identity")+ ylab("area burned per acre land")+
  theme(text =element_text(size=20))


# EP CURVES #######################################
###################################################
# AREA BURNED BY YEAR
res_area_burned_by_year <- EVENTSET %>%
  group_by(sim_year) %>%
  summarise(
    tot_area_burned_acres = sum(FIRE_SIZE)
  )
res_area_burned_by_year <- res_area_burned_by_year[order(res_area_burned_by_year$tot_area_burned_acres),] 

res_area_burned_by_year$RP <- 1:max(res_area_burned_by_year$sim_year) 
res_area_burned_by_year$EP <- 1/res_area_burned_by_year$RP
res_area_burned_by_year$nonEP <- 1-res_area_burned_by_year$EP

ggplot(res_area_burned_by_year, aes(x=log10(tot_area_burned_acres), y=EP)) +
  ylim(c(0,0.1)) +
  geom_line(size=1) +
  theme_minimal()+xlab("log10(Area per yr (acres))")+ylab("Exceedance Probability")

# SIMULATED LOSSES
this_YLT <- this_YLT[order(this_YLT$GU_loss),]
this_YLT$RP <- 1:999
this_YLT$EP <- 1/this_YLT$RP
this_YLT$nonEP <- 1-this_YLT$EP

ggplot(this_YLT, aes(x=GU_loss, y=log(EP)) ) +
  # ylim(c(0,0.1)) +
  geom_line(size=1) +
  theme_minimal()+xlab("Total simulated yearly loss")+ylab("Exceedance Probability")


# MAP FIRE YEARS (ASSUMING SPHERICAL)
# PLOT FIRE SIZE BY YEAR (ASSUMING SPHERICAL)
map_spherical_fires_by_year <- function(res_fires_df, sim_year){
  
  res_fires_df <- data.frame(res_fires_df)
  year_res_fires_df <- res_fires_df[which(res_fires_df$sim_year == sim_year),]
  year_res_fires_df$FIRE_SIZE_sq_km <- year_res_fires_df$FIRE_SIZE/sq_km_to_acres
  
  spherical_fire_radii <-  sqrt(year_res_fires_df$FIRE_SIZE_sq_km/pi)
  
  leaflet(year_res_fires_df) %>%
    addProviderTiles("Esri.WorldImagery") %>%
    addCircles(lng=~lon, lat=~lat, radius=~spherical_fire_radii*1000, weight=2, fillOpacity=1, opacity=1,
               fillColor="grey", color="black") %>%
    addCircles(lng=this_centroid_study_area[1], lat=this_centroid_study_area[2],
               radius=this_study_area_radius*1000, color="blue", opacity = 1, fillOpacity=0) %>%
    addLegend(
      colors = "blue", title = "Study Area", labels = "study area",
      opacity = 1, position="bottomleft"
    ) %>%
    addLegend(colors = "grey", labels="simulated spherical fire radii", title = paste("Spherical Fire Radii: year", sim_year),
              position="bottomleft")
  
  
}
map_spherical_fires_by_year(EVENTSET, 2)
