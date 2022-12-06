rm(list=ls())
library(here)

##### LOAD PACKAGES ######################
##########################################

script_loc <- paste0(here::here(), "/scripts/R/")
req_packages = c("general",  "visualisation", "SQL", "spatial")
source(paste0(script_loc, "libraries_and_file_locs.R"))

# FUNCTIONS ########################
####################################

# fns_loc <- paste0(here::here(), "/scripts/R/fns/")
# source(paste0(fns_loc, ".R"))

##### GET INPUT DATA ####################
#########################################

this_study_years <- c(2000, 2015)
this_study_area_lon_lat <- c(-120.06, 36.03)
this_study_area_radius_km <- 100

this_input_file_loc <- paste0(input_file_loc, "/hazard data/historical wildfires/")
this_output_file_loc <- paste0(output_file_loc, "/")

this_hist_wildfire_DB <- DBI::dbConnect(RSQLite::SQLite(), paste0(this_input_file_loc, "US_WildfireRecords.sqlite"))

this_rough_fire_dat <- data.table(dbGetQuery(this_hist_wildfire_DB, paste(
                                  "SELECT * FROM Fires WHERE 
                                   LATITUDE > 34 AND LATITUDE < 38 AND
                                   LONGITUDE > -122 AND LONGITUDE < -117 AND
                                   FIRE_YEAR >=", this_study_years[1], "AND FIRE_YEAR <=", this_study_years[2])))

odbc::dbDisconnect(this_hist_wildfire_DB)

#### GET WEATHER STATION DATA ################
#### (SO CAN JOIN FIRE DAT BY DAY)############ 

this_station_dat <- fread(paste0(input_file_loc, "/wildfire simulation model/station data/station_data_kettleman_hills_00_22.csv"))

#### TAKE CIRCLE SUBSET AROUND STUDY AREA ####
##############################################

this_rough_fire_dat$dist_from_study_area_km <- geosphere::distHaversine(this_study_area_lon_lat, this_rough_fire_dat[,c("LONGITUDE", "LATITUDE")])/1000
this_fire_dat <- this_rough_fire_dat[this_rough_fire_dat$dist_from_study_area_km <= this_study_area_radius_km,]

# DENSITY PLOT
min(this_rough_fire_size_dat$FIRE_SIZE)
max(this_rough_fire_size_dat$FIRE_SIZE)
ggplot(this_rough_fire_size_dat[this_rough_fire_size_dat$FIRE_SIZE > 0,], 
       aes(x=log(FIRE_SIZE))) + geom_density() + xlim(-3,4)


### ONLY KEEP FIRES FROM CLASS C ONWARDS #####
##############################################

this_fire_dat <- this_fire_dat[this_fire_dat$FIRE_SIZE_CLASS %in% c("C", "D", "E", "F", "G"),] 

#### GET DATA SUMMARY ########################
##############################################

head(this_fire_dat)

# what is fire shape?
class(this_fire_dat$Shape)
this_fire_dat <- data.table(this_fire_dat) %>% dplyr::select(-Shape)

# how many NAs per column 
apply(this_fire_dat, 2, function(x) sum(is.na(x)))

# are fire sizes missing 
apply(this_fire_dat[,c("FIRE_SIZE", "FIRE_SIZE_CLASS")], 2, function(x) sum(x==0))

# 1) look into fire sizes

# what is fire size class? A-G
unique(this_fire_dat$FIRE_SIZE_CLASS)

# A=0-0.25 acres, 
# B=0.26-9.9 acres, 
# C=10.0-99.9 acres, 
# D=100-299 acres, 
# E=300 to 999 acres, 
# F=1000 to 4999 acres, 
# and G=5000+ acres

this_fire_dat_fire_size_cls_count <- this_fire_dat %>%
  group_by(FIRE_SIZE_CLASS) %>%
  summarise(n = n())

ggplot(data=this_fire_dat_fire_size_cls_count, aes(x=FIRE_SIZE_CLASS, y=n)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()

# 2) what months have most ignitions?

this_fire_dat <- this_fire_dat %>%
  mutate(
    DISC_MONTH = case_when(
      DISCOVERY_DOY %in% c(1:31) ~ 1,
      DISCOVERY_DOY %in% c(32:59) ~ 2,
      DISCOVERY_DOY %in% c(60:90) ~ 3,
      DISCOVERY_DOY %in% c(91:120) ~ 4,
      DISCOVERY_DOY %in% c(121:151) ~ 5,
      DISCOVERY_DOY %in% c(152:181) ~ 6,
      DISCOVERY_DOY %in% c(182:212) ~ 7,
      DISCOVERY_DOY %in% c(213:243) ~ 8,
      DISCOVERY_DOY %in% c(244:273) ~ 9,
      DISCOVERY_DOY %in% c(274:304) ~ 10,
      DISCOVERY_DOY %in% c(305:334) ~ 11,
      DISCOVERY_DOY %in% c(335:365) ~ 12
    )
  )

this_fire_dat_fire_disc_month_count <- this_fire_dat %>%
  group_by(DISC_MONTH) %>%
  summarise(n = n())

this_fire_dat_fire_disc_doy_count <- this_fire_dat %>%
  group_by(DISCOVERY_DOY) %>%
  summarise(n = n())

ggplot(data=this_fire_dat_fire_disc_month_count, aes(x=DISC_MONTH, y=n)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()

ggplot(data=this_fire_dat_fire_disc_doy_count, aes(x=DISCOVERY_DOY, y=n)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()

# how many multi-fire days?

this_fire_dat_fire_disc_date_count <- this_fire_dat %>%
  group_by(DISCOVERY_DATE) %>%
  summarise(num_of_fires_in_day = n()) %>%
  group_by(num_of_fires_in_day) %>%
  summarise(n = n())

assertthat::are_equal(
  sum(this_fire_dat_fire_disc_date_count$n*this_fire_dat_fire_disc_date_count$num_of_fires_in_day),
  dim(this_fire_dat)[1])


ggplot(data=this_fire_dat_fire_disc_date_count, 
       aes(x=num_of_fires_in_day, y=n)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()

### MAP FIRES ###############################
#############################################

# A=0-0.25 acres, 
# B=0.26-9.9 acres, 
# C=10.0-99.9 acres, 
# D=100-299 acres, 
# E=300 to 999 acres, 
# F=1000 to 4999 acres, 
# and G=5000+ acres

fire_size_col <- RColorBrewer::brewer.pal(5, "Reds")
fire_size_labels <- c("C (10-99.9)", 
                      "D (100-299)", "E (300-999)", "F (1000-4999)", "G (5000+ acres)")
fire_size_title <- "Fire Size Class (acres)"

fire_size_pal <- colorFactor(
  palette = fire_size_col,
  domain = as.factor(sort(unique(this_fire_dat$FIRE_SIZE_CLASS)))
)

leaflet(this_fire_dat) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addProviderTiles("Stamen.TonerLabels") %>%
  addCircleMarkers(lng=~LONGITUDE, lat=~LATITUDE, radius=4, weight=2, fillOpacity=1, opacity=1,
                   fillColor=~fire_size_pal(FIRE_SIZE_CLASS), color="black") %>%
  addLegend(
    "bottomleft", colors=fire_size_col, labels=fire_size_labels, title=fire_size_title, 
    opacity=1
  )


### GET FIRE IGNITION DAYS ####################
###############################################

this_fire_dat_n_fires_by_DOY <- this_fire_dat %>% 
  group_by(FIRE_YEAR, DISCOVERY_DOY, FIRE_SIZE_CLASS) %>%
  summarise(n_fires = n())

this_fire_dat_n_fires_by_DOY_size_class <- pivot_wider(this_fire_dat_n_fires_by_DOY, names_from=FIRE_SIZE_CLASS, values_from=n_fires,
                                                       names_prefix = "FIRE_SIZE_CLASS_") %>% ungroup()


# add id columns (from station data)
this_fire_dat_n_fires_by_DOY_size_class <- this_fire_dat_n_fires_by_DOY_size_class %>%
  left_join(this_station_dat[,c("id", "year", "day_of_year")], by=c("FIRE_YEAR"="year", "DISCOVERY_DOY"="day_of_year")) %>% relocate(id)

this_fire_dat_n_fires_by_DOY_size_class <- this_fire_dat_n_fires_by_DOY_size_class %>% 
  replace_na(list(FIRE_SIZE_CLASS_A=0, FIRE_SIZE_CLASS_B=0, FIRE_SIZE_CLASS_C=0,
                  FIRE_SIZE_CLASS_D=0, FIRE_SIZE_CLASS_E=0, FIRE_SIZE_CLASS_F=0,
                  FIRE_SIZE_CLASS_G=0))


#### SAVE RESULT #################################
##################################################

file_output_name1 <- paste0("kettleman_hils_", this_study_years[1], "_", this_study_years[2], "_", this_study_area_radius_km,"_km_ignition_days_by_fire_size_class.csv")
file_output_name2 <- paste0("kettleman_hils_", this_study_years[1], "_", this_study_years[2], "_", this_study_area_radius_km,"_km_fires.csv")

write.csv(this_fire_dat_n_fires_by_DOY_size_class, 
          paste0(output_file_loc, "/wildfire simulation model/", file_output_name1),
          row.names=FALSE)

write.csv(this_fire_dat, 
          paste0(output_file_loc, "/wildfire simulation model/", file_output_name2),
          row.names=FALSE)



