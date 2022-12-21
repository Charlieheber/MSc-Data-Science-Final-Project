rm(list=ls())
library(here)

##### LOAD PACKAGES ######################
##########################################

script_loc <- paste0(here::here(), "/scripts/R/")
req_packages = c("general",  "visualisation", "spatial", "statistical")
source(paste0(script_loc, "libraries_and_file_locs.R"))

##### FIRE INPUT DATA ###################
#########################################

input_file_loc <- paste0(input_file_loc, "/wildfire simulation model/")
this_fire_dat <- fread(paste0(input_file_loc, "/station data/station_data_kettleman_hills_00_22_w_ERC_fire_ignitions.csv"))

#### BUILD MODEL #######################
########################################

## POISSON ##########################
#####################################
# only care about ignitions days 
this_fire_dat_ignition_days <- this_fire_dat[this_fire_dat$fire_ignition_day,]  
# this_fire_dat_ignition_days <- this_fire_dat  

this_fire_dat_num_ignitions <- this_fire_dat_ignition_days %>%
  group_by(num_ignitions) %>%
  summarise(
    Freq = n(),
    source = "actual"
  )
1.


# how many multi fire days?
ggplot(data=this_fire_dat_num_ignitions, aes(x=as.factor(num_ignitions), y=Freq)) +
  geom_bar(stat="identity", fill="#F8766D")+
  theme_minimal() + xlab("number ignitions / day") +
  theme(legend.position =  "none",text =element_text(size=34))+ coord_flip()


lambda <- sum((this_fire_dat_num_ignitions$num_ignitions)*this_fire_dat_num_ignitions$Freq)/sum(this_fire_dat_num_ignitions$Freq)
num_ignitions <- rpois(length(this_fire_dat_ignition_days$id), lambda)

this_fire_dat_ignitions_w_pois <- data.frame(table(num_ignitions))
this_fire_dat_ignitions_w_pois$num_ignitions = as.numeric(as.character(this_fire_dat_ignitions_w_pois$num_ignitions)) + 1 
this_fire_dat_ignitions_w_pois$source = "pois"

this_fire_dat_num_ignitions_w_pred <- rbind(this_fire_dat_num_ignitions,
      this_fire_dat_ignitions_w_pois)

#### ECDF ##############################
########################################

num_ignitions_ecdf <-ecdf(this_fire_dat_ignition_days$num_ignitions)
num_ignitions_ecdf(4)

plot(num_ignitions_ecdf, verticals = TRUE, do.points = FALSE)

stats::quantile(num_ignitions_ecdf, seq=1:100)

F10 <- ecdf(rnorm(10))
summary(F10)

plot(F10)
plot(F10, verticals = TRUE, do.points = FALSE)

epdfPlot(this_fire_dat_ignition_days$num_ignitions, discrete=TRUE)

saveRDS(num_ignitions_ecdf, paste0(output_file_loc, "/ecdf_num_ignitions.rds"))

#### VISUALISE #########################
########################################

# how many multi fire days?
ggplot(data=this_fire_dat_num_ignitions_w_pred, aes(x=as.factor(num_ignitions), y=Freq, fill=source)) +
  geom_bar(stat="identity", position="dodge")

#### BUILD MODEL FUNCTION #############
#######################################



pois_num_fires_per_day_model <- function(lambda, n){
  
  # want to count from 0 (ie. 0 is one fire per day)
  lambda = lambda - 1

  num_ignitions <- rpois(n, lambda)
  
  return(num_ignitions)  
}





