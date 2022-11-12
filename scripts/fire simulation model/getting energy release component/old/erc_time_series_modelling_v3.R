rm(list=ls())
library(here)

##### LOAD PACKAGES ######################
##########################################

script_loc <- paste0(here::here(), "/scripts/R/")
req_packages = c("general", "statistical")
source(paste0(script_loc, "libraries_and_file_locs.R"))

##### GET INPUT DATA ####################
#########################################

this_input_file_loc <- paste0(input_file_loc, "/wildfire simulation model")
this_output_file_loc <- paste0(output_file_loc, "/wildfire simulation model")

this_station_data_w_ERC <- fread(paste0(this_input_file_loc, "/fuel moisture contents/station_data_kettleman_hills_00_22_w_ERC.csv"))


this_ERC_df <- data.table(
  "day_of_run" = this_station_data_w_ERC[!is.na(this_station_data_w_ERC$ERC), "day_of_run"][[1]],
  "ERC" = this_station_data_w_ERC[!is.na(this_station_data_w_ERC$ERC), "ERC"][[1]],
  "year" = this_station_data_w_ERC[!is.na(this_station_data_w_ERC$ERC), "year"][[1]]
)

#### TRANSFORM TO TS DATA ###############
#########################################

ERC_ts <- ts(data=this_ERC_df$ERC, start=c(2000, 246), frequency=365)
ERC_ts_test <- ts(data=this_ERC_df[this_ERC_df$year %in% 2000:2018, "ERC"], start=c(2000, 246), frequency=365)
ERC_ts_validation <- ts(data=this_ERC_df[this_ERC_df$year %in% 2019:2022, "ERC"], start=c(2019, 0), frequency=365)
ERC_ts_lst <- list("all" = ERC_ts, "test" = ERC_ts_test, "validation" = ERC_ts_validation)

lapply(ERC_ts_lst, start)
lapply(ERC_ts_lst, end)
lapply(ERC_ts_lst, frequency)

#### VISUALISE THE DATA #################
#########################################

components_ERCts <- decompose(ERC_ts)
plot(components_ERCts)

plot(ERC_ts)
abline(reg=lm(ERC_ts~time(ERC_ts)))

ERC_ts_cycle <- cycle(ERC_ts)

#This will print the cycle across years.
plot(aggregate(ERC_ts,FUN=mean))


#### TRAIN MODEL ########################
#########################################

ERC_model_bestfit <- list(aicc=Inf)
for(k in 1:25){
  
  message(k)
  ERC_model_fit <- auto.arima(ERC_ts_lst$test, xreg=fourier(ERC_ts_lst$test, K=k), seasonal=FALSE, trace=TRUE)
  
  if(ERC_model_fit$aicc < ERC_model_bestfit$aicc){
    
    ERC_model_bestfit <- ERC_model_fit
    ERC_model_best_k <- k
    
  } else {break}
}

ERC_bestmodel_fit <- Arima(ERC_ts_lst$test, order=c(3,1,2), xreg=fourier(ERC_ts_lst$test, K=k),
                           lambda=1)
as.character(ERC_bestmodel_fit)

ERC_bestmodel_fit %>%
  forecast(xreg=fourier(ERC_ts_lst$validation, K=k, h=365)) %>%
  autoplot() + autolayer(ERC_ts_lst$validation)


fc <- forecast::forecast(ERC_bestmodel_fit, xreg=fourier(ERC_ts_lst$validation, K=k, h=5*365))
fc <- forecast::forecast(ERC_bestmodel_fit, h=2, xreg=1:20)
plot(fc)

predict()



