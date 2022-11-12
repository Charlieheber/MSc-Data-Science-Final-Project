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

ERC_ts <- ts(data=this_ERC_df$ERC, start=c(2000, 246), frequency=365.25)

components_ERCts <- decompose(ERC_ts)
plot(components_ERCts)

#### SPLIT INTO TRAIN AND VALIDATION SETS
#########################################

ERC_train <- this_ERC_df[this_ERC_df$year %in% 2000:2019, "ERC"]
ERC_validation <- this_ERC_df[this_ERC_df$year %in% 2020:2022,"ERC"] 

ERC_ts_train <- ts(ERC_ts[1:nrow(ERC_train)], start=c(2000, 246), frequency=365.25)
ERC_ts_validation <- ts(ERC_ts[nrow(ERC_train):nrow(this_ERC_df)], start=c(2000, 246), frequency=365.25)

#### BUILD ARIMA MODEL USING AUTO.ARIMA #
#########################################
# will do properly at later date

# train model
fit_basic_ERC_mod <- auto.arima(ERC_ts_train[[1]])
summary(fit_basic_ERC_mod)

# forecast
forecst_basic_ERC_mod <- predict(fit_basic_ERC_mod, 990)

# evaluation
rmse(forecst_basic_ERC_mod$pred, ERC_ts_validation$ERC)

#### Plot modelled ERC ###################
##########################################

pred_basic_ERC_mod <- data.frame(
  "ERC_pred" = forecst_basic_ERC_mod$pred,
  "day_of_pred" = 1:length(forecst_basic_ERC_mod$pred)
)

ggplot(pred_basic_ERC_mod, aes(x=day_of_pred, y = ERC_pred)) + 
  geom_point()



fit_fourier_ERC_mod <- auto.arima(ERC_ts_train, xreg=fourier(ERC_ts_train, K=4))

# forecast
forecst_fourier_ERC_mod <- predict(fit_fourier_ERC_mod, newxreg=fourier(ERC_ts_train, K=4))

# evaluation
rmse(forecst_fourier_ERC_mod$pred, ERC_ts_validation$ERC)

pred_fourier_ERC_mod <- data.frame(
  "ERC_pred" = forecst_fourier_ERC_mod$pred,
  "day_of_pred" = 1:length(forecst_fourier_ERC_mod$pred)
)

ggplot(pred_fourier_ERC_mod, aes(x=day_of_pred, y = ERC_pred)) + 
  geom_point()

ERC_ts

bestfit <- list(aicc=Inf)
for(K in 1:25){
  
  message(K)
  
  fit <- auto.arima(ERC_ts_train, xreg=fourier(ERC_ts_train, K=K), seasonal=FALSE, trace=TRUE)
  if(fit$aicc < bestfit$aicc){
    bestfit <- fit
    bestK <- K
  } else break
    
}

bestfit <- Arima(ERC_ts_train, order=c(2,1,4), xreg=fourier(ERC_ts_train, K=bestK))

fc <- forecast(bestfit, xreg=fourier(ERC_ts_train, K=bestK, h=365.25))
plot(fc)

pred <- predict(bestfit, newxreg = fourier(ERC_ts_validation, K=bestK))

plot.new()
line(pred$pred)
line(ERC_ts_validation)




