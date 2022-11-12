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

#### TBATS MODELLING ##################
#######################################
set.seed(123)

ERC_TBATS_model <- forecast::tbats(ERC_ts_lst$test, use.trend = FALSE, use.damped.trend = FALSE)
print(ERC_TBATS_model)
as.character(ERC_TBATS_model)

#### SARIMA WITH FOURIER TERMS ########
#######################################

ERC_ARIMA_model_bestfit <- list(aicc=Inf)
for(k in 1:25){
  
  message(k)
  ERC_ARIMA_model_fit <- auto.arima(window(ERC_ts_lst$test, start=2001), xreg=fourier(window(ERC_ts_lst$test, start=2001), K=k), 
                                    seasonal=FALSE, trace=TRUE)
  
  if(ERC_ARIMA_model_fit$aicc < ERC_ARIMA_model_bestfit$aicc){
    
    ERC_ARIMA_model_bestfit <- ERC_ARIMA_model_fit
    best_k <- k
    
  } else {break}
}


### VISUALISE #########################
#######################################

# TBATS
plot(predict(ERC_TBATS_model, n.ahead=1))
plot(simulate(ERC_TBATS_model, nsim=365))

ERC_TBATS_plot <- window(ERC_ts_lst$validation, start=2019, end=c(2020, 0)) %>% autoplot() + 
  labs(x = "Year", y = "ERC(G)", title = "TBATS simulations of modelled Energy Release Component (fuel model G)")
for (i in 1:2){
  ERC_TBATS_plot <- ERC_TBATS_plot + autolayer(simulate(ERC_TBATS_model, nsim = 365), series = paste("Simulated year:", i))
}
ERC_TBATS_plot

# ARIMA
plot(simulate(ERC_ARIMA_model_bestfit, nsim=365,
              xreg=fourier(window(ERC_ts_lst$validation, start=2019, end=c(2020, 0)), K=best_k)))

ERC_ARIMA_plot <- window(ERC_ts_lst$validation, start=2019, end=c(2020, 0)) %>% autoplot() + 
  labs(x = "Year", y = "ERC(G)", title = "ARIMA simulations of modelled Energy Release Component (fuel model G)")
for (i in 1:2){
  ERC_ARIMA_plot <- ERC_ARIMA_plot + autolayer(
    simulate(ERC_ARIMA_model_bestfit, 
             xreg=fourier(window(ERC_ts_lst$validation, start=2019, end=c(2020, 0)), K=best_k)), 
    series = paste("Simulated year:", i))
}
ERC_ARIMA_plot

### COMPARE PERFORMANCE OF MODELS #####
#######################################

mean_absolute_error <- function(actual, pred){
  mean(abs(pred-actual))
}

# validate on 3 full years (2019-2022)
ERC_validation <- as.vector(window(ERC_ts_lst$validation, start=2019, end=c(2022, 0)))
num_validation_yrs <- length(ERC_validation)/365

# simulate 3 years of data
# TBATS
ERC_TBATS_pred <-c()
for(i in 1:num_validation_yrs){
  ERC_TBATS_pred <- append(ERC_TBATS_pred, simulate(ERC_TBATS_model, nsim = 365))
}

# ARIMA
ERC_ARIMA_pred <-c()
for(i in 1:num_validation_yrs){
  ERC_ARIMA_pred <- append(ERC_ARIMA_pred, simulate(ERC_ARIMA_model_bestfit,
                                                    xreg=fourier(window(ERC_ts_lst$validation, start=2019, end=c(2020, 0)), K=best_k)))
}

mean_absolute_error(ERC_validation, ERC_TBATS_pred)
mean_absolute_error(ERC_validation, ERC_ARIMA_pred)







