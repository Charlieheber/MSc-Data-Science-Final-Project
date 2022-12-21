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
ERC_ts_train <- ts(data=this_ERC_df[this_ERC_df$year %in% 2000:2018, "ERC"], start=c(2000, 246), frequency=365)
ERC_ts_validation <- ts(data=this_ERC_df[this_ERC_df$year %in% 2019:2022, "ERC"], start=c(2019, 0), frequency=365)
ERC_ts_lst <- list("all" = ERC_ts, "train" = ERC_ts_train, "validation" = ERC_ts_validation)

lapply(ERC_ts_lst, start)
lapply(ERC_ts_lst, end)
lapply(ERC_ts_lst, frequency)

#### VISUALISE THE DATA #################
#########################################

components_ERCts <- stats::decompose(ERC_ts)
plot(components_ERCts)

plot(ERC_ts)
abline(reg=lm(ERC_ts~time(ERC_ts)))

ERC_ts_cycle <- cycle(ERC_ts)

#This will print the cycle across years.
plot(aggregate(ERC_ts,FUN=mean))

##### SEASONAL DIFF ###################
######################################

# monthly_ERC <- this_station_data_w_ERC %>%
#   group_by(year, month) %>%
#   summarise(
#    `average monthly ERC` = mean(ERC, na.rm=TRUE) 
#   )
# monthly_ERC_ts <- ts(monthly_ERC[monthly_ERC$year != 2000, "average monthly ERC"], start=2001, frequency=12)
# monthly_ERC_ts_seasonal_diff <- diff(monthly_ERC_ts, differences=12, lag=1)
# 
# forecast::Acf(monthly_ERC_ts_seasonal_diff, 36)
# 
# forecast::Pacf(monthly_ERC_ts_diff, 36)
# 
# diff_365 <- diff(ERC_ts, 365)
# 
# forecast::Acf(ERC_ts, 400)
# forecast::Pacf(ERC_ts, 400)

#### TBATS MODELLING ##################
#######################################
set.seed(1)

ERC_TBATS_model <- forecast::tbats(ERC_ts_lst$train, use.trend = FALSE, use.damped.trend = FALSE)
tbats.components(ERC_TBATS_model)
print(ERC_TBATS_model)

summary(ERC_TBATS_model)

ERC_TBATS_model$k.vector
as.character(ERC_TBATS_model)

#### ARIMA WITH FOURIER TERMS #########
#######################################

ERC_ARIMA_model_bestfit <- list(aicc=Inf)
for(k in 1:25){
  
  message(k)
  ERC_ARIMA_model_fit <- auto.arima(window(ERC_ts_lst$train, start=2001), xreg=forecast::fourier(window(ERC_ts_lst$train, start=2001), K=k), 
                                    seasonal=FALSE, trace=TRUE)
  
  if(ERC_ARIMA_model_fit$aicc < ERC_ARIMA_model_bestfit$aicc){
    
    ERC_ARIMA_model_bestfit <- ERC_ARIMA_model_fit
    best_k <- k
    
  } else {break}
}


### VISUALISE #########################
#######################################

# TBATS
# plot(predict(ERC_TBATS_model, n.ahead=1))
plot(simulate(ERC_TBATS_model, nsim=365))

ERC_TBATS_plot <- window(ERC_ts_lst$validation, start=2019, end=c(2020, 0)) %>% autoplot() + 
  labs(x = "Year", y = "ERC", title = "TBATS simulations of modelled Energy Release Component")
for (i in 1:2){
  ERC_TBATS_plot <- ERC_TBATS_plot + autolayer(simulate(ERC_TBATS_model, nsim = 365), series = paste("Simulated year:", i))
}
ERC_TBATS_plot

# ARIMA
plot(simulate(ERC_ARIMA_model_bestfit, nsim=365,
              xreg=fourier(window(ERC_ts_lst$validation, start=2019, end=c(2020, 0)), K=best_k)))

ERC_ARIMA_plot <- window(ERC_ts_lst$validation, start=2019, end=c(2020, 0)) %>% autoplot(size=1.2) + 
  labs(x = "Year", y = "ERC", title = "ARIMA simulations of modelled Energy Release Component")
for (i in 1:2){
  ERC_ARIMA_plot <- ERC_ARIMA_plot + autolayer(
    simulate(ERC_ARIMA_model_bestfit, 
             xreg=fourier(window(ERC_ts_lst$validation, start=2019, end=c(2020, 0)), K=best_k)), 
    series = paste("Simulated year:", i), size=1.2)
}
ERC_ARIMA_plot

# NAIVE MODEL 
# Dummy data
data <- data.frame(
  day = as.Date("2017-06-14") - 0:364,
  value = runif(365) + seq(-140, 224)^2 / 10000
)


naive_pred <- data.frame("day" = rep(1:365,2),
           "ERC" = c(window(ERC_ts_lst$validation, start=2019, end=c(2020, 0)), 
                     tapply(ERC_ts_train,cycle(ERC_ts_train),mean)),
           "source" = c(rep("empirical", 365), rep("naive model", 365)))

# Most basic bubble plot
ggplot(naive_pred, aes(x=day, y=ERC, color=source, group=source)) +
  geom_line(size=1.2) + 
  ylab("ERC") + scale_colour_manual(values = c("black", "#F8766D"))

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
mean_absolute_error(ERC_validation, rep(tapply(ERC_ts_lst$train,cycle(ERC_ts_lst$train),mean),1))

# SD OF MODELS 
sd(naive_pred[naive_pred$source == "naive model", "ERC"])
sd(ERC_ARIMA_pred)
sd(ERC_validation)

delta_SD_ARIMA = abs(sd(ERC_validation) - sd(ERC_ARIMA_pred))
delta_SD_naive = abs(sd(naive_pred[naive_pred$source == "naive model", "ERC"]) - sd(ERC_ARIMA_pred))

### SAVE BOTH MODELS ####################
#########################################

saveRDS(ERC_TBATS_model, paste0(output_file_loc, "/models/ERC_TSM_TBATS.rds"))
saveRDS(ERC_ARIMA_model_bestfit, paste0(output_file_loc, "/models/ERC_TSM_ARIMA.rds"))








