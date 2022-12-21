rm(list=ls())
library(here)

##### LOAD PACKAGES ######################
##########################################

script_loc <- paste0(here::here(), "/scripts/R/")
req_packages = c("general",  "visualisation", "statistical")
source(paste0(script_loc, "libraries_and_file_locs.R"))

##### GET INPUT DATA ####################
#########################################

input_file_loc <- paste0(input_file_loc, "/wildfire simulation model/")
this_fire_dat_w_ERC_wind_speed <- fread(paste0(input_file_loc, "/station data/station_data_kettleman_hills_00_22_w_ERC_fire_ignitions.csv"))

this_fire_size_classes <- c("FIRE_SIZE_CLASS_C",
                            "FIRE_SIZE_CLASS_D", "FIRE_SIZE_CLASS_E", 
                            "FIRE_SIZE_CLASS_F", "FIRE_SIZE_CLASS_G")

#### BUILD LOGISTIC MODELS ###############
##########################################

# DATA PREP ###############################
###########################################
set.seed(123)

colnames(this_fire_dat_w_ERC_wind_speed)
this_predictors <- c("ERC")

this_model1_vars <- c("fire_ignition_day", this_predictors)
this_fire_dat_logit_model1_vars <- this_fire_dat_w_ERC_wind_speed[, ..this_model1_vars] # row for every day 

# how many missing entries?
apply(this_fire_dat_logit_model1_vars, 2, function(x) sum(is.na(x)))

# Two Sample t-test - ERC of fire ignition days is statistically different to non-igntions 
t.test(this_fire_dat_logit_model1_vars[this_fire_dat_logit_model1_vars$fire_ignition_day, "ERC"],
       this_fire_dat_logit_model1_vars[!this_fire_dat_logit_model1_vars$fire_ignition_day, "ERC"])


print(paste("Model 1:", sum(this_fire_dat_logit_model1_vars$fire_ignition_day), "fire ignitions days out of", dim(this_fire_dat_logit_model1_vars)[1]))

# split into training/testing 
model_1_training_inx <- createDataPartition(this_fire_dat_logit_model1_vars$fire_ignition_day, p=0.7, list=FALSE)


this_fire_dat_logit_model1_vars_lst <- list("train"=this_fire_dat_logit_model1_vars[model_1_training_inx,],
                                            "test"=this_fire_dat_logit_model1_vars[-model_1_training_inx,])

lapply(this_fire_dat_logit_model1_vars_lst, 
       function(x) data.frame(
         "ignition days" = sum(x$fire_ignition_day),
         "total days" = dim(x)[1]
         ))

1679-181



#### INVESTIGATE DISTRIBUTIONS ##########
#########################################
# lets plot ERC against igntion days (0 = no ignitions, 1 = ignitions) and colour by largest fire size ignited that day
color_1 = "lightcoral"
color_2 = "lightsteelblue"
color_3 = RColorBrewer::brewer.pal(3, "Set2")[1]

this_fire_dat_logit_model1_vars %>%
  group_by(fire_ignition_day) %>%
  summarise(
    ave_ERC = mean(ERC),
  )

# SCATTER
# ERC
fire_ignition_day_ERC_plot <- ggplot(this_fire_dat_logit_model1_vars, aes(x=ERC, y=fire_ignition_day)) +
  geom_jitter(width = 0, height = 0.1, color=color_1, size=0.4) +
  labs(title = "Kettleman Hills 2000-2015: ERC on Fire Ignition Days") + xlab("ERC") + ylab("fire ignition day")

fire_ignition_day_ERC_plot

# BOX & WHISKER
ggplot(this_fire_dat_logit_model1_vars, aes(x=fire_ignition_day, y=ERC, color=fire_ignition_day)) +
  geom_boxplot() + xlab("fire ignition day") + ylab("ERC")

#### LOGISTIC REGRESSION ################
#########################################

model_LR <- glm(fire_ignition_day~ERC, family=binomial(link="logit"), data=this_fire_dat_logit_model1_vars_lst$train)
summary(model_LR)
plot(model_LR)

### NAIVE BAYES ########################
########################################

model_NB <- naivebayes::naive_bayes(fire_ignition_day~ERC, data=this_fire_dat_logit_model1_vars_lst$train)

plot(model_NB)

### PREDICTION #########################
########################################

stats::predict(model_LR, data.frame("ERC" = 150), type="response")

this_fire_dat_logit_model1_vars_lst$test$LR_model_prob <- stats::predict(model_LR, this_fire_dat_logit_model1_vars_lst$test[, c("ERC")], type="response")
this_fire_dat_logit_model1_vars_lst$test$NB_model_prob <- stats::predict(model_NB, this_fire_dat_logit_model1_vars_lst$test[, c("ERC")], type="prob")[,2]
this_fire_dat_logit_model1_vars_lst$test$NM_model_prob <- sum(this_fire_dat_logit_model1_vars$fire_ignition_day)/length(this_fire_dat_logit_model1_vars$fire_ignition_day)

this_fire_dat_logit_model1_vars_lst$test[ which(this_fire_dat_logit_model1_vars_lst$test$LR_model_prob == max(this_fire_dat_logit_model1_vars_lst$test$LR_model_prob)),]
stats::predict(model_LR, this_fire_dat_logit_model1_vars_lst$test[, c("ERC")], type="response")

model1_binned_test_data <- this_fire_dat_logit_model1_vars_lst$test
model1_binned_test_data$bin <- findInterval(this_fire_dat_logit_model1_vars_lst$test$ERC, c(0,10,20,30,40,50,60,70,80,90,100))
model1_binned_test_data <- model1_binned_test_data %>%
  mutate(
    midpoint = case_when(
      bin == 1 ~ 5,
      bin == 2 ~ 15,
      bin == 3 ~ 25,
      bin == 4 ~ 35,
      bin == 5 ~ 45,
      bin == 6 ~ 55,
      bin == 7 ~ 65,
      bin == 8 ~ 75,
      bin == 9 ~ 85,
      bin == 10 ~ 95,
    )
  )

model1_binned_test_data <- model1_binned_test_data %>%
  group_by(bin) %>%
  summarise(
    midpoint = unique(midpoint),
    num_days = n(),
    obs_fire_ignition_days = sum(fire_ignition_day),
    LR_pred_fire_ignition_days = sum(LR_model_prob),
    NB_pred_fire_ignition_days = sum(NB_model_prob),
    NM_pred_fire_ignition_days = sum(NM_model_prob),
  )
model1_binned_test_data$prop_fire_ignition_days_obs <- model1_binned_test_data$obs_fire_ignition_days/model1_binned_test_data$num_days
model1_binned_test_data$LR_prop_fire_ignition_days_pred <- model1_binned_test_data$LR_pred_fire_ignition_days/model1_binned_test_data$num_days
model1_binned_test_data$NB_prop_fire_ignition_days_pred <- model1_binned_test_data$NB_pred_fire_ignition_days/model1_binned_test_data$num_days
model1_binned_test_data$NM_prop_fire_ignition_days_pred <- model1_binned_test_data$NM_pred_fire_ignition_days/model1_binned_test_data$num_days

RMSE <- function(f, o){
  
  f_o = f-o
  
  sqrt(sum(f_o)^2/length(f_o))
  
}

fire_ignition_RMSE <- data.frame(
  "RMSE_LR" = RMSE(model1_binned_test_data$LR_prop_fire_ignition_days_pred, model1_binned_test_data$prop_fire_ignition_days_obs),
  "RMSE_NB" = RMSE(model1_binned_test_data$NB_prop_fire_ignition_days_pred, model1_binned_test_data$prop_fire_ignition_days_obs),
  "RMSE_NM" = RMSE(model1_binned_test_data$NM_prop_fire_ignition_days_pred, model1_binned_test_data$prop_fire_ignition_days_obs))

#### PLOT PROBABILITIES ##########
##################################

synthetic_dat <- data.frame("ERC"=1:1000/10)
synthetic_dat$NB_model_prob <- stats::predict(model_NB, data.frame("ERC"=1:1000/10), type="prob")[,2]
synthetic_dat$NM_model_prob <- sum(this_fire_dat_logit_model1_vars$fire_ignition_day)/length(this_fire_dat_logit_model1_vars$fire_ignition_day)

ggplot(this_fire_dat_logit_model1_vars_lst$train, aes(x=ERC, y=as.numeric(fire_ignition_day))) +
  geom_line(data=synthetic_dat, aes(x=ERC, y=NB_model_prob), color=color_2, size=1.8) +
  geom_line(data=synthetic_dat, aes(x=ERC, y=NM_model_prob), color=color_3, size=1.8) +
  stat_smooth(method="glm", color=color_1, se=FALSE,
              method.args = list(family=binomial), size=1.8)+
  geom_point(data=model1_binned_test_data, aes(x=midpoint, y=prop_fire_ignition_days_obs), color="grey", size=5) +
  ylab("P(fire ignition day | ERC)") + xlab("ERC") +
  theme(legend.position =  "none",
        text =element_text(size=34))


#### SAVE RESULT #################
##################################

saveRDS(model_LR, paste0(output_file_loc, "/models/fire_igntion_day_vars_ERC.rds"))

write.csv(model1_binned_test_data, paste0(output_file_loc, "/models/fire_igntion_mod_preds.csv"), row.names = FALSE)
write.csv(fire_ignition_RMSE, paste0(output_file_loc, "/models/fire_igntion_mod_RMSE.csv"), row.names=FALSE)


