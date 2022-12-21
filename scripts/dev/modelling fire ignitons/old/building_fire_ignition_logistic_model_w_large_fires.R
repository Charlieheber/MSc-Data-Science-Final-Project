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
# this_large_fire_size_classes <- c("FIRE_SIZE_CLASS_C", "FIRE_SIZE_CLASS_D", "FIRE_SIZE_CLASS_E", 
#                                   "FIRE_SIZE_CLASS_F", "FIRE_SIZE_CLASS_G")

#### BUILD LOGISTIC MODELS ###############
##########################################
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4828741/
# 1) Probabilty of fire ignition day
# 2) Probabilty of large fire given an ignition

# initial explanatory variables:  ERC(G), ave wind speed, maximum wind gust, mean wind direction 

# DATA PREP ###############################
###########################################
set.seed(123)

colnames(this_fire_dat_w_ERC_wind_speed)
this_predictors <- c("ave_wind_speed", "ERC", "mean_wind_direction", "max_wind_gust")

this_model1_vars <- c("fire_ignition_day", this_predictors)
# this_model2_vars <- c("large_fire_ignition_day", this_predictors)

this_fire_dat_logit_model1_vars <- this_fire_dat_w_ERC_wind_speed[, ..this_model1_vars] # row for every day 

# how many missing entries?
apply(this_fire_dat_logit_model1_vars, 2, function(x) sum(is.na(x)))

this_fire_dat_logit_model1_vars <- this_fire_dat_logit_model1_vars[!is.na(this_fire_dat_logit_model1_vars$ave_wind_speed),]

# Two Sample t-test - ERC of fire ignition days is statistically different to non-igntions 

t.test(this_fire_dat_logit_model1_vars[this_fire_dat_logit_model1_vars$fire_ignition_day, "ERC"],
       this_fire_dat_logit_model1_vars[!this_fire_dat_logit_model1_vars$fire_ignition_day, "ERC"])


print(paste("Model 1:", sum(this_fire_dat_logit_model1_vars$fire_ignition_day), "fire ignitions days out of", dim(this_fire_dat_logit_model1_vars)[1]))
# print(paste("Model 2:", sum(this_fire_dat_logit_model2_vars$large_fire_ignition_day), "large fire ignitions", dim(this_fire_dat_logit_model2_vars)[1]))

# split into training/testing 
model_1_training_inx <- createDataPartition(this_fire_dat_logit_model1_vars$fire_ignition_day, p=0.7, list=FALSE)


this_fire_dat_logit_model1_vars_lst <- list("train"=this_fire_dat_logit_model1_vars[model_1_training_inx,],
                                            "test"=this_fire_dat_logit_model1_vars[-model_1_training_inx,])

#### INVESTIGATE DISTRIBUTIONS ##########
#########################################

this_fire_dat_logit_model1_vars %>%
  group_by(fire_ignition_day) %>%
  summarise(
    ave_wind_speed = mean(ave_wind_speed),
    ave_ERC = mean(ERC),
    mean_wind_direction = mean(mean_wind_direction),
    ave_max_wind_gust = mean(max_wind_gust)
  )

ggplot(this_fire_dat_logit_model1_vars, aes(x=ERC, y=ave_wind_speed, color=fire_ignition_day)) +
  geom_point() + xlab("ERC") +  ylab("average daily wind speed")


#### VISUALISE ##########################
#########################################
display.brewer.all()

# lets plot ERC against igntion days (0 = no ignitions, 1 = ignitions) and colour by largest fire size ignited that day
fire_color = "lightcoral"
large_fire_color = "lightsteelblue"
multi_fire_color = RColorBrewer::brewer.pal(3, "Set2")[1]

# FIRE IGNITION DAYS
# ERC
fire_ignition_day_ERC_plot <- ggplot(this_fire_dat_logit_model1_vars, aes(x=ERC, y=fire_ignition_day)) +
  geom_jitter(width = 0, height = 0.1, color=fire_color, size=0.1) +
  labs(title = "Kettleman Hills 2000-2015: ERC on Fire Ignition Days") + xlab("ERC") + ylab("fire ignition day")

fire_ignition_day_ERC_plot

# STEP 1: CORRELATION MATRIX ###
################################

cor_matrix_logit_model <- cor(this_fire_dat_logit_model1_vars[, c("ave_wind_speed", "mean_wind_direction", "max_wind_gust",
                                                                  "ERC")], use="complete.obs", method="pearson")

corrplot(cor_matrix_logit_model, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# max wind speed and ave wind gust are highly correlated (ya doy) so only keep one
# keep one w/ lowest p-value? ave_wind_speed

this_fire_dat_logit_model1_vars_lst <- lapply(this_fire_dat_logit_model1_vars_lst, function(x) x %>% dplyr::select(!max_wind_gust))

# STEP 2: UNIVARIABLE ANALYSIS ####
###################################

# model 1
model_1_univariable.ERC <- glm(fire_ignition_day~ERC, family=binomial(link="logit"), data=this_fire_dat_logit_model1_vars_lst$train)
model_1_univariable.ave_wind_speed <- glm(fire_ignition_day~ave_wind_speed, family=binomial(link="logit"), data=this_fire_dat_logit_model1_vars_lst$train)
model_1_univariable.mean_wind_direction <- glm(fire_ignition_day~mean_wind_direction, family=binomial(link="logit"), data=this_fire_dat_logit_model1_vars_lst$train)

summary(model_1_univariable.ERC)
summary(model_1_univariable.ave_wind_speed)
summary(model_1_univariable.mean_wind_direction)

# All have a p-value lower than 0.25 so can be included for multivariable regression
# cutoff value of 0.25 supported by literature (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4828741/)

# STEP 2: MULTIVARIABLE MODEL COMPARISONS ####
##############################################

# model 1.1: predictors - ave_wind_speed, mean_wind_direction, ERC
model_1.1 <- glm(fire_ignition_day~ave_wind_speed+mean_wind_direction+ERC, family=binomial(link="logit"), data=this_fire_dat_logit_model1_vars_lst$train)
summary(model_1.1)

# mean_wind_direction has a p value of 0.262 --> can be excluded!

# model 1.2:  predictors - ave_wind_speed, ERC
model_1.2 <- glm(fire_ignition_day~ave_wind_speed+ERC, family=binomial(link="logit"), data=this_fire_dat_logit_model1_vars_lst$train)
summary(model_1.2)

# compare the changes in coefficients for each variable in model 1.1 and 1.2
model_1_delta.coef <- abs((coef(model_1.2)-coef(model_1.1)[-3]))/coef(model_1.1)[-3]
round(model_1_delta.coef, 3)

# STEP 3: GET PROBABILITIES & PREDICTIONS ##
############################################

# model 1.2
model_1.2_probs <- stats::predict(model_1.2, type = "response")
model_1.2_pred.classes <- ifelse(model_1.2_probs > 0.5, "ignition_day", "non_ignition_day")
head(model_1.2_pred.classes, 100)

# STEP 4: TEST LINEARITY ASSUMPTION ########
############################################
# Bind the logit and tidying the data for plot
this_final_predictors <- c("ave_wind_speed", "ERC")

# model 1
this_fire_dat_logit_model_1_2_train_pred_logit <- this_fire_dat_logit_model1_vars_lst$train[, ..this_final_predictors] %>%
  mutate(logit = log(model_1.2_probs/(1-model_1.2_probs))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(this_fire_dat_logit_model_1_2_train_pred_logit, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

# STEP 5: INFLUENTIAL VALUES ###########
########################################
# http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/#linearity-assumption

# Note that, not all outliers are influential observations. 
# To check whether the data contains potential influential observations, 
# the standardized residual error can be inspected. 
# Data points with an absolute standardized residuals above 3 represent possible outliers and may deserve closer attention.

plot(model_1.2, which = 4, id.n = 3)
plot(model_1.1, which = 4, id.n = 3)

# Extract model results
# model 1
model_1.2.data <- broom::augment(model_1.2) %>% 
  mutate(index = 1:n()) 
model_1.2.data %>% top_n(3, .cooksd)

ggplot(model_1.2.data, aes(index, .std.resid)) + 
  geom_point(aes(color = fire_ignition_day), alpha = .5) +
  theme_bw()

# there are no influential values in our dataset

# STEP 6: GOODNESS OF FIT ##############
########################################

# model 1
# model_1.1 - predictors: ave_wind_speed, mean_wind_direction, ERC
# model_1.2 - predictors: ave_wind_speed, ERC
# model_1.3 - predictors: ERC

# Likelihood Test: model_1.1:model_1.2
anova(model_1.1, model_1.2, test ="Chisq")
# p-value of 0.36 - model 1.1 is not statistically significant from model 1.2 (discard model 1.1)

# is a model w/ just ERC statistically significant from model w/ ERC & ave_wind_speed?
model_1.3 <- glm(fire_ignition_day~ERC, family=binomial(link="logit"), data=this_fire_dat_logit_model1_vars_lst$train)

# Likelihood Test: model_1.2:model_1.3
anova(model_1.2, model_1.3, test ="Chisq")
# p-value < 0.01 - model 1.2 is statistically significant from model 1.3 - model 1.2 should be retained 

# FINAL MODEL VALIDATION ##############
#######################################

# final models are 
# model 1.3 - logistic model w/ ERC as predictor variable
# model 2.4 - logitsic model w/ ERC and average wind speed as predictor variable

# 1) make predictions on test dataset

this_fire_dat_logit_model1_vars_lst$test$fire_ignition_day_model_prob <- predict(model_1.3, this_fire_dat_logit_model1_vars_lst$test[, c("ERC")], type="response")

# 2) Bin data for model 1

model1_binned_test_data <- this_fire_dat_logit_model1_vars_lst$test
model1_binned_test_data$bin <- findInterval(this_fire_dat_logit_model1_vars_lst$test$ERC, c(0,10,20,30,40,50,60,70,80,90,100))
model1_binned_test_data <- model1_binned_test_data %>%
  mutate(
    fire_ignition_day_model_pred = case_when(
      fire_ignition_day_model_prob >= 0.5 ~ TRUE,
      fire_ignition_day_model_prob < 0.5 ~ FALSE
    ),
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
    pred_fire_ignition_days = sum(fire_ignition_day_model_prob)
  )
model1_binned_test_data$prop_fire_ignition_days_obs <- model1_binned_test_data$obs_fire_ignition_days/model1_binned_test_data$num_days
model1_binned_test_data$prop_fire_ignition_days_pred <- model1_binned_test_data$pred_fire_ignition_days/model1_binned_test_data$num_days

ggplot(this_fire_dat_logit_model1_vars_lst$train, aes(x=ERC, y=as.numeric(fire_ignition_day))) +
  stat_smooth(method="glm", color=fire_color, se=FALSE,
              method.args = list(family=binomial)) +
  geom_point(data=model1_binned_test_data, aes(x=midpoint, y=prop_fire_ignition_days_obs), color="grey", size=3) +
  ylab("P(fire ignition day)") + xlab("ERC")


#### SAVE MODELS #############
##############################

model_1.2
model_1.3
model_2.1

saveRDS(model_1.2, paste0(output_file_loc, "/models/fire_igntion_day_vars_ERC_+_ave_ws.rds"))
saveRDS(model_1.3, paste0(output_file_loc, "/models/fire_igntion_day_vars_ERC.rds"))
saveRDS(model_2.1, paste0(output_file_loc, "/models/large_fire_igntion_day_vars_ERC_+_ave_ws.rds"))




