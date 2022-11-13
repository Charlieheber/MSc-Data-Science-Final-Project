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

#### VISUALISE ##########################
#########################################
display.brewer.all()

# lets plot ERC against igntion days (0 = no ignitions, 1 = ignitions) and colour by largest fire size ignited that day
fire_color = "lightcoral"
large_fire_color = "lightsteelblue"
multi_fire_color = RColorBrewer::brewer.pal(3, "Set2")[1]

# FIRE IGNITION DAYS
# ERC
fire_ignition_day_ERC_plot <- ggplot(this_fire_dat_w_ERC_wind_speed, aes(x=ERC, y=fire_ignition_day)) +
  geom_jitter(width = 0, height = 0.1, color=fire_color, size=0.1) +
  labs(title = "Kettleman Hills 2000-2015: ERC on Fire Ignition Days")

# # max wind gust
# fire_ignition_day_max_wind_gust_plot <- ggplot(this_fire_dat_w_ERC_wind_speed, aes(x=max_wind_gust, y=fire_ignition_day)) +
#   geom_jitter(width = 0, height = 0.1, color=fire_color) +
#   labs(title = "Kettleman Hills 2000-2015: ERC on Fire Ignition Days")

# LARGE FIRE IGNITION DAYS
large_fire_ignition_day_ERC_plot <- ggplot(this_fire_dat_w_ERC_wind_speed, aes(x=ERC, y=large_fire_ignition_day)) +
  geom_jitter(width = 0, height = 0.1, color=large_fire_color, size=0.1) +
  labs(title = "Kettleman Hills 2000-2015: ERC on Large Fire Ignition Days (> 100 acres)")

multi_fire_ignition_day_ERC_plot <- ggplot(this_fire_dat_w_ERC_wind_speed, aes(x=ERC, y=multi_fire_ignition_day)) +
  geom_jitter(width = 0, height = 0.1, color=multi_fire_color, size=0.1) +
  labs(title = "Kettleman Hills 2000-2015: ERC on Mulit Fire Ignition Days")

plot_grid(fire_ignition_day_ERC_plot, large_fire_ignition_day_ERC_plot, multi_fire_ignition_day_ERC_plot)

#### BUILD LOGISTIC MODELS ###############
##########################################
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4828741/
# 1) Probabilty of fire ignition
# 2) Probabilty of large fire given an ignition

# initial explanatory variables:  ERC(G), ave wind speed, maximum wind gust, mean wind direction 

# DATA PREP ###############################
###########################################
set.seed(123)

colnames(this_fire_dat_w_ERC_wind_speed)
this_predictors <- c("ave_wind_speed", "ERC", "mean_wind_direction", "max_wind_gust")

this_model1_vars <- c("fire_ignition_day", this_predictors)
this_model2_vars <- c("large_fire_ignition_day", this_predictors)

this_fire_dat_logit_model1_vars <- this_fire_dat_w_ERC_wind_speed[, ..this_model1_vars]
this_fire_dat_logit_model2_vars <- this_fire_dat_w_ERC_wind_speed[this_fire_dat_w_ERC_wind_speed$fire_ignition_day | 
                                                                    this_fire_dat_w_ERC_wind_speed$FIRE_SIZE_CLASS_A!=0, ..this_model2_vars]

# how many missing entries?
apply(this_fire_dat_logit_model1_vars, 2, function(x) sum(is.na(x)))

this_fire_dat_logit_model1_vars <- this_fire_dat_logit_model1_vars[!is.na(this_fire_dat_logit_model1_vars$ave_wind_speed),]
this_fire_dat_logit_model2_vars <- this_fire_dat_logit_model2_vars[!is.na(this_fire_dat_logit_model2_vars$ave_wind_speed),]

print(paste("Model 1:", sum(this_fire_dat_logit_model1_vars$fire_ignition_day), "fire ignitions days out of", dim(this_fire_dat_logit_model1_vars)[1]))
print(paste("Model 2:", sum(this_fire_dat_logit_model2_vars$large_fire_ignition_day), "large fire ignitions days out of", dim(this_fire_dat_logit_model2_vars)[1]))

# split into training/testing 

model_1_training_inx <- createDataPartition(this_fire_dat_logit_model1_vars$fire_ignition_day, p=0.7, list=FALSE)
model_2_training_inx <- createDataPartition(this_fire_dat_logit_model2_vars$large_fire_ignition_day, p=0.7, list=FALSE)


this_fire_dat_logit_model1_vars_lst <- list("train"=this_fire_dat_logit_model1_vars[model_1_training_inx,],
                                            "test"=this_fire_dat_logit_model1_vars[-model_1_training_inx,])

this_fire_dat_logit_model2_vars_lst <- list("train"=this_fire_dat_logit_model2_vars[model_2_training_inx,],
                                            "test"=this_fire_dat_logit_model2_vars[-model_2_training_inx,])

# STEP 1: CORRELATION MATRIX ###
################################

cor_matrix_logit_model <- cor(this_fire_dat_logit_model1_vars[, c("ave_wind_speed", "mean_wind_direction", "max_wind_gust",
                                                                  "ERC")], use="complete.obs", method="pearson")

corrplot(cor_matrix_logit_model, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# max wind speed and ave wind gust are highly correlated (ya doy) so only keep one
# keep one w/ lowest p-value? ave_wind_speed

this_fire_dat_logit_model1_vars_lst <- lapply(this_fire_dat_logit_model1_vars_lst, function(x) x %>% select(!max_wind_gust))
this_fire_dat_logit_model2_vars_lst <- lapply(this_fire_dat_logit_model2_vars_lst, function(x) x %>% select(!max_wind_gust))

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

# model 2
model_2_univariable.ERC <- glm(large_fire_ignition_day~ERC, family=binomial(link="logit"), data=this_fire_dat_logit_model2_vars_lst$train)
model_2_univariable.ave_wind_speed <- glm(large_fire_ignition_day~ave_wind_speed, family=binomial(link="logit"), data=this_fire_dat_logit_model2_vars_lst$train)
model_2_univariable.mean_wind_direction <- glm(large_fire_ignition_day~mean_wind_direction, family=binomial(link="logit"), data=this_fire_dat_logit_model2_vars_lst$train)

summary(model_2_univariable.ERC)
summary(model_2_univariable.ave_wind_speed)
summary(model_2_univariable.mean_wind_direction)

# ERC and ave_wind_speed have a p-value lower than 0.25 so can be included for multivariable regression, mean wind direction should be discarded
# cutoff value of 0.25 supported by literature (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4828741/)

# STEP 2: MULTIVARIABLE MODEL COMPARISONS ####
##############################################

# model 1
model_1.1 <- glm(fire_ignition_day~ave_wind_speed+mean_wind_direction+ERC, family=binomial(link="logit"), data=this_fire_dat_logit_model1_vars_lst$train)
summary(model_1.1)

# mean_wind_direction has a p value of 0.262 --> can be excluded!

model_1.2 <- glm(fire_ignition_day~ave_wind_speed+ERC, family=binomial(link="logit"), data=this_fire_dat_logit_model1_vars_lst$train)
summary(model_1.2)

# compare the changes in coefficients for each variable in model 1.1 and 1.2
model_1_delta.coef <- abs((coef(model_1.2)-coef(model_1.1)[-3]))/coef(model_1.1)[-3]
round(model_1_delta.coef, 3)

# model 2

# use class weights to address imbalance
large_fire_class_weights = sum(this_fire_dat_logit_model2_vars$large_fire_ignition_day)/dim(this_fire_dat_logit_model2_vars)[1]

this_fire_dat_logit_model2_vars_lst$train <- this_fire_dat_logit_model2_vars_lst$train %>%
  mutate(
      model_2_class_weights = case_when(
        large_fire_ignition_day ~ 1 - large_fire_class_weights,
        !large_fire_ignition_day ~ large_fire_class_weights,
      ) 
  )

model_2.1 <- glm(large_fire_ignition_day~ave_wind_speed+mean_wind_direction+ERC, family=quasibinomial(link="logit"), 
                 data=this_fire_dat_logit_model2_vars_lst$train, weights = this_fire_dat_logit_model2_vars_lst$train$model_2_class_weights)
summary(model_2.1)

# mean_wind_direction has a p value of 0.215 --> can be excluded!

model_2.2 <- glm(large_fire_ignition_day~ave_wind_speed+ERC, family=quasibinomial(link="logit"), 
                 data=this_fire_dat_logit_model2_vars_lst$train)
summary(model_2.2)

# compare the changes in coefficients for each variable in model 1.1 and 1.2
model_2_delta.coef <- abs((coef(model_2.2)-coef(model_2.1)[-3]))/coef(model_2.1)[-3]
round(model_2_delta.coef, 3)

# STEP 3: GET PROBABILITIES & PREDICTIONS ##
############################################

# model_1.1 <- glm(fire_ignition_day~ave_wind_speed+mean_wind_direction+ERC, family=binomial(link="logit"), data=this_fire_dat_logit_model_vars_train)
# summary(model_1.1)
# 
# # mean_wind_direction has a p value of 0.35 --> can be excluded!
# 
# model_1.2 <- glm(fire_ignition_day~ave_wind_speed+ERC, family=binomial(link="logit"), data=this_fire_dat_logit_model_vars_train)
# summary(model_1.2)
# 
# # Predict the probability (p) of fire ignition
# probabilities <- predict(model_1.2, type = "response")
# predicted.classes <- ifelse(probabilities > 0.5, "ignition_day", "non_ignition_day")
# head(predicted.classes, 100)

# STEP 4: TEST LINEARITY ASSUMPTION ########
############################################
# Bind the logit and tidying the data for plot
this_final_predictors <- c("ave_wind_speed", "ERC")

# Predict the probability (p) of fire ignition
# model 1
model_1.2_probs <- predict(model_1.2, type = "response")
model_1.2_pred.classes <- ifelse(model_1.2_probs > 0.5, "ignition_day", "non_ignition_day")
head(model_1.2_pred.classes, 100)

# model 2
model_2.2_probs <- predict(model_2.2, type = "response")
model_2.2_pred.classes <- ifelse(model_2.2_probs > 0.5, "ignition_to_large_fire", "ignition_not_to_large_fire")
head(model_2.2_pred.classes, 100)


this_fire_dat_logit_model1_vars_lst$train[, ..this_final_predictors]

this_fire_dat_logit_model_vars_train_predictors <- this_fire_dat_logit_model_vars_train_predictors %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(this_fire_dat_logit_model_vars_train_predictors, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

# STEP 5: INFLUENTIAL VALUES
# http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/#linearity-assumption

# Note that, not all outliers are influential observations. 
# To check whether the data contains potential influential observations, 
# the standardized residual error can be inspected. 
# Data points with an absolute standardized residuals above 3 represent possible outliers and may deserve closer attention.

plot(model_1, which = 4, id.n = 3)

spline(this_fire_dat_logit_model_vars_train$ave_wind_speed)

# Extract model results
model.data <- broom::augment(model_1) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = fire_ignition_day), alpha = .5) +
  theme_bw()

# there are no influential values in our dataset

# STEP 6: GOODNESS OF FIT


