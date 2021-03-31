# Packages ####

# resampling, splitting and validation
library(rsample)
# feature engineering or preprocessing
library(recipes)
# specifying models
library(parsnip)
# tuning
library(tune)
# tuning parameters
library(dials)
# measure model performance
library(yardstick)
# variable importance plots
library(vip)
# combining feature engineering and model specification
library(workflows)
# data manipulation
library(dplyr)
library(purrr)
library(tidyr)
# plotting
library(ggplot2)

# parallelism
library(doFuture)
library(parallel)
# timing
library(tictoc)
# viz
library(skimr)

# library(tidymodels)

# Data ####

data(credit_data, package='modeldata')
credit <- credit_data %>% as_tibble()
credit
?modeldata::credit_data

# EDA ####

ggplot(credit, aes(x=Status)) + geom_bar()

ggplot(credit, aes(x=Status, y=Amount)) + geom_violin()
ggplot(credit, aes(x=Status, y=Age)) + geom_violin()
ggplot(credit, aes(x=Status, y=Income)) + geom_violin()

ggplot(credit, aes(x=Age, y=Income, color=Status)) + 
    geom_point()

ggplot(credit, aes(x=Age, y=Income, color=Status)) + 
    geom_jitter()

ggplot(credit, aes(x=Age, y=Income, color=Status)) + 
    geom_hex() + 
    scale_fill_gradient(low='red', high='blue') +
    facet_wrap(~ Status) + 
    theme(legend.position='bottom')

# Split the Data ####

set.seed(8261)

# from {rsample}
credit_split <- initial_split(credit, prop=0.8, strata='Status')
credit_split
class(credit_split)

train <- training(credit_split)
test <- testing(credit_split)

train
test

# Feature Engineering ####
# also called preprocessing

# from {recipes}

# goal: relate outcome to inputs

# outcomes: response, y, label, target, output, known, result,
# ..., dependent variable, event, range
# inputs: predictors, x, features, covariates, variables, data,
# ..., attributes, independent variables, descriptors. context, 
# ..., subject variables

# two primary ways to deal with unbalanced data:
# 1) upsample the minority
# 2) downsample the majority class

library(useful)
colors1 <- tibble(Color=c('blue', 'green', 'blue', 'red', 'red', 
                          'yellow', 'green'))
build.x(~ Color, data=colors1)
build.x(~ Color, data=colors1, contrasts=FALSE)

rec1 <- recipe(Status ~ ., data=train) %>% 
    # xgboost can handle, will remove later
    themis::step_downsample(Status, under_ratio=1.2) %>% 
    # not really needed for xgboost
    step_normalize(Age, Price) %>% 
    step_other(all_nominal(), -Status, other='Misc') %>% 
    # remove columns with very little variance
    # as opposed to step_zv
    step_nzv(all_predictors()) %>% 
    # imputation: filling in missing values
    # not needed for xgboost
    step_modeimpute(all_nominal(), -Status) %>% 
    step_knnimpute(all_numeric()) %>% 
    step_dummy(all_nominal(), -Status, one_hot=TRUE)

rec1    

rec1 %>% prep() %>% juice()

# Model Specification ####

# from {parsnip}

# boosted tree
boost_tree()
# BART: dbart
# gbm
# catboost
# xgboost
# LightGBM: May NY R Meetup: meetup.com/nyhackr

# model types
# model modes
# engines
# parameters

parsnip::decision_tree
parsnip::rand_forest
parsnip::svm_poly
parsnip::svm_rbf
parsnip::linear_reg
multinom_reg
logistic_reg
boost_tree
surv_reg

linear_reg()
linear_reg() %>% set_engine('glmnet')
linear_reg() %>% set_engine('lm')
linear_reg() %>% set_engine('stan')
linear_reg() %>% set_engine('spark')
linear_reg() %>% set_engine('keras')

show_engines('linear_reg')

show_engines('logistic_reg')

logistic_reg() %>% set_engine('keras')

show_engines('boost_tree')
show_engines('surv_reg')

boost_tree() %>% set_engine('xgboost')
boost_tree() %>% set_engine('xgboost') %>% set_mode('classification')
boost_tree(mode='classification') %>% set_engine('xgboost')

xg_spec1 <- boost_tree(mode='classification', trees=100) %>% 
    set_engine('xgboost')
xg_spec1

# Workflows ###

# from {workflows}

flow1 <- workflow() %>% 
    add_recipe(rec1) %>% 
    add_model(xg_spec1)
flow1

fit1 <- fit(flow1, data=train)
fit1
fit1 %>% class()

fit1 %>% extract_model() %>% class()

# variable importance plot
fit1 %>% extract_model() %>% vip()

# How Did We Do? ####

# AIC, accuracy, logloss, AUC

# from {yardstick}

loss_fn <- metric_set(accuracy, roc_auc, mn_log_loss)
loss_fn

val_split <- validation_split(data=train, prop=0.8, strata='Status')
val_split
val_split$splits[[1]]
val_split$splits[[1]] %>% class()

# from {tune}

val1 <- fit_resamples(object=flow1, resamples=val_split, metrics=loss_fn)
val1
val1$.metrics[[1]]

val1 %>% collect_metrics()

# cross-validation