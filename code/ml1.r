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

# two primary ways to deal with imbalanced data:
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

# split training data into training.1 and val
# fit a model on training.1
# use val and model to make predictions
# compare predictions from model with true outcome variable in val
# use AUC, accuracy or log loss for that comparison

# or use fit_resamples()

# cross-validation

cv_split <- vfold_cv(data=train, v=5, strata='Status')
cv_split
cv_split %>% class()
cv_split$splits[[1]]
cv_split$splits[[1]] %>% class()

val1 <- fit_resamples(object=flow1, resamples=val_split, metrics=loss_fn)
cv1 <- fit_resamples(object=flow1, resamples=cv_split, metrics=loss_fn)
cv1
cv1$.metrics[[1]]
cv1$.metrics[[2]]

cv1 %>% collect_metrics()

xg_spec2 <- boost_tree(mode='classification', trees=300) %>% 
    set_engine('xgboost')

xg_spec1
xg_spec2

flow2 <- flow1 %>% 
    update_model(xg_spec2)
flow1
flow2

val2 <- fit_resamples(object=flow2, resamples=val_split, metrics=loss_fn)
val2
val1

val1 %>% collect_metrics()
val2 %>% collect_metrics()

xg_spec3 <- boost_tree(mode='classification', 
                       trees=300, learn_rate=0.15) %>% 
    set_engine('xgboost')

flow3 <- flow2 %>% 
    update_model(xg_spec3)

val3 <- fit_resamples(flow3, resamples=val_split, metrics=loss_fn)

val3 %>% collect_metrics()

# More Recipes ####

rec2 <- recipe(Status ~ ., data=train) %>% 
    # xgboost can handle, will remove later
    themis::step_downsample(Status, under_ratio=1.2) %>% 
    step_other(all_nominal(), -Status, other='Misc') %>% 
    # remove columns with very little variance
    # as opposed to step_zv
    step_nzv(all_predictors()) %>% 
    step_dummy(all_nominal(), -Status, one_hot=TRUE)

flow4 <- flow3 %>% 
    update_recipe(rec2)
flow4

val4 <- fit_resamples(flow4, resamples=val_split, metrics=loss_fn)
val4 %>% collect_metrics()

# Imbalanced Data ####

rec3 <- recipe(Status ~ ., data=train) %>% 
    step_other(all_nominal(), -Status, other='Misc') %>% 
    # remove columns with very little variance
    # as opposed to step_zv
    step_nzv(all_predictors()) %>% 
    step_dummy(all_nominal(), -Status, one_hot=TRUE)

# scale_pos_weight

scaler <- train %>% 
    count(Status) %>% 
    pull(n) %>%
    rev() %>%
    reduce(`/`)
2561/1004
scaler

xg_spec5 <- boost_tree(mode='classification', 
                       trees=300, learn_rate=0.15) %>% 
    set_engine('xgboost', scale_pos_weight=!!scaler)
xg_spec5

flow5 <- flow4 %>% 
    update_model(xg_spec5)
flow5

val5 <- fit_resamples(flow5, resamples=val_split, metrics=loss_fn)

val4 %>% collect_metrics()
val5 %>% collect_metrics()

# Tune Parameters ####

# from {tune} and {dials}

xg_spec6 <- boost_tree(
    mode='classification',
    learn_rate=0.15,
    tree_depth=4,
    trees=tune()
) %>% 
    set_engine('xgboost', scale_pos_weight=!!scaler)
xg_spec6

flow6 <- flow5 %>% 
    update_model(xg_spec6)
flow6

# fails
# because our workflow has a parameter that needs tuning
val6 <- fit_resamples(flow6, resamples=val_split, metrics=loss_fn)
# neither will fit()

# run in parallel
registerDoFuture()
cl <- makeCluster(6)
plan(cluster, workers=cl)

# only if using a dark theme
options(tidymodels.dark=TRUE)

tic()
tune6_val <- tune_grid(
    flow6, 
    resamples=val_split,
    grid=30,
    metrics=loss_fn,
    control=control_grid(verbose=TRUE, allow_par=TRUE)
)
toc()
tune6_val %>% collect_metrics()

tune6_val %>% show_best(metric='roc_auc')
