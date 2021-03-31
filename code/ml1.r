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
    facet_wrap(~ Status) + 
    theme(legend.position='bottom')
