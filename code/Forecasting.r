# Packages ####

# forecasting
library(fable)
# plotting
library(feasts)
library(ggplot2)
library(timetk)
# time series data manipulation
library(tsibble)
# data manipulation
library(dplyr)
library(tidyr)
library(purrr)


# Data ####

elec <- readr::read_csv('data/electricity_france.csv')
elec

elec <- elec %>% 
    as_tsibble(index=Date) %>% 
    mutate(Year=lubridate::year(Date)) %>% 
    filter_index('2007' ~ .)
