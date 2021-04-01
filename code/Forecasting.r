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

# Time Series Formats ####

ts_elec <- ts(elec$ActivePower, start=min(elec$Date), end=max(elec$Date))
ts_elec
ts_elec %>% class()
ts_elec %>% plot()

xts_elec <- xts::as.xts(ts_elec)
xts_elec
xts_elec %>% class()

xts_elec %>% plot()

# Visualization ####

# from {feats}

elec %>% autoplot(ActivePower)
elec %>% gg_season(ActivePower, period='year')

# from {timetk}

elec %>% 
    plot_time_series(
        .date_var=Date, .value=ActivePower)
elec %>% 
    plot_time_series(
        .date_var=Date, .value=ActivePower, 
        .interactive=FALSE
    )

elec %>% 
    plot_time_series(
        .date_var=Date, .value=ActivePower, 
        .color_var=Year,
        .interactive=TRUE
    )

elec %>% 
    plot_time_series(
        .date_var=Date, .value=ActivePower, 
        .color_var=Year,
        .facet_vars=Year, .facet_scales='free_x',
        .smooth=FALSE,
        .interactive=TRUE
    )

p <- elec %>% 
    plot_time_series(
        .date_var=Date, .value=ActivePower, 
        .color_var=Year,
        .facet_vars=Year, .facet_scales='free_x',
        .smooth=FALSE,
        .interactive=TRUE
    )
plotly::toWebGL(p)


