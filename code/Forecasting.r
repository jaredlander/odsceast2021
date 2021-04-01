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

# ACF ####

# Autocorrelation function
elec %>% ACF(ActivePower)
elec %>% ACF(ActivePower) %>% autoplot()

plot_acf_diagnostics(elec, .date_var=date, .value=ActivePower, .lags=40)

# Simple Forecasting ####

elec %>% autoplot(ActivePower)

naive_mod <- elec %>% 
    model(Naive=NAIVE(ActivePower))
naive_mod
naive_mod %>% select(Naive) %>% report()
naive_mod %>% forecast(h=90)

naive_mod %>% forecast(h=90) %>% autoplot()
naive_mod %>% forecast(h=90) %>% autoplot(elec)
naive_mod %>% forecast(h=90) %>% autoplot(elec %>% filter_index('2010' ~ .))

naive_mod %>% forecast(h='90 days')
naive_mod %>% forecast(h='3 months')                                          

mean_mod <- elec %>% 
    model(Mean=MEAN(ActivePower))
mean_mod

elec2010 <- elec %>% filter_index('2010' ~ .)
mean_mod %>% forecast(h=90) %>% autoplot(elec2010)

snaive_mod <- elec %>% 
    model(SNaive=SNAIVE(ActivePower ~ lag('month') + lag('year') + lag('week')))
snaive_mod %>% forecast(h=90) %>% autoplot(elec2010)          

simple_mods <- elec %>% 
    model(
        Mean=MEAN(ActivePower),
        Naive=NAIVE(ActivePower),
        SNaive=SNAIVE(ActivePower ~ lag('month') + lag('year') + lag('week'))
    )

simple_mods
simple_mods %>% select(SNaive) %>% report()
simple_mods %>% glance()

simple_mods %>% forecast(h=90)
simple_mods %>% forecast(h=90) %>% View
simple_mods %>% forecast(h=90) %>% autoplot(elec2010)
simple_mods %>% forecast(h=90) %>% autoplot(elec2010, level=NULL)
