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


# Transformations ####

elec %>% autoplot(ActivePower)
elec %>% autoplot(log(ActivePower))

elec %>% autoplot(box_cox(ActivePower, lambda=1.7))
elec %>% autoplot(box_cox(ActivePower, lambda=0.7))
elec %>% autoplot(box_cox(ActivePower, lambda=0.07))
elec %>% autoplot(box_cox(ActivePower, lambda=0.0))

# feasts
elec %>% 
    features(ActivePower, features=guerrero)

elec %>% autoplot(box_cox(ActivePower, lambda=0.67))

# Fitted Values and Residuals ####

simple_mods %>% augment()

# Prediction Intervals ####

snaive_mod %>% forecast(h=10) %>% hilo()
snaive_mod %>% forecast(h=10) %>% hilo(level=95)

# Evaluating Model ####

snaive_augment <- snaive_mod %>% augment()
snaive_augment

mean_augment <- mean_mod %>% augment()
mean_augment

mean_augment %>% autoplot(.resid)

mean_mod %>% gg_tsresiduals()

train <- elec %>% 
    filter_index(. ~ '2010-08-31')
test <- elec %>% 
    filter_index('2010-09-01' ~ .)

train_mods <- train %>% 
    model(
        Mean=MEAN(ActivePower),
        SNaive=SNAIVE(ActivePower ~ lag('year'))
    )        
train_mods

train_mods %>% forecast(h=nrow(test))
train_mods %>% forecast(new_data=test)


train_forecast <- train_mods %>% forecast(new_data=test)
train_forecast %>% 
    autoplot(train %>% filter_index('2010'), level=NULL) +
    autolayer(test, ActivePower) + 
    facet_wrap(~.model, ncol=1)

accuracy(train_forecast, test)


# STL ####

# Seasonality, Trend, L (loess)

elec %>% 
    model(
        stl=STL(ActivePower ~ trend(window=7))
    ) %>% 
    components() %>% 
    autoplot()

# ETS ####

# Error, Trend, Seasonality

# Error: additive, multiplicative, none
# trend: additive, multiplicative, none
# seasonality: additive, multiplicative, none

ets_mod <- elec %>% 
    model(
        ana=ETS(ActivePower ~ error('A') + trend('N') + season('A')),
        aan=ETS(ActivePower ~ error('A') + trend('A') + season('N')),
        aaa=ETS(ActivePower ~ error('A') + trend('A') + season('A')),
        ama=ETS(ActivePower ~ error('A') + trend('M') + season('A'))
    )

ets_mod
ets_mod %>% select(ana) %>% report()
ets_mod %>% glance()
ets_mod %>% glance() %>% slice_min(AICc)

ets_mod %>% forecast(h=90) %>% autoplot(elec2010)
ets_mod %>% forecast(h=90) %>% autoplot(elec2010, level=NULL)
ets_mod %>% 
    select(-ama) %>% 
    forecast(h=90) %>% autoplot(elec2010, level=NULL)
ets_mod %>% 
    select(-ama) %>% 
    forecast(h=90) %>% autoplot(elec2010)

# error, trend, seasonlity
# additive, multiplicative, none
# 27 combinations

ets_auto <- elec %>% 
    model(
        auto=ETS(ActivePower)
    )
ets_auto %>% report()

ets_auto %>% 
    forecast(h=90) %>% 
    autoplot(elec2010)

# Differencing ####

elec %>% autoplot()
elec_diff <- elec %>% 
    mutate(
        PowerDiff=difference(ActivePower),
        PowerDiff2=difference(ActivePower, differences=2)
    )
elec_diff %>% select(ActivePower, PowerDiff, PowerDiff2)
1269-2749
1014-1269
3259-1014

-255 - -1480
2245 - -255
-546 - 2245

elec_diff %>% 
    autoplot(ActivePower) + 
    autolayer(elec_diff, PowerDiff, color='green') + 
    autolayer(elec_diff, PowerDiff2, color='blue', alpha=1/3)

elec %>% 
    mutate(PowerLag=difference(ActivePower, lag=2)) %>% 
    select(ActivePower, PowerLag)
1014-2749
3259-1269

# log difference = log(today) - log(yesterday) = log(today/yesterday) = log returns

# ARIMA ####

# autoregressive integrated moving average

# y_t ~ y_{t-1}

elec %>% 
    mutate(Lag=lag(ActivePower)) %>% 
    select(ActivePower, Lag) %>% 
    lm(ActivePower ~ Lag, data=.)

# ARIMA(p,d,q)
# ARIMA(p,d,q)(P,D,Q)

arima_323 <- elec %>% 
    model(arima=ARIMA(ActivePower ~ pdq(3, 2, 3) + PDQ(0, 0, 0))
    )
arima_323 %>% report()

arima_323 %>% forecast(h=90) %>% autoplot(elec2010)

arima_212_100 <- elec %>% 
    model(
        ARIMA(ActivePower ~ pdq(2, 1, 2) + PDQ(1, 0, 0))
    )
arima_212_100 %>% forecast(h=90) %>% autoplot(elec2010)

arima_mod <- elec %>% 
    model(
        ARIMA(ActivePower)
    )
arima_mod %>% report()
arima_mod %>% forecast(h=90) %>% autoplot(elec2010)

arima_mod2 <- elec %>% 
    model(
        ARIMA(ActivePower ~ pdq(0:4, 0:2, 0:4) + PDQ(0:3, 0:2, 0:3))
    )
arima_mod2 %>% report()

arima_mod3 <- elec %>% 
    model(
        ARIMA(ActivePower ~ pdq(0:4, 0:2, 0:4) + PDQ(0:3, 0:2, 0:3),
              stepwise=FALSE, approximation=FALSE, greedy=FALSE)
    )
arima_mod3 %>% report()
arima_mod3 %>% forecast(h=90) %>% autoplot(elec2010)

arima_mod3 %>% gg_tsresiduals()
