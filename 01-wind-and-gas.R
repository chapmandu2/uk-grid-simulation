# This script contains the code for the analysis that underpins the first article I wrote on linkedin
# Link to code

# load libraries - these are very standard R libraries see https://www.tidyverse.org/
library(dplyr)
library(ggplot2)
library(readxl)

# load the data - downloaded from https://www.gridwatch.templar.co.uk/download.php
# filter by date so that subsequent analysis is consistent
dat <- readr::read_csv('~/Downloads/gridwatch.csv') %>%
  filter(timestamp < '2022-05-12', timestamp > '2012-01-01')

# check out the data - one record every 5 minutes, columns for each data source, 1.08M rows
dat
nrow(dat)

# we don't really need data at 5 minute intervals, so summarise data to hourly
# we also don't really need all of the power sources at this point
# find median of  each 1 hour interval
# this also makes conversion  from MW to MWh very simple
dat_1hr <- dat %>%
  dplyr::select(timestamp, demand, wind, solar, nuclear, ccgt) %>%
  dplyr::mutate(ts_hour = lubridate::floor_date(timestamp, 'hours')) %>%
  dplyr::group_by(ts_hour) %>%
  dplyr::summarise_if(is.numeric, median, na.rm = TRUE) %>%
  dplyr::ungroup() %>%
  dplyr::mutate_if(
    is.numeric,
    ~ifelse(is.na(.), 0, .)
  ) 

#now have a nuch smaller dataset - 90k rows
dat_1hr
nrow(dat_1hr)

# get some intuition about the data
dat_1hr %>%
  dplyr::filter(ts_hour > '2019-01-01', ts_hour < '2019-12-31') %>%
  ggplot(aes(ts_hour, demand)) + geom_line() + ylim(0,50000) + theme_bw()

dat_1hr %>%
  dplyr::filter(ts_hour > '2019-03-01', ts_hour < '2019-03-16') %>%
  ggplot(aes(ts_hour, demand)) + geom_line() + ylim(0,50000) + theme_bw()


# get some intuition about the data-  what does a year look like with demand and supply?
dat_1hr %>%
  dplyr::filter(ts_hour > '2019-01-01', ts_hour < '2019-12-31') %>%
  ggplot(aes(ts_hour, demand)) + 
    geom_line() + 
    geom_line(aes(y=wind), color = 'blue') +
    geom_line(aes(y=solar), color = 'orange') +
    geom_line(aes(y=nuclear), color = 'darkgrey') +
    geom_line(aes(y=ccgt), color = 'magenta') +    
    ylim(0,50000) + 
    theme_bw()

# what about a month
dat_1hr %>%
  dplyr::filter(ts_hour > '2019-03-01', ts_hour < '2019-03-16') %>%
  ggplot(aes(ts_hour, demand)) + 
    geom_line() + 
    geom_line(aes(y=wind), color = 'blue') +
    geom_line(aes(y=solar), color = 'orange') +
    geom_line(aes(y=nuclear), color = 'darkgrey') +
    geom_line(aes(y=ccgt), color = 'magenta') +    
    ylim(0,50000) + 
    theme_bw()

# and finally - what about a low wind month?
dat_1hr %>%
  dplyr::filter(ts_hour > '2022-03-01', ts_hour < '2022-04-01') %>%
  ggplot(aes(ts_hour, demand)) + 
  geom_line() + 
  geom_line(aes(y=wind), color = 'blue') +
  geom_line(aes(y=solar), color = 'orange') +
  geom_line(aes(y=nuclear), color = 'darkgrey') +
  geom_line(aes(y=ccgt), color = 'magenta') +    
  ylim(0,50000) + 
  theme_bw()

# note that 'demand' in gridwatch data is affected by unmetered wind a solar 
# this shows up as reductions in demand - eg household solar reduces that household's requirements

# so far we have seen that demand is variable - day/night, weekend vs weekday, winter vs summer.
# solar and wind are also variable, but gas seems to be able to ramp up and down around demand/RE variablility

# the next step of the analysis is to see what would happen with different grids
# to do this we need to normalise the wind and solar output - ie what would it have looked like if we 
# hadn't been building more over time.

# simplest way to do this is to use the cumulative maximum for wind and solar

# find max of each 1 hour interval and then cumulative max for wind and solar
max_vargen <- dat %>%
  dplyr::select(timestamp, demand, wind, solar) %>%
  dplyr::filter(solar < 100000) %>%  # gets rid of two rogue rows
  dplyr::mutate(ts_hour = lubridate::floor_date(timestamp, 'hours')) %>%
  dplyr::group_by(ts_hour) %>%
  dplyr::summarise_if(is.numeric, max, na.rm = TRUE) %>%
  dplyr::ungroup() %>%
  dplyr::mutate_if(
    is.numeric,
    ~ifelse(is.na(.), 0, .)
  ) %>%
  mutate(max_wind = cummax(wind),
         max_solar = cummax(solar)) %>%
  dplyr::select(ts_hour, max_wind, max_solar)
max_vargen

# combine median and max wind/solar so we can calculate % wind/solar
# this will be an overestimate of % since we will be likely be undestimating wind/solar capacity
# but in addition mix and location of wind has changed over time -> high capacity factors
# lets us normalise wind and solar output against max capacity available at time
normalised_vargen <- dat_1hr %>% 
  inner_join(max_vargen, by='ts_hour') %>%
  mutate(pct_wind =  wind/max_wind,
         pct_solar = solar/max_solar)

#plot normalised  wind output
ggplot(normalised_vargen %>% dplyr::filter(ts_hour > '2013-01-01'), aes(ts_hour, wind)) + 
  geom_line(aes(y=max_wind))  + 
  geom_line()  +
  theme_bw()
ggplot(normalised_vargen %>% dplyr::filter(ts_hour > '2013-01-01'), aes(ts_hour, pct_wind)) + 
  geom_line()  + theme_bw()

# plot normalised solar
ggplot(normalised_vargen %>% dplyr::filter(ts_hour > '2017-04-01'), aes(ts_hour, solar)) + 
  geom_line(aes(y=max_solar))  + 
  geom_line()  +
  theme_bw()
ggplot(normalised_vargen %>% dplyr::filter(ts_hour > '2017-04-01'), aes(ts_hour, pct_solar)) + geom_line()  + theme_bw()

# model 1 - simple model with just wind, gas, solar, no capacity constraints or overgeneration

#specify wind, solar and gas capacity
wind_cap_m1 <- 20000
gas_cap_m1 <- 50000
solar_cap_m1 <- 10000

# scaleup wind and solar to model capacity, then let gas fill in the gaps
model1 <- normalised_vargen %>%
  dplyr::filter(ts_hour > '2017-04-01') %>%
  dplyr::select(ts_hour, demand, pct_wind, pct_solar) %>%
  dplyr::mutate(solar = pct_solar * solar_cap_m1,
                wind = pct_wind * wind_cap_m1,
                gas = demand - wind - solar)

model1 %>%
  dplyr::filter(ts_hour > '2022-03-01', ts_hour < '2022-04-01') %>%
  ggplot(aes(ts_hour, demand)) +
  geom_line(color = 'black') +
  geom_line(aes(y=wind), color = 'blue') +
  geom_line(aes(y=solar), color = 'orange') +
  geom_line(aes(y=gas), color = 'magenta') +
  theme_bw()


# model 2 - add curtailment and excess
# need this model to look at what happens when we add more wind to the system.
# what happens when we have 'too much' wind for system to cope with?
# reach point of diminishing returns- most of new wind output curtailed but still have wind lull problem
# EV's could help by soaking up excess, interconnectors could export/import
# but this is the point where storage starts to help us use more of our otherwise curtailed wind
wind_cap_m2 <- 60000
gas_cap_m2 <- 50000
solar_cap_m2 <- 20000


model2 <- normalised_vargen %>%
  dplyr::filter(ts_hour > '2017-04-01') %>%
  dplyr::select(ts_hour, demand, pct_wind, pct_solar) %>%
  dplyr::mutate(solar = pct_solar * solar_cap_m2,
                wind = pct_wind * wind_cap_m2,
                gas = pmax(pmin(demand - wind - solar, gas_cap_m2), 0),
                deficit = pmax(demand - wind - solar - gas, 0),
                curtailment = pmax(wind + solar + gas - demand, 0))

model2 %>%
  dplyr::filter(ts_hour > '2021-01-01', ts_hour < '2022-01-01') %>%
  ggplot(aes(ts_hour, demand)) +
  geom_line(color = 'black') +
  geom_line(aes(y=wind), color = 'blue') +
  geom_line(aes(y=solar), color = 'orange') +
  geom_line(aes(y=gas), color = 'magenta') +
  geom_line(aes(y=-curtailment), color = 'red')  +
  theme_bw()

model2 %>%
  dplyr::filter(ts_hour > '2021-01-01', ts_hour < '2022-01-01') %>%
  dplyr::summarise_at(c('wind', 'solar', 'gas', 'curtailment', 'deficit'), ~sum(., na.rm = TRUE)/1000000)

model2 %>%
  dplyr::filter(ts_hour > '2022-03-01', ts_hour < '2022-04-01') %>%
  ggplot(aes(ts_hour, demand)) +
  geom_line(color = 'black') +
  geom_line(aes(y=wind), color = 'blue') +
  geom_line(aes(y=solar), color = 'orange') +
  geom_line(aes(y=gas), color = 'magenta') +
  geom_line(aes(y=-curtailment), color = 'red')  +
  theme_bw()

# now let's try to do the same thing but across a range of wind capacity

model2_fn <- function(df, start_date, end_date, wind_cap, gas_cap, solar_cap) {
  
  df %>%
    dplyr::filter(ts_hour >= start_date, ts_hour <= end_date) %>%
    dplyr::select(ts_hour, demand, pct_wind, pct_solar) %>%
    dplyr::mutate(solar = pct_solar * solar_cap,
                  wind = pct_wind * wind_cap,
                  gas = pmax(pmin(demand - wind - solar, gas_cap), 0),
                  deficit = pmax(demand - wind - solar - gas, 0),
                  curtailment = pmax(wind + solar + gas - demand, 0)) %>%
    dplyr::summarise_at(c('wind', 'solar', 'gas', 'curtailment', 'deficit'), ~sum(., na.rm = TRUE)/1000000)
  
}

normalised_vargen %>%
  model2_fn(
    start_date = '2017-01-01',
    end_date = '2022-05-12',
    wind_cap = 20000,
    gas_cap = 50000,
    solar_cap = 20000)

# do a full simulation with different amounts of wind and gas
sim_df <- tibble(
  input_data = list(normalised_vargen),
  start_date = '2017-01-01',
  end_date = '2022-05-12',
  solar_cap = 20000
) %>% tidyr::crossing(
  wind_cap = c(10000,20000, 40000, 80000, 120000),
  gas_cap = c(20000, 35000, 50000)
) %>%
  dplyr::mutate(
    sim_out = purrr::pmap(
      .f=model2_fn,
      .l=list(
        df=input_data, 
        start_date=start_date, 
        end_date=end_date, 
        wind_cap=wind_cap, 
        solar_cap=solar_cap, 
        gas_cap=gas_cap
        )
      )
    ) %>%
  tidyr::unnest(sim_out)

sim_df

# what does wind, solar, gas generaiton look like
sim_df %>%
  dplyr::select(wind_cap, gas_cap, wind, solar, gas) %>%
  tidyr::pivot_longer(cols = c('wind', 'solar', 'gas'), names_to = 'source', values_to = 'GWh') %>%
  ggplot(aes(as.factor(wind_cap), GWh, fill=source)) + geom_bar(stat='identity') + facet_wrap(~gas_cap) + theme_bw()

#what about deficit and curtailment
sim_df %>%
  dplyr::transmute(wind_cap, gas_cap, curtailment, deficit = -deficit) %>%
  tidyr::pivot_longer(cols = c('curtailment', 'deficit'), names_to = 'source', values_to = 'GWh') %>%
  ggplot(aes(as.factor(wind_cap), GWh, fill=source)) + geom_bar(stat='identity') + facet_wrap(~gas_cap) + theme_bw()

# finally consider amount of gas, deficit and curtailment
sim_df %>%
  dplyr::transmute(wind_cap, gas_cap, curtailment, gas, deficit=-deficit) %>%
  tidyr::pivot_longer(cols = c('curtailment',  'gas', 'deficit'), names_to = 'source', values_to = 'GWh') %>%
  ggplot(aes(as.factor(wind_cap), GWh, fill=source)) + geom_bar(stat='identity') + facet_wrap(~gas_cap) + theme_bw()



# we have two problems with a 

  
