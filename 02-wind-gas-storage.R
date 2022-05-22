# This script contains the code for the analysis that underpins the second article I wrote on linkedin

###########
# NB First sections of code are more throughly documented in 01-wind-and-gas.R
###########

# load libraries - these are very standard R libraries see https://www.tidyverse.org/
library(dplyr)
library(ggplot2)
library(readxl)

# load the data - downloaded from https://www.gridwatch.templar.co.uk/download.php
# filter by date so that subsequent analysis is consistent
dat <- readr::read_csv('~/Downloads/gridwatch.csv') %>%
  filter(timestamp < '2022-05-12', timestamp > '2012-01-01')


# find median of  each 1 hour interval
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
normalised_vargen <- dat_1hr %>% 
  inner_join(max_vargen, by='ts_hour') %>%
  mutate(pct_wind =  wind/max_wind,
         pct_solar = solar/max_solar)

# model 3 - model with just wind, gas, solar, and storage

model3_fn <- function(df, start_date, end_date, wind_cap, gas_cap, solar_cap, battery_cap, battery_stor) {
  
  dat <- df %>%
    dplyr::filter(ts_hour >= start_date, ts_hour <= end_date) %>%
    dplyr::select(ts_hour, demand, pct_wind, pct_solar) %>%
    dplyr::mutate(solar = pct_solar * solar_cap,
                  wind = pct_wind * wind_cap,
                  vargen_deficit_bool  = demand > (solar + wind),
                  vargen_deficit = demand - solar - wind,
                  battery_store_pre = 0,
                  battery_store_post = 0,
                  battery = 0)
  
  
  # this bit is pretty hideous, 
  # not sure how to vectorise as each time period is dependent on the one before
  # might be better to at least refactor so central logic is seperate and can exist in different forms?
  for (i in 1:nrow(dat)) {
    dat$battery_store_pre[i] = ifelse(i==1, battery_stor, dat$battery_store_post[i-1])
    
    if (dat$vargen_deficit_bool[i]) {
      dat$battery[i] <- min(dat$battery_store_pre[i], battery_cap, dat$vargen_deficit[i], na.rm=TRUE)
    } else {
      dat$battery[i] <- -min(
        battery_stor - dat$battery_store_pre[i], 
        battery_cap, 
        -dat$vargen_deficit[i], 
        na.rm=TRUE)
    }
    
    dat$battery_store_post[i] <- dat$battery_store_pre[i] - dat$battery[i]
    
  }
  
  dat %>%  
    dplyr::mutate(gas = pmax(pmin(demand - wind - solar - battery, gas_cap), 0),
                  deficit = pmax(demand - wind - solar - battery - gas, 0),
                  curtailment = pmax(wind + solar + gas + battery - demand, 0))
  
}

test_out <- normalised_vargen %>%
  model3_fn(
    start_date = '2022-03-15',
    end_date = '2022-04-03',
    wind_cap = 80000,
    gas_cap = 50000,
    solar_cap = 20000,
    battery_cap = 24000,
    battery_stor =  500000)

test_out %>%
  ggplot(aes(ts_hour, demand)) +
    geom_line(aes(color = 'black')) +
    geom_line(aes(y=wind, color = 'blue')) +
    geom_line(aes(y=solar, color = 'orange')) +
    geom_line(aes(y=gas, color = 'magenta')) +
    geom_line(aes(y=battery, color = 'darkgreen')) +
    geom_line(aes(y=-deficit, color = 'gray'))  +
    geom_line(aes(y=-curtailment, color = 'red'))  +
    geom_area(aes(y=-battery_store_post/10, fill='lightblue'), alpha = 0.6) +
    xlab('') + ylab('Generation/Demand MWh') +
    theme_bw() +
    scale_color_identity(name = "",
                         breaks = c("black", "blue", "orange", "red", "darkgreen", "magenta"),
                         labels = c("demand", "wind", "solar", "curtailment", "storage", "ccgt"),
                         guide = "legend") +
    scale_fill_identity(name = '', guide='legend',
                      breaks = c('lightblue'),
                      labels = c('reservoir GWh/10'))

sim_out_summarise <- function(df) {
  df %>% 
    dplyr::mutate(battery_in = pmax(0, battery),
                  battery_out =  pmin(0, battery)) %>%
    dplyr::summarise_at(
      c('wind', 'solar', 'gas', 'curtailment', 'deficit', 'battery_store_post', 'battery_in', 'battery_out', 'demand'), 
      ~sum(., na.rm = TRUE)/1000000)
  }

sim_out_summarise(test_out)

# parallelise across 8 cores
library(furrr)
plan(multisession, workers = 8)

# do a full simulation with different amounts of wind and configurations of storage
# more GW and fewer GWh is 'battery-like' whereas fewer GW and more GWh is more 'pumped-hydro' like
# eg Coire Glas is 1.5GW/30GWh whereas intergen gateway battery project is 320MW/640MWh
sim1_df <- tibble(
  input_data = list(normalised_vargen),
  start_date = '2017-01-01',
  end_date = '2022-01-01',
  solar_cap = 20000,
  gas_cap = 50000,
) %>% tidyr::crossing(
  wind_cap = c(40000, 80000, 120000),
  battery_stor = c(0, 100000, 500000, 2000000),
  battery_cap = c(12000, 24000, 48000)
) %>%
  dplyr::mutate(
    sim_out = furrr::future_pmap(
      .f=model3_fn,
      .l=list(
        df=input_data, 
        start_date=start_date, 
        end_date=end_date, 
        wind_cap=wind_cap, 
        solar_cap=solar_cap, 
        gas_cap=gas_cap,
        battery_cap=battery_cap,
        battery_stor=battery_stor
        )
      )
    ) 

sim1_df_summarised <- sim1_df %>%
  dplyr::mutate(sim_summarised = purrr::map(sim_out, sim_out_summarise)) %>%
  dplyr::select(-input_data) %>%
  tidyr::unnest(sim_summarised)


sim1_df_summarised %>%
  dplyr::transmute(demand, solar_cap, gas_cap, wind_cap, battery_stor, battery_cap, gas_pct = gas/demand) %>%
  dplyr::arrange(desc(gas_pct))

# what does wind, solar, gas generaiton look like
# NB ADD DEMAND LINE AS PER @MhehedZherting suggestion
sim1_df_summarised %>%
  dplyr::select(wind_cap, gas_cap, wind, solar, gas, battery_stor, battery_cap, demand) %>%
  tidyr::pivot_longer(cols = c('wind', 'solar', 'gas'), names_to = 'source', values_to = 'TWh') %>%
  dplyr::mutate(battery_cap_gw = battery_cap/1000, battery_stor_gw = battery_stor/1000) %>%
  ggplot(aes(as.factor(wind_cap/1000), TWh, fill=source)) + 
    geom_bar(stat='identity') + 
    geom_hline(aes(yintercept=demand), linetype = 'dashed') +
    facet_grid(battery_cap_gw ~ battery_stor_gw, 
               labeller = labeller(
                 battery_cap_gw = function(val) {paste0(val, ' GW Storage')},
                 battery_stor_gw = function(val) {paste0(val, ' GWh Storage')}
                 )
               )  +
    xlab('Total Wind Capacity (GW)') +
    theme_bw() +
    scale_fill_manual(name = '', guide = 'legend',
                      values =c(
                      #  'demand' = 'black',
                        'wind'='blue',
                        'solar'='orange', 
                       # 'nuclear' = 'darkgrey', 
                        'gas' = 'magenta'
                      #  'curtailment' = 'red',
                       # 'deficit' = 'purple'
                      ))

#what about deficit and curtailment
sim1_df_summarised %>%
  dplyr::transmute(wind_cap, gas_cap, curtailment, deficit = -deficit, battery_stor, battery_cap) %>%
  tidyr::pivot_longer(cols = c('curtailment', 'deficit'), names_to = 'source', values_to = 'TWh') %>%
  dplyr::mutate(battery_cap_gw = battery_cap/1000, battery_stor_gw = battery_stor/1000) %>%
  ggplot(aes(as.factor(wind_cap/1000), TWh, fill=source)) + 
  geom_bar(stat='identity') + 
  facet_grid(battery_cap_gw ~ battery_stor_gw, 
             labeller = labeller(
               battery_cap_gw = function(val) {paste0(val, ' GW Storage')},
               battery_stor_gw = function(val) {paste0(val, ' GWh Storage')}
             )
  )  +
  xlab('Total Wind Capacity (GW)') +
  theme_bw() +
  scale_fill_manual(name = '', guide = 'legend',
                    values =c(
                      #  'demand' = 'black',
                      #'wind'='blue',
                      #'solar'='orange', 
                      # 'nuclear' = 'darkgrey', 
                      #'gas' = 'magenta'
                        'curtailment' = 'red',
                       'deficit' = 'purple'
                    ))

# finally consider amount of gas, deficit and curtailment
sim1_df_summarised %>%
  dplyr::transmute(wind_cap, gas_cap, curtailment, gas, deficit = -deficit, battery_stor, battery_cap) %>%
  tidyr::pivot_longer(cols = c('curtailment',  'gas', 'deficit'), names_to = 'source', values_to = 'TWh') %>%
  dplyr::mutate(battery_cap_gw = battery_cap/1000, battery_stor_gw = battery_stor/1000) %>%
  ggplot(aes(as.factor(wind_cap/1000), TWh, fill=source)) + 
  geom_bar(stat='identity') + 
  facet_grid(battery_cap_gw ~ battery_stor_gw, 
             labeller = labeller(
               battery_cap_gw = function(val) {paste0(val, ' GW Storage')},
               battery_stor_gw = function(val) {paste0(val, ' GWh Storage')}
             )
  )  +
  xlab('Total Wind Capacity (GW)') +
  theme_bw() +
  scale_fill_manual(name = '', guide = 'legend',
                    values =c(
                      #  'demand' = 'black',
                      #'wind'='blue',
                      #'solar'='orange', 
                      # 'nuclear' = 'darkgrey', 
                      'gas' = 'magenta',
                      'curtailment' = 'red',
                      'deficit' = 'purple'
                    ))


# what is % gas?
sim1_df_summarised %>%
  dplyr::transmute(wind_cap, gas_cap, gas, demand, battery_stor, battery_cap,
                   battery_cap_gw = battery_cap/1000, battery_stor_gw = battery_stor/1000,
                   wind_cap_gw = wind_cap/1000,
                   gas_pct = gas * 100/demand) %>%
  ggplot(aes(wind_cap_gw, gas_pct, color = as.factor(battery_stor_gw))) +
    geom_point() + geom_line() +
    facet_wrap(~battery_cap_gw, 
               labeller = labeller(
                 battery_cap_gw = function(val) {paste0(val, ' GW Storage')}
               )
    )  +
    guides(color=guide_legend(title="GWh Storage")) +
    xlab('Total Wind Capacity (GW)') + ylab('% Gas TWh') +
    theme_bw()
  

# what is % utilisation of gas?
sim1_df_summarised %>%
  dplyr::transmute(wind_cap, gas_cap, gas, demand, battery_stor, battery_cap,
                   battery_cap_gw = battery_cap/1000, battery_stor_gw = battery_stor/1000,
                   wind_cap_gw = wind_cap/1000,
                   gas_cf = gas * 100 * 1000/(gas_cap * 24 * 365)) %>%
  ggplot(aes(wind_cap_gw, gas_cf, color = as.factor(battery_stor_gw))) +
  geom_point() + geom_line() +
  facet_wrap(~battery_cap_gw, 
             labeller = labeller(
               battery_cap_gw = function(val) {paste0(val, ' GW Storage')}
             )
  )  +
  guides(color=guide_legend(title="GWh Storage")) +
  xlab('Total Wind Capacity (GW)') + ylab('Gas Utilisation Factor') +
  theme_bw()

# what is max capacity of gas required?
sim1_df_summarised %>%
  dplyr::transmute(wind_cap, gas_cap, gas, demand, battery_stor, battery_cap,
                   battery_cap_gw = battery_cap/1000, battery_stor_gw = battery_stor/1000,
                   wind_cap_gw = wind_cap/1000,
                   gas_max_cap = purrr::map_dbl(sim_out, ~max(.$gas))) %>%
  ggplot(aes(wind_cap_gw, gas_max_cap, color = as.factor(battery_stor_gw))) +
  geom_point() + geom_line() +
  facet_wrap( ~ battery_cap_gw, 
             labeller = labeller(
               battery_cap_gw = function(val) {paste0(val, ' GW Storage')}
             )
  )  +
  guides(color=guide_legend(title="GWh Storage")) +
  xlab('Total Wind Capacity (GW)') + ylab('Maximum Gas Output (GW)') +
  ylim(0, 50000) +
  theme_bw()

#why do you need more gas capacity when you have more storage capacity?! 
# with 12GW use part storage, part gas all the way through the wind lull
normalised_vargen %>%
  model3_fn(
    start_date = '2022-03-15',
    end_date = '2022-04-03',
    wind_cap = 120000,
    gas_cap = 50000,
    solar_cap = 20000,
    battery_cap = 12000,
    battery_stor =  2000000) %>%
    ggplot(aes(ts_hour, demand)) +
      geom_line(aes(color = 'black'), alpha=.3) +
      geom_line(aes(y=wind, color = 'blue'), alpha=.3) +
      geom_line(aes(y=solar, color = 'orange'), alpha=.3) +
      geom_line(aes(y=gas, color = 'magenta')) +
      geom_line(aes(y=battery, color = 'darkgreen')) +
      geom_line(aes(y=-deficit, color = 'gray'))  +
      geom_line(aes(y=-curtailment, color = 'red'), alpha=.3)  +
      geom_area(aes(y=-battery_store_post/50, fill='lightblue'), alpha = 0.6) +
      xlab('') + ylab('Generation/Demand MWh') +
      theme_bw() +
      ggtitle('March 2022 With 12GW/2000GWh storage') +
      scale_color_identity(name = "",
                           breaks = c("black", "blue", "orange", "red", "darkgreen", "magenta"),
                           labels = c("demand", "wind", "solar", "curtailment", "storage", "ccgt"),
                           guide = "legend") +
      scale_fill_identity(name = '', guide='legend',
                          breaks = c('lightblue'),
                          labels = c('reservoir GWh/50'))

# with 48GW use all storage, no gas most of the way through the wind lull 
# but then all gas, no storage at the end when the storage runs dry!!
normalised_vargen %>%
  model3_fn(
    start_date = '2022-03-15',
    end_date = '2022-04-03',
    wind_cap = 120000,
    gas_cap = 50000,
    solar_cap = 20000,
    battery_cap = 48000,
    battery_stor =  2000000) %>%
    ggplot(aes(ts_hour, demand)) +
      geom_line(aes(color = 'black'), alpha=.3) +
      geom_line(aes(y=wind, color = 'blue'), alpha=.3) +
      geom_line(aes(y=solar, color = 'orange'), alpha=.3) +
      geom_line(aes(y=gas, color = 'magenta')) +
      geom_line(aes(y=battery, color = 'darkgreen')) +
      geom_line(aes(y=-deficit, color = 'gray'))  +
      geom_line(aes(y=-curtailment, color = 'red'), alpha=.3)  +
      geom_area(aes(y=-battery_store_post/50, fill='lightblue'), alpha = 0.6) +
      xlab('') + ylab('Generation/Demand MWh') +
      theme_bw() +
      ggtitle('March 2022 With 48GW/2000GWh storage') +
      scale_color_identity(name = "",
                           breaks = c("black", "blue", "orange", "red", "darkgreen", "magenta"),
                           labels = c("demand", "wind", "solar", "curtailment", "storage", "ccgt"),
                           guide = "legend") +
      scale_fill_identity(name = '', guide='legend',
                          breaks = c('lightblue'),
                          labels = c('reservoir GWh/50'))

# also worth noting that more  solar could have helped here by keeping storage topped up, even in march
normalised_vargen %>%
  model3_fn(
    start_date = '2022-03-15',
    end_date = '2022-04-03',
    wind_cap = 120000,
    gas_cap = 50000,
    solar_cap = 50000,
    battery_cap = 48000,
    battery_stor =  2000000) %>%
    ggplot(aes(ts_hour, demand)) +
      geom_line(aes(color = 'black'), alpha=.3) +
      geom_line(aes(y=wind, color = 'blue'), alpha=.3) +
      geom_line(aes(y=solar, color = 'orange'), alpha=1) +
      geom_line(aes(y=gas, color = 'magenta')) +
      geom_line(aes(y=battery, color = 'darkgreen')) +
      geom_line(aes(y=-deficit, color = 'gray'))  +
      geom_line(aes(y=-curtailment, color = 'red'), alpha=.3)  +
      geom_area(aes(y=-battery_store_post/50, fill='lightblue'), alpha = 0.6) +
      xlab('') + ylab('Generation/Demand MWh') +
      theme_bw() +
      ggtitle('March 2022 With 48GW/2000GWh storage + 50GW solar') +
      scale_color_identity(name = "",
                           breaks = c("black", "blue", "orange", "red", "darkgreen", "magenta"),
                           labels = c("demand", "wind", "solar", "curtailment", "storage", "ccgt"),
                           guide = "legend") +
      scale_fill_identity(name = '', guide='legend',
                          breaks = c('lightblue'),
                          labels = c('reservoir GWh/50'))
  
# in reality the most cost effecive option would be for storage and gas (or biomass) to behave differently
# pricing mechanisms would try to get the balance right between running storage running down to avoid
# using gas/biomass and ensuring that excess renewables can be absorbed once wind lull is over
# and on the other hand ensuring that storage doesn't run down entirely
# a smaller, more amount of 'backup' capacity might therefore be retained that runs more often in 'top up' mode

# the benefit of retaining a larger amount of gas capacity is the GUARANTEE that there will never be 
# a shortfall, but this comes at the cost of retaining this capacity

# also have the option of interconnectors to import and export
# haven't included in this analysis since it is unclear in times of low RE generation whether import would be
# possible since other countries would also be suffering from low RE generation (esp wind)
# may help imore n summer periods - other countries running nukes, better solar resources, lower demand generally

# where is gas most used?
normalised_vargen %>%
  model3_fn(
    start_date = '2019-01-01',
    end_date = '2019-12-31',
    wind_cap = 80000,
    gas_cap = 50000,
    solar_cap = 10000,
    battery_cap = 24000,
    battery_stor =  500000) %>%
  ggplot(aes(ts_hour, demand)) +
    geom_line(color = 'black') +
    geom_line(aes(y=wind), color = 'blue', alpha = 0.2) +
    geom_line(aes(y=solar), color = 'orange', alpha = 0.2) +
    geom_line(aes(y=gas), color = 'magenta') +
    geom_line(aes(y=battery), color = 'darkgreen', alpha = 0.2) +
    geom_line(aes(y=-curtailment), color = 'red', alpha = 0.2)  +
    geom_area(aes(y=-battery_store_post/30), fill = 'lightblue', alpha = 0.6) +
    theme_bw()
