
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

# calculate the change in MW output per hour expressed as a % of maximum output seen in 2021 
dat_1hr %>%
  dplyr::filter(ts_hour < '2021-12-31', ts_hour > '2021-01-01') %>%
  dplyr::mutate_at(c('demand', 'wind', 'nuclear', 'solar', 'ccgt'), ~ (. - dplyr::lag(.))/max(.)) %>%
  tidyr::pivot_longer(-ts_hour, names_to='source', values_to='pct_ramp') %>%
  dplyr::filter(!source %in% c('nuclear', 'solar')) %>%
  ggplot(aes(x=pct_ramp, color=source)) + geom_density() + theme_bw() + xlim(-.2, .2)

                   