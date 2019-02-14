#####Load packages#####
library(dplyr)
library(lubridate)
library(ggplot2)

#####Load the RDS files made in script 01#####
wq_full<-readRDS("wq_2018_full.rds")

#####wrangling for different purposes#####
###daily means for temperature, salinity and depth
wq_full$date<-date(wq_full$datetime)

wq_daily_means<-wq_full %>% 
  select(date, station, depth_ft, depth_m, salinity, temperature, temp_wl) %>% 
  group_by(., date, station) %>% 
  summarize_if(is.numeric, mean, na.rm=TRUE)
wq_daily_means$station<-as.factor(wq_daily_means$station)
 
saveRDS(wq_daily_means, "wq_daily_means.rds")

###hourly means for temperature salinity and depth###

wq_full$date<-date(wq_full$datetime)
wq_full$hour<-hour(wq_full$datetime)

wq_hourly_means<-wq_full %>% 
  select(date, hour, station, depth_ft, depth_m, salinity, temperature, temp_wl) %>% 
  group_by(station, date, hour) %>% 
  summarise_if(is.numeric, mean, na.rm=TRUE)

wq_hourly_means$datetime<-paste0(wq_hourly_means$date, " ", wq_hourly_means$hour, ":00")
wq_hourly_means$datetime<-ymd_hm(wq_hourly_means$datetime)
wq_hourly_means$station <- as.factor(wq_hourly_means$station)

saveRDS(wq_hourly_means, "wq_hourly_means.rds")

###Hypoxia instances###
hypoxia<-wq_full %>% 
  filter(DO_mg<= 4)
saveRDS(hypoxia, "hypoxia_full.rds")

wq_full$date<-date(wq_full$datetime)
wq_full$hour<-hour(wq_full$datetime)

hourly_hypoxia<-wq_full %>% 
  select(date, hour, station, DO_mg) %>% 
  group_by(station, date, hour) %>% 
  summarise_if(is.numeric, mean, na.rm=TRUE) %>% 
  filter(DO_mg<=4)
hourly_hypoxia$datetime<-ymd_hm(paste0(hourly_hypoxia$date, " ", hourly_hypoxia$hour, ":00"))
hourly_hypoxia$light<-if_else(hourly_hypoxia$hour<7|hourly_hypoxia$hour>19, "night", "day")
saveRDS(hourly_hypoxia, "hourly_hypoxia.rds")

daily_hypoxia<-wq_full %>% 
  select(date, station, DO_mg) %>% 
  group_by(station, date) %>% 
  summarise_if(is.numeric, mean, na.rm=TRUE) %>% 
  filter(DO_mg<=4)
saveRDS(daily_hypoxia, "daily_hypoxia.rds")

do_temp_hourly<-wq_full %>% 
  select(date, hour, station, DO_mg, temperature, temp_wl) %>% 
  group_by(station, date, hour) %>% 
  summarise_if(is.numeric, mean, na.rm=TRUE)
saveRDS(do_temp_hourly, "do_temp_hourly.rds")
