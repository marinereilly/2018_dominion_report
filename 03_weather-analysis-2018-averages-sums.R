#####load packages and data#####
library(dplyr)
library(lubridate)
library(ggplot2)

weather_2018<-readRDS("weather_2018.rds")

###Groups for Calculations
weather_2018$month<-month(weather_2018$datetime)
weather_2018$date<-date(weather_2018$datetime)
weather_2018$hour<-hour(weather_2018$datetime)

#####Cumulative Rainfall#####

###Cumulative Monthly Rainfall###

monthly_rain<-weather_2018 %>% 
  select(month, rainfall) %>% 
  group_by(month) %>% 
  summarise(., rain=sum(rainfall))
saveRDS(monthly_rain, "monthly_rain_2018.rds")

###Summary Stats###

weather_summary<-data.frame(total_rainfall=sum(weather_2018$rainfall), 
                            max_air=max(weather_2018$air_temp),
                            min_air=min(weather_2018$air_temp),
                            av_air=mean(weather_2018$air_temp),
                            total_hail=sum(weather_2018$hail))
weather_summary$year<-"2018"

###Daily rainfall###
daily_rainfall<-weather_2018 %>% 
  select(date, rainfall, hail) %>% 
  group_by(date) %>% 
  summarise_if(is.numeric, sum)
saveRDS(daily_rainfall, "daily-rain-hail-2018.rds")

###Daily Average Temperature and humidity
daily_average_temp_rh<-weather_2018 %>% 
  select(date, air_temp, rel_hum) %>% 
  group_by(date) %>% 
  summarise_if(is.numeric, mean)


View(daily_average_temp_rh)
