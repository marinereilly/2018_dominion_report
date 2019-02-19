#####load packages#####
library(dplyr)
library(lubridate)
library(ggplot2)

#####load data#####
weather_2018<-readRDS("weather_2018.rds")
weather_2010_2017<-read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/ysi_noaa_wq_weather_tides/data/previously_downloaded/YSI data/R Formatted Data/MET_all.csv", stringsAsFactors=FALSE)

#####Prep data for joining####
colnames(weather_2018)
colnames(weather_2010_2017)

weather_2010_2017<- weather_2010_2017 %>% 
  select(datetime=DateTime, air_temp=AirTemp, bp=BP, hail=Hail, rainfall=RainFall, 
         rel_hum=RH, wind_dir=WindDir, wind_spd=WindSpd, wind_max=WindGust, wind_min=WindLull)
weather_2010_2017$datetime<-ymd_hms(weather_2010_2017$datetime)

weather<-weather_2010_2017 %>% 
  full_join(weather_2018)

#####Water Year Things #####
wy_rain<-weather_2018 %>% 
  select(date, rainfall)