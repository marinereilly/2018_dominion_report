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

saveRDS(weather, "weather_2010_2018.rds")

#####Load Joined Data####
weather<-readRDS("weather_2010_2018.rds")

weather$year<-year(weather$datetime)
weather$doy<-yday(weather$datetime)
weather$date<-date(weather$datetime)
weather$month<-month(weather$datetime)
weather$hour<-hour(weather$datetime)

#####Water Year Things #####
water_year<-weather %>% 
  select(date, year, doy, month, rainfall) %>% 
  group_by(date, doy, year, month) %>% 
  summarise(rainfall=sum(rainfall)) %>% 
  mutate(water_year=if_else(month<10, year, year+1))

water_year<-water_year %>% 
  group_by(water_year) %>% 
  arrange(date) %>% 
  mutate(wy_cum_rainfall = cumsum(rainfall))

saveRDS(water_year, "water_year_2010_2018.rds")

#####Daily Average Temperatures####
daily_air_temp <- weather %>% 
  select(date, air_temp) %>% 
  group_by(date) %>% 
  summarise(air_temp=mean(air_temp))
daily_air_temp$doy<-yday(daily_air_temp$date)
daily_air_temp$year <- year(daily_air_temp$date)

saveRDS(daily_air_temp, "daily_air_temp_2010_2018.rds")

####Storms#####
r_storms<-weather %>% 
  select(date, hour, rainfall) %>% 
  group_by(date, hour) %>% 
  summarise(rainfall=sum(rainfall)) %>% 
  filter(rainfall>=0.3)
r_storms$datetime<-ymd_h(paste0(r_storms$date, " ",r_storms$hour))
w_storms <- weather %>% 
  select(date, hour, wind_spd) %>%
  group_by(date, hour) %>% 
  summarise(wind_spd=mean(wind_spd))%>% 
  filter(wind_spd>=25)
w_storms$datetime<-ymd_h(paste0(w_storms$date, " ",w_storms$hour))
storms<-  w_storms %>% 
  full_join(., r_storms)
storms<-storms %>% 
  mutate(code=if_else(is.na(wind_spd)==TRUE,"r",
                      if_else(is.na(rainfall)==TRUE, "w","b")))
s_weather<-storms %>% 
  select(datetime, code) %>% 
  full_join(weather, .)
####Winds####
wind<-weather %>% 
  select(datetime, wind_dir, wind_max, wind_spd, wind_min, year, doy, date, hour)
wind<-wind %>% 
  mutate(group_dir=case_when(
    wind_dir<=22                   ~ 0,
    wind_dir>22 & wind_dir<= 68    ~ 45,
    wind_dir>68 & wind_dir<= 112   ~ 90,
    wind_dir>112 & wind_dir<= 158  ~ 135,
    wind_dir>158 & wind_dir<= 202  ~ 180,
    wind_dir>202 & wind_dir<= 248  ~ 225,
    wind_dir>248 & wind_dir<= 292  ~ 270,
    wind_dir>292 & wind_dir<= 338  ~ 315,
    wind_dir>338                   ~ 0
  ))

wind_freq<-wind %>% 
  group_by(year) %>% 
  count(., group_dir)
wind_zero<-wind_freq %>% 
  filter(group_dir==0)
wind_zero$group_dir<-360
wind_freq<-wind_freq %>% 
  bind_rows(., wind_zero)  

saveRDS(wind_freq, "windfrequency2010-2018.rds")

rm(wind_zero)

high_wind<-wind %>% 
  filter(wind_spd>=18)
high_wind_freq<-high_wind %>% 
  group_by(year) %>% 
  count(., group_dir)
highwind_zero<-high_wind_freq %>% 
  filter(group_dir==0)
highwind_zero$group_dir=360
high_wind_freq<-high_wind_freq %>% 
  bind_rows(., highwind_zero)
saveRDS(high_wind_freq, "high_winds_2010_2018.rds")

rm(highwind_zero)

beaufort<-wind %>% 
  mutate(beaufort=case_when(
    wind_spd <= 1                      ~ 0,
    wind_spd <= 4 & wind_spd > 1       ~ 1,
    wind_spd <= 7 & wind_spd > 4       ~ 2,
    wind_spd <= 12 & wind_spd > 7      ~ 3,
    wind_spd <= 18 & wind_spd > 12     ~ 4,
    wind_spd <= 23 & wind_spd > 18     ~ 5,
    wind_spd <= 30 & wind_spd > 23     ~ 6,
    wind_spd <= 38 & wind_spd > 30     ~ 7,
    wind_spd <= 46 & wind_spd > 38     ~ 8,
    wind_spd <= 55 & wind_spd > 46     ~ 9
  ))
beaufort_freq<-beaufort %>% 
  group_by(year, beaufort) %>% 
  count(., group_dir)
saveRDS(beaufort_freq, "wind_beaufort_scale_2010_2018.rds")


###Test plots###
r<-storms %>% 
  filter(!is.na(wind_spd))
w<-storms %>% 
  filter(!is.na(rainfall))
b<-storms %>% 
  filter(!is.na(rainfall)) %>% 
  filter(!is.na(wind_spd))

a<-ggplot(storms)+geom_point(aes(x=date, y=1, color=code))
a

a<-ggplot()+geom_point(data=r, aes(x=date, y=1), color="deepskyblue")+
  geom_point(data=w, aes(x=date, y=2), color="springgreen")+
  geom_point(data=b, aes(x=date, y=3), color= "orange")
a


