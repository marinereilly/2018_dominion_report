#####load packages#####
library(dplyr)
library(lubridate)
library(ggplot2)

#####load data#####
weather_2018<-readRDS("weather_2018.rds")
weather_2010_2017<-readRDS("met_2010_2017.rds")

#####Prep data for joining####
colnames(weather_2018)
colnames(weather_2010_2017)

weather_2010_2017<- weather_2010_2017 %>% 
  select(datetime=DateTime, air_temp=Air_Temp, bp=BP, hail=Hail, rainfall=Rainfall, 
         rel_hum=RH, wind_dir=Wind_Dir, wind_spd=Wind_spd, wind_max=Wind_Max, wind_min=Wind_Min)


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
weather$day<-day(weather$datetime)

#####Water Year Things #####
water_year<-weather %>% 
  select(date, day, year, doy, month, rainfall) %>% 
  group_by(date, day, doy, year, month) %>% 
  summarise(rainfall=sum(rainfall)) %>% 
  mutate(water_year=if_else(month<10, year, year+1))

water_year<-water_year %>% 
  group_by(water_year) %>% 
  arrange(date) %>% 
  mutate(wy_cum_rainfall = cumsum(rainfall)) %>% 
  ungroup()

water_year$days2<-if_else(water_year$month<10, 
                            paste0(water_year$month, "-", water_year$day, "-", 2000),
                          paste0(water_year$month, "-", water_year$day, "-", 1999))
water_year$days2<-mdy(water_year$days2)
water_year$water_year<-as.factor(water_year$water_year)

a<-daily_air_temp2 %>% 
  filter(date>= as.POSIXct("2017-01-01")) %>% 
  ggplot(.)+
  geom_line(aes(x=date, y=air_temp, color=year))
a

saveRDS(water_year, "water_year_2010_2018.rds")
#####Monthly Rainfall####
monthly_rainfall<-weather %>% 
  select(month, year, rainfall) %>% 
  group_by(month, year) %>% 
  summarise(rainfall=sum(rainfall)) %>% 
  ungroup()
monthly_rainfall$month_abb<-month.abb[monthly_rainfall$month]
monthly_rainfall$month_abb<-factor(monthly_rainfall$month_abb, levels = month.abb)
monthly_rainfall$year<-as.factor(monthly_rainfall$year)
monthly_rainfall$month<-month(monthly_rainfall$month)
monthly_rainfall<-monthly_rainfall %>%
  group_by(year) %>% 
  mutate(cum_month=cumsum(rainfall))

saveRDS(monthly_rainfall, "monthly_rainfall_2010_2018.rds")

#####Daily Average Temperatures####
daily_air_temp <- weather %>% 
  select(date, air_temp) %>% 
  group_by(date) %>% 
  summarise(air_temp=mean(air_temp))
daily_air_temp$doy<-yday(daily_air_temp$date)
daily_air_temp$year <- year(daily_air_temp$date)
#removing temperatures between jan 1 2018 and april 9 2018 
daily_air_temp<-daily_air_temp %>% 
  filter(!c(date>= as.POSIXct("2018-01-01") & date<=as.POSIXct("2018-04-09")))



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
saveRDS(s_weather, "weather_2010_2018_storms.rds")
####Winds####
wind<-weather %>% 
  select(datetime, wind_dir, wind_max, wind_spd, wind_min)
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

wind$year<-lubridate::year(wind$datetime)

wind_freq<-wind %>% 
  group_by(year) %>% 
  count(., group_dir)
wind_zero<-wind_freq %>% 
  filter(group_dir==0)
wind_zero$group_dir<-360
wind_freq<-wind_freq %>% 
  bind_rows(., wind_zero)  

saveRDS(wind_freq, "windfrequency2010-2018.rds")

wind$month<-lubridate::month(wind$datetime)

wind$season<-case_when(
  wind$month<=2                    ~ "Winter",
  wind$month>2 & wind$month<=5     ~ "Spring",
  wind$month>5 & wind$month<=8     ~ "Summer",
  wind$month>8 & wind$month<=11    ~ "Autumn",
  wind$month ==12                  ~ "Winter")

wind_season<-wind %>%
  group_by(year, season) %>% 
  count(., group_dir)
season_zero<-wind_season %>% 
  filter(group_dir==0)
season_zero$group_dir<-360
wind_season<-wind_season %>% 
  bind_rows(., season_zero)
saveRDS(wind_season, "seasonal_wind_freq_2010_2018.rds")
  
highseason<-wind %>% 
  filter(wind_spd>=18) %>% 
  group_by(year,season) %>% 
  count(., group_dir)
hseasonzero<-highseason %>% 
  filter(group_dir==0)
hseasonzero$group_dir<-360
high_season<-highseason %>% 
  bind_rows(., hseasonzero) %>% 
  ungroup()
saveRDS(high_season,"seasonal_highwind_freq_2010_2018.rds")


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
b_zero<-beaufort_freq %>% 
  filter(group_dir==0)
b_zero$group_dir<-360
b_freq<-beaufort_freq %>% 
  full_join(.,b_zero)

saveRDS(b_freq, "wind_beaufort_scale_2010_2018.rds")


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


