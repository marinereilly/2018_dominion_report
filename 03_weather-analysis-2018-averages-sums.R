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
##NOTE THAT THERE WAS A FAILURE OF THE TEMPERATURE MODULE THAT WASN"T REPLACED UNTIL 
##APRIL 9th SO THESE DATA ARE LIKELY TO BE SKEWED AND MAYBE SHOULDN"T EVEN BE REPORTED?
##WHAT GOOD IS AN AVERAGE/MIN/MAX AIR TEMPERATURE FOR A WHOLE YEAR?

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

saveRDS(daily_average_temp_rh, "daily_average_temp_rh.rds")

#####THERE WAS A SENSOR FAILURE FOR THE TEMPERATURE MODULE THAT
###WASN"T FIXED UNTIL APRIL 9TH 


###Hourly rainfall###
hourly_rainfall<-weather_2018 %>% 
  select(date, hour, rainfall) %>% 
  group_by(date, hour) %>% 
  summarise_if(is.numeric, sum) %>% 
  mutate(., intensity=case_when(
    rainfall== 0                ~ "none",
    rainfall>0 & rainfall<0.1   ~ "light",
    rainfall>=0.1 & rainfall<0.3 ~ "moderate",
    rainfall>0.3 & rainfall<2.0 ~ "heavy",
    rainfall>2.0                ~ "violent"
    )
    )
saveRDS(hourly_rainfall, "hourly_rainfall.rds")

#Number of hours a day that it rained on the days that it rained grouped by intensity
raindays<-count(hourly_rainfall, intensity) %>% 
  filter(., n !=24)
saveRDS(raindays, "days-of-rain-by-hrs-intensity.rds")
View(raindays)


###For testing and QA purposes Pretty much ignore this
View(daily_average_temp_rh)
a<-hourly_rainfall %>% 
  ggplot()+
  geom_point(aes(x=date, y=rainfall, color=intensity))
a

raindays<-count(hourly_rainfall, intensity) %>% 
  filter(., n !=24)
saveRDS(raindays, "days-of-rain-by-hrs-intensity.rds")
View(raindays)

b<-raindays %>% 
  ggplot() +
  geom_bar(aes(x=date, y=n, fill=intensity), position="stack", stat="identity", width = 2)
b

cblpier <- read.csv("C:/Users/Erin/Downloads/DataSetExport-1550239580564/DataSetExport-1550239580564.csv", header=FALSE, stringsAsFactors=FALSE)
cblpier<-cblpier[-c(1:2),-c(3:5)]
colnames(cblpier)<-c("datetime", "temperature")
cblpier$datetime<-ymd_hms(cblpier$datetime)
cblpier$date<-date(cblpier$datetime)
cblpier$temperature<-as.numeric(cblpier$temperature)
cbl_da<-cblpier %>% 
  group_by(date) %>% 
  summarise_if(is.numeric, mean)
