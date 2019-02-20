#####load libraries####
library("dplyr")
library("ggplot2")
library("lubridate")

#####load and join data#####
wq_2018<-readRDS("wq_2018_full.rds")
wq_2010_2017 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/ysi_noaa_wq_weather_tides/data/various_R_code/dominion_water_quality/Raw YSI Data/wq_2010_2017.csv", stringsAsFactors=FALSE)
wq_2010_2017$DateTime<-ymd_hms(wq_2010_2017$DateTime)


colnames(wq_2018)
colnames(wq_2010_2017)

wq<-wq_2010_2017 %>% 
  select(datetime=DateTime,station=Station, depth_ft=Depth, temperature=Temp,
         "DO_%" =ODO, DO_mg=ODO_Conc, spcond=SpCond, salinity=Salinity) %>% 
  mutate(depth_m=depth_ft*0.3048) %>% 
  full_join(wq_2018, .) 

wq$station[wq$station=="North"]<-"north"
wq$station[wq$station=="South"]<-"south"
wq$station[wq$station=="Mid"]<-"mid"

wq$year<-as.factor(year(wq$datetime))
wq$date<-date(wq$datetime)
wq$hour<-hour(wq$datetime)
  
saveRDS(wq, "wq_2010_2018.rds")

##NORTH STATION December 17/18, 2017 depth measurements arbad
s<-wq %>% 
  filter(depth_ft>=40)
#####Salnity daily averages#####
salinity<-wq %>% 
  select(date, salinity) %>% 
  group_by(date) %>% 
  summarize(salinity=mean(salinity))
saveRDS(salinity, "mean_daily_salinity2010_2018.rds")

salinity2<-wq %>% 
  select(date, station, salinity) %>% 
  group_by(date, station) %>% 
  summarize(salinity=mean(salinity))
saveRDS(salinity2, "mean_daily_station_salinity2010_2018.rds")

###Hourly DO averages###
hypoxia<-wq %>% 
  select(date, hour, station, DO_mg) %>% 
  group_by(date, hour, station) %>% 
  summarise(DO_mg=mean(DO_mg)) %>% 
  filter(DO_mg<=4) %>% 
  mutate(datetime=ymd_h(paste0(date," ",hour)))

saveRDS(hypoxia, "hourly_hypoxia_2010_2018.rds")

####test plots for qa/qc###
a<-ggplot(hypoxia)+geom_point(aes(x=date, y=station, color=station)) +facet_wrap()        
a
