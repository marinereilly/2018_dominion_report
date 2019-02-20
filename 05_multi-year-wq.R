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

wq$year<-as.factor(year(wq$datetime))

saveRDS(wq, "wq_2010_2018.rds")

##NORTH STATION December 17/18, 2017 depth measurements arbad
s<-wq %>% 
  filter(depth_ft>=40)

####test plots for qa/qc###
a<-ggplot(wq)+geom_point(aes(x=depth_ft, y=depth_m, color=year))         
a
