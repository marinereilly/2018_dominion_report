####library####
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(hablar)
#####load HOBO Water Level data#####
#May be missing Nov 19 - Nov 30 Look later
wl1 <- read.delim("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/WL_12_19_2017_02_14_2018.csv", header=FALSE, comment.char="#", stringsAsFactors=FALSE)
wl1<-wl1[-1,-1]
wl_head<-c("datetime", "kPA", "temp", "barokPA", "depth") #creates a list of column names in the order of the columns
colnames(wl1)<-wl_head #assigns the columns the names we created above
wl1$kPA<-as.character(wl1$kPA) #formats for joining purposes
wl1$temp<-as.character(wl1$temp)
wl1$barokPA<-as.character(wl1$barokPA)
wl1$depth<-as.character(wl1$depth)
wl1$datetime<-mdy_hm(wl1$datetime)


wl2 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/CPWeir_02_28_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl2<-wl2[-c(1:2),-1] #removes the first two rows and the first column
colnames(wl2)<-wl_head
wl2$datetime<-mdy_hms(wl2$datetime)
wl2<-wl2[!is.na(wl2$depth),] #removes rows where there is no depth value


wl3 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/CPWeir_03_15_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl3<-wl3[-c(1:2),-1]
colnames(wl3)<-wl_head
wl3$datetime<-mdy_hms(wl3$datetime)
wl3<-wl3[!is.na(wl3$depth),]


wl4 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/10051488_CPWeir_04_23_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl4<-wl4[-c(1:2),-1]
colnames(wl4)<-wl_head
wl4<-wl4[!is.na(wl4$depth),]
wl4$datetime<-mdy_hms(wl4$datetime)


wl5 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/CPWeir_05_11_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl5<-wl5[-c(1:2),-1]
colnames(wl5)<-wl_head
wl5$datetime<-mdy_hms(wl5$datetime)
wl5<-wl5[!is.na(wl5$depth),]


wl6 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/CPWeir_05_24_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl6<-wl6[-c(1:2),-1]
colnames(wl6)<-wl_head
wl6$datetime<-mdy_hms(wl6$datetime)
wl6<-wl6[!is.na(wl6$depth),]


wl7 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/CPWeir_07_02_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl7<-wl7[-c(1:2),-1]
colnames(wl7)<-wl_head
wl7$datetime<-mdy_hms(wl7$datetime)
wl7<-wl7[!is.na(wl7$depth),]


wl8 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/CPWeir_09_06_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl8<-wl8[-c(1:2),-c(1,7:10)]
colnames(wl8)<-wl_head
wl8$datetime<-mdy_hms(wl8$datetime)


wl9 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/CPWeir_09_19_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl9<-wl9[-c(1:2),-c(1,7:10)]
colnames(wl9)<-wl_head
wl9$datetime<-mdy_hms(wl9$datetime)
wl9<-wl9[!is.na(wl9$depth),]


wl10 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/CPWeir_10_04_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl10<-wl10[-c(1:2),-1]
colnames(wl10)<-wl_head
wl10$datetime<-mdy_hms(wl10$datetime)


wl11 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/2018_10_19_Weir.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl11<-wl11[-c(1:2),-1]
colnames(wl11)<-wl_head
wl11$datetime<-mdy_hms(wl11$datetime)


wl12 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/CPWeir_11_19_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl12<-wl12[-c(1:2),-1]
colnames(wl12)<-wl_head
wl12$datetime<-mdy_hms(wl12$datetime)


wl13 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/CPWeir_01_18_2019.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl13<-wl13[-c(1:2),-1]
wl13$datetime<-paste0(wl13$V2, " ", wl13$V3) #combines the date column and the time column so that it is the same as the other data.  Will probably be separated again during analysis
colnames(wl13)<-c("date", "time", "kPA", "temp", "barokPA", "depth", "datetime")
wl13$datetime<-mdy_hms(wl13$datetime)


wl14 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/CPWeir_04_03_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl14<-wl14[-c(1:2),-1]
wl14$datetime<-paste0(wl14$V2, " ", wl14$V3) #combines the date column and the time column so that it is the same as the other data.  Will probably be separated again during analysis
colnames(wl14)<-c("date", "time", "kPA", "temp", "barokPA", "depth", "datetime")
wl14$datetime<-mdy_hms(wl14$datetime)
wl14<-wl14[!is.na(wl14$depth),]


wl15 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/CPWeir_08_01_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl15<-wl15[-c(1:2),-1]
wl15$datetime<-paste0(wl15$V2, " ", wl15$V3) #combines the date column and the time column so that it is the same as the other data.  Will probably be separated again during analysis
colnames(wl15)<-c("date", "time", "kPA", "temp", "barokPA", "depth", "datetime")
wl15$datetime<-mdy_hms(wl15$datetime)
wl15<-wl15[!is.na(wl15$depth),]


wl16 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/CPWeir_10_31_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl16<-wl16[-c(1:2),-1]
wl16$datetime<-paste0(wl16$V2, " ", wl16$V3) #combines the date column and the time column so that it is the same as the other data.  Will probably be separated again during analysis
colnames(wl16)<-c("date", "time", "kPA", "temp", "barokPA", "depth", "datetime")
wl16$datetime<-mdy_hms(wl16$datetime)
wl16<-wl16[!is.na(wl16$depth),]


wl17 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/CPWeir_11_19_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl17<-wl17[-c(1:2),-1]
colnames(wl17)<-wl_head
wl17$datetime<-mdy_hms(wl17$datetime)


wl18 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/CPWeir_11_30_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl18<-wl18[-c(1:2),-1]
wl18$datetime<-paste0(wl18$V2, " ", wl18$V3) #combines the date column and the time column so that it is the same as the other data.  Will probably be separated again during analysis
colnames(wl18)<-c("date", "time", "kPA", "temp", "barokPA", "depth", "datetime")
wl18$datetime<-mdy_hms(wl18$datetime)
wl18<-wl18[!is.na(wl18$depth),]

#####Join Hobo Water Level Data####

hobo_wl<-full_join(wl1,wl2) %>% 
  full_join(., wl3) %>% 
  full_join(., wl4) %>% 
  full_join(., wl5) %>% 
  full_join(., wl6) %>% 
  full_join(., wl7) %>% 
  full_join(., wl8) %>% 
  full_join(., wl9) %>% 
  full_join(., wl10) %>% 
  full_join(., wl11) %>% 
  full_join(., wl13) %>% 
  full_join(., wl14) %>% 
  full_join(., wl15) %>% 
  full_join(., wl16) %>% 
  full_join(., wl17) %>% 
  full_join(., wl18) %>% 
  select(-date, -time) #Joins all of the wl data with all columns and removes the date and time columns since they only exist in wl13

saveRDS(hobo_wl, "hobo_wl_2018.rds") #saves this as an r object that can be read in again later (more efficient than csv (the csv file was 4x as big!) for large data files)
rm(wl1,wl2,wl3,wl4,wl5,wl6,wl7,wl8,wl9,wl10,wl11,wl12,wl13,wl14,wl15,wl16,wl17,wl18, hobo_wl,wl_head) #removes the temporary objects we made from the global environment
#####Hobo Salinity Data####
#may be missing January 1 - February 14 - look later
sal1<-read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/sal_02_28_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
sal_names<-c("datetime", "conductivity", "temperature", "sp_cond", "salinity")
sal1<-sal1[-c(1:2),-1]
colnames(sal1)<-sal_names
sal1$datetime <- mdy_hms(sal1$datetime)


sal2<-read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/sal_03_15_2018b.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
sal2<-sal2[-1,-1]
colnames(sal2)<-sal_names
sal2$datetime <- mdy_hm(sal2$datetime)


sal3<-read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/sal_04_23_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
sal3<-sal3[-c(1:2),-1]
colnames(sal3)<-sal_names
sal3$datetime <- mdy_hms(sal3$datetime)


sal4<-read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/sal_05_11_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
sal4<-sal4[-c(1:2),-1]
colnames(sal4)<-sal_names
sal4$datetime <- mdy_hms(sal4$datetime)


sal5<-read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/sal_05_24_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
sal5<-sal5[-c(1:2),-1]
colnames(sal5)<-sal_names
sal5$datetime <- mdy_hms(sal5$datetime)


sal6<-read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/sal_07_02_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
sal6<-sal6[-c(1:2),-1]
colnames(sal6)<-sal_names
sal6$datetime <- mdy_hms(sal6$datetime)


sal7<-read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/sal_09_06_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
sal7<-sal7[-c(1:2),-c(1,7:10)]
colnames(sal7)<-sal_names
sal7$datetime <- mdy_hms(sal7$datetime)


sal8<-read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/sal_09_19_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
sal8<-sal8[-c(1:2),-c(1, 7:10)]
colnames(sal8)<-sal_names
sal8$datetime <- mdy_hms(sal8$datetime)


sal9<-read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/sal_10_04_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
sal9<-sal9[-c(1:2),-1]
colnames(sal9)<-sal_names
sal9$datetime <- mdy_hms(sal9$datetime)


sal10<-read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/sal_10_19_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
sal10<-sal10[-c(1:2),-1]
colnames(sal10)<-sal_names
sal10$datetime <- mdy_hms(sal10$datetime)


sal11<-read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/sal_10_31_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
sal11<-sal11[-c(1:2),-1]
colnames(sal11)<-sal_names
sal11$datetime <- mdy_hms(sal11$datetime)


sal12<-read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/sal_11_19_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
sal12<-sal12[-c(1:2),-1]
colnames(sal12)<-sal_names
sal12$datetime <- mdy_hms(sal12$datetime)


sal13<-read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/sal_01_18_2019.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
sal13<-sal13[-c(1:2),-c(1,8)]
sal13$datetime<-paste0(sal13$V2, " ", sal13$V3)
colnames(sal13)<-c("date", "time", "conductivity", "temperature", "sp_cond", "salinity", "datetime")
sal13$datetime <- mdy_hms(sal13$datetime)


sal14<-read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/sal_04_03_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
sal14<-sal14[-c(1:2),-1]
sal14$datetime<-paste0(sal14$V2, " ", sal14$V3)
colnames(sal14)<-c("date", "time", "conductivity", "temperature", "sp_cond", "salinity", "datetime")
sal14$datetime <- mdy_hms(sal14$datetime)


sal15<-read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/sal_2018_08_01.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
sal15<-sal15[-c(1:2),-1]
sal15$datetime<-paste0(sal15$V2, " ", sal15$V3)
colnames(sal15)<-c("date", "time", "conductivity", "temperature", "sp_cond", "salinity", "datetime")
sal15$datetime <- mdy_hms(sal15$datetime)


sal16<-read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/sal_11_19_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
sal16<-sal16[-c(1:2),-1]
colnames(sal16)<-sal_names
sal16$datetime <- mdy_hms(sal16$datetime)


sal17<-read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/sal_11_30_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
sal17<-sal17[-c(1:2),-1]
sal17$datetime<-paste0(sal17$V2, " ", sal17$V3)
colnames(sal17)<-c("date", "time", "conductivity", "temperature", "sp_cond", "salinity", "datetime")
sal17$datetime <- mdy_hms(sal17$datetime)


#####Join HOBO salinity data#####
hobo_sal<-full_join(sal1,sal2) %>% 
  full_join(., sal3) %>% 
  full_join(., sal4) %>% 
  full_join(., sal5) %>% 
  full_join(., sal6) %>% 
  full_join(., sal7) %>% 
  full_join(., sal8) %>% 
  full_join(., sal9) %>% 
  full_join(., sal10) %>% 
  full_join(., sal11) %>% 
  full_join(., sal13) %>% 
  full_join(., sal14) %>% 
  full_join(., sal15) %>% 
  full_join(., sal16) %>% 
  full_join(., sal17) %>% 
  select(-date, -time) #Joins all of the wl data with all columns and removes the date and time columns since they only exist in wl13
saveRDS(hobo_sal, "hobo_sal_2018.rds") #saves this as an r object that can be read in again later (more efficient than csv (the csv file was 4x as big!) for large data files)
rm(sal1,sal2,sal3,sal4,sal5,sal6,sal7,sal8,sal9,sal10,sal11,sal12,sal13,sal14,sal15,sal16,sal17, hobo_sal, sal_names)

#####Join all HOBO data and continue to wrangle####
hobo1<-readRDS("hobo_wl_2018.rds")
hobo2<-readRDS("hobo_sal_2018.rds")

hobo<-full_join(hobo1,hobo2)
hobo<- hobo[order(hobo$datetime),]

hobo2018<-hobo %>% 
  filter(., datetime >= "2018-01-01 00:00:00") %>% 
  filter(., datetime <= "2019-01-01 00:00:00")
View(hobo2018)

rm(hobo, hobo1, hobo2)

hobo2018<-hobo2018 %>% 
  convert(num("kPA","temp","barokPA","depth","conductivity","temperature", "sp_cond","salinity"))
hobo2018$station<-"hobo"

saveRDS(hobo2018, "hobo2018.rds")

#####plot for Quick QA purposes#####
a<-ggplot(hobo2018, aes(x=datetime))
b<-a+geom_point(aes(y=temperature),color="red")+geom_point(aes(y=temp), color="blue")
b

rm(a,b)
#####load YSI data#####
mid_eagleio_2018 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/ysi_noaa_wq_weather_tides/data/2018_mid_eagleio.csv", header=FALSE, stringsAsFactors=FALSE)
mid_eagleio_2018<-mid_eagleio_2018[-c(1:3),-2]
colnames(mid_eagleio_2018)<-c("datetime", "depth_ft", "DO_%", "DO_mg", "salinity", "spcond", "temperature")
mid_eagleio_2018$station<-"mid"
mid_eagleio_2018$datetime<-ymd_hms(mid_eagleio_2018$datetime)
saveRDS(mid_eagleio_2018, "mid_eagleio_2018.rds")

north_eagleio_2018 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/ysi_noaa_wq_weather_tides/data/2018_north_eagleio.csv", header=FALSE, stringsAsFactors=FALSE)
north_eagleio_2018<-north_eagleio_2018[-c(1:3),-2]
colnames(north_eagleio_2018)<-c("datetime", "depth_ft", "DO_%", "DO_mg", "salinity", "spcond", "temperature")
north_eagleio_2018$station<-"north"
north_eagleio_2018$datetime<-ymd_hms(north_eagleio_2018$datetime)
saveRDS(north_eagleio_2018, "north_eagleio.rds")

south_eagleio_2018 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/ysi_noaa_wq_weather_tides/data/2018_south_eagleio.csv", header=FALSE, stringsAsFactors=FALSE)
south_eagleio_2018<-south_eagleio_2018[-c(1:3),-2]
colnames(south_eagleio_2018)<-c("datetime", "depth_ft", "DO_%", "DO_mg", "salinity", "spcond", "temperature")
south_eagleio_2018$station<-"south"
south_eagleio_2018$datetime<-ymd_hms(south_eagleio_2018$datetime)
saveRDS(south_eagleio_2018, "south_eagleio_2018.rds")

ysi2018<-north_eagleio_2018 %>% 
  full_join(., mid_eagleio_2018) %>% 
  full_join(., south_eagleio_2018)
ysi2018<-ysi2018 %>% 
  convert(num("depth_ft", "DO_%", "DO_mg", "salinity", "spcond", "temperature"))
saveRDS(ysi2018, "ysi2018.rds")
View(ysi2018)

rm(mid_eagleio_2018,north_eagleio_2018,south_eagleio_2018)

#####Combining ysi data and hobo data####
#issues arise because there are two temperatures in the hobodata because of the two sensors
#renameing some of the variables so that  they play nicely
hobo2018 <- hobo2018 %>% 
  rename(., depth_m =depth, spcond=sp_cond, temp_wl=temp)
hobo2018$depth_ft<-hobo2018$depth_m*3.28084
ysi2018$depth_m<-ysi2018$depth_ft*0.03048

wq2018<-ysi2018 %>% 
  full_join(., hobo2018)
View(wq2018)
saveRDS(wq2018, "wq_2018_full.rds")

#####Loading weather data#####
weather_2018 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/ysi_noaa_wq_weather_tides/data/2018_weather_eaglio.csv", header=FALSE, stringsAsFactors=FALSE)
weather_2018<-weather_2018[-c(1:3),]
w_names<-c("datetime", "air_temp", "bp", "hail", "rainfall",
           "rel_hum", "wind_dir", "wind_spd", "wind_max", "wind_min")
colnames(weather_2018)<-w_names
weather_2018$datetime<-ymd_hms(weather_2018$datetime)
weather_2018<-weather_2018 %>% 
  convert(num("air_temp", "bp", "hail", "rainfall", "rel_hum", "wind_dir", "wind_spd", "wind_max", "wind_min"))
saveRDS(weather_2018, "weather_2018.rds")
rm(w_names)
#####Formatting for plots analysis etc.#####
#load wq RDS
wq_2018_full<-readRDS("wq_2018_full.rds")
wq_2018<-wq_2018_full %>% 
  select(datetime, station, depth_ft, depth_m, temperature, temp_wl, salinity, DO_mg )
rm(wq_2018_full)
weather_2018<-readRDS("weather_2018.rds")

