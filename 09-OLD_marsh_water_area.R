#####packages#####
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(purrr)
library(broom)

#####load data#####
###acreage is all delineated by Erin/Amanda RE the CERF 2017 analysis
###it doesn't go to 2010-2011 because of projection/time issues
###in the 2017 report I am using the pervious delineations (delineation)+ the 
###2017 delineation for CERF for continuity.  there is about a 10% difference in 
###marsh area between the two so I don't feel like I can use the Lora Pre-restoration
###images and the Erin post restoration delineations
library(readxl)
acreage <- read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/aerial_imagery_land_cover_delineation/ArcGIS_acreage.xlsx")

colnames(acreage)<-c("year", "marsh", "water", "total")
a<-acreage %>% 
  gather(key= habitat, value= acres, -year, -total) %>% 
  ggplot(aes(x=year, y=acres, fill=habitat))+
  geom_col(color="black")+
  scale_fill_manual(values = c("marsh"="chartreuse3", "water"="cyan3"))+
  geom_text(aes(label= sprintf("%0.2f", round(acres, digits = 2))), position=position_stack(vjust=0.5))+
  guides(fill=guide_legend(title="Habitat"))+
  xlab("Year")+ylab("Total Acres")+
  ggtitle("Area of Marsh and Water Since 2012")+
  theme_dominion()+
  scale_x_continuous(breaks=seq(2012,2018, by=1))
a

plot_save("mw-bar")

###Ratio Plot###
c<- ggplot(data=acreage, aes(x=water, y=marsh, color=as.factor(year)))+
  geom_point()+
  theme_dominion()+
  ggtitle("Area of Marsh and Water since 2012")
c

######Plot rain v MW ratio####
#load the weather data#
wyb<-readRDS("rds/water_year_2010_2018b.rds")
wy<-readRDS("rds/water_year_2010_2018.rds")
monrain<-readRDS("rds/monthly_rainfall_2010_2018.rds")
met<-readRDS("rds/weather_2010_2018.rds")


flight_month_rain<-monrain %>% 
  mutate(flight=case_when(
    year=="2012" & month== 8      ~ 1,
    year=="2013" & month== 8      ~ 1,
    year=="2014" & month== 8      ~ 1,
    year=="2015" & month== 7      ~ 1,
    year=="2016" & month== 8      ~ 1,
    year=="2017" & month== 8      ~ 1,
    year=="2018" & month== 8      ~ 1,
    TRUE                          ~ 0
  )) %>% 
  filter(flight==1)
flight_month_rain$year<-as.numeric(as.character(flight_month_rain$year))
rm(monrain)
wy_rain<-wy %>% 
  mutate(flight=case_when(
    date=="2012-08-31"     ~1,
    date=="2013-09-04"     ~1,
    date=="2014-08-14"     ~1,
    date=="2015-08-03"     ~1,
    date=="2016-08-19"     ~1,
    date=="2017-08-22"     ~1,
    date=="2018-08-23"     ~1,
    TRUE                   ~0
  )) %>% 
  filter(flight==1)
wyb_rain<-wyb %>% 
  mutate(flight=case_when(
    date=="2012-08-31"     ~1,
    date=="2013-09-04"     ~1,
    date=="2014-08-14"     ~1,
    date=="2015-08-03"     ~1,
    date=="2016-08-19"     ~1,
    date=="2017-08-22"     ~1,
    date=="2018-08-23"     ~1,
    TRUE                   ~0
  )) %>% 
  filter(flight==1)
wy2018<-filter(wyb_rain,date=="2018-08-23")

wy_rain<-wy_rain %>% 
  bind_rows(wy2018) %>% 
  na.omit()
rm(wy,wyb,wyb_rain,wy2018)

two_week_sum <- function(x,n=14){filter(x,rep(1,n), sides=1)}

cum_rain<-met %>% 
  select(datetime,rainfall) %>% 
  mutate(year=year(datetime),
         month=month(datetime),
         day=day(datetime),
         date=date(datetime)) %>%
  select(-datetime) %>% 
  group_by(date, month, year,day) %>% 
  summarise(daily_rain=sum(rainfall))
cum_rain$water_year<-if_else(cum_rain$month<10, 
                              cum_rain$year, cum_rain$year+1)
cum_rain2<-cum_rain %>% 
  group_by(water_year) %>% 
  arrange(date) %>% 
  mutate(two_week_cum_rain=rollsumr(daily_rain, k=14, fill=NA),
         three_week_cum_rain=rollsumr(daily_rain, k=21, fill=NA),
         one_week_cum_rain=rollsumr(daily_rain, k=7, fill=NA),
         wy_cum_rainfall=cumsum(daily_rain),
         flight=case_when(
           date=="2012-08-31"     ~1,
           date=="2013-09-04"     ~1,
           date=="2014-08-14"     ~1,
           date=="2015-08-03"     ~1,
           date=="2016-08-19"     ~1,
           date=="2017-08-22"     ~1,
           date=="2018-08-23"     ~1,
           date=="2018-08-24"     ~1,
           date=="2018-08-30"     ~1,
           TRUE                   ~0)) %>% 
  filter(flight==1)
  
mwratio<-flight_month_rain %>% 
  select(year,cum_month) %>% 
  full_join(., cum_rain2) %>%
  full_join(.,acreage) %>% 
  mutate(mw_ratio=marsh/water, 
         wm_ratio=water/marsh)
mwratio[is.na(mwratio)]<-51.808

mwratio_2<-mwratio %>%
  select(year,date,one_week_cum_rain,
         two_week_cum_rain,three_week_cum_rain,wm_ratio) %>% 
  gather(key=time, value=rain, -year,-date,-wm_ratio)

rain_model<-function(df){
  lm(rain~wm_ratio, data=df)
}

mw_nest<-mwratio_2 %>% 
  filter(!date=="2018-08-24") %>% 
  filter(!date=="2018-08-30") %>%
  group_by(time) %>% 
  nest() %>% 
  mutate(model=map(data, rain_model))
mw_details<-mw_nest %>% 
  mutate(
    glance_lm=model %>% map(glance),  #model summary: rsquared...
    rsq=glance_lm %>% map_dbl("r.squared"),  #extract rsquared
    
    tidy_lm=model %>% map(tidy), #model estimate: coeff...
    
    augment_lm=model %>% map(augment), #observation stats: resid,hat...
    res=augment_lm %>% map(".resid") #extract resid
  )
mw_nest2<-mwratio_2 %>% 
  filter(!date=="2018-08-24") %>% 
  filter(!date=="2018-08-23") %>%
  group_by(time) %>% 
  nest() %>% 
  mutate(model=map(data, rain_model))
mw_details2<-mw_nest2 %>% 
  mutate(
    glance_lm=model %>% map(glance),  #model summary: rsquared...
    rsq=glance_lm %>% map_dbl("r.squared"),  #extract rsquared
    
    tidy_lm=model %>% map(tidy), #model estimate: coeff...
    
    augment_lm=model %>% map(augment), #observation stats: resid,hat...
    res=augment_lm %>% map(".resid") #extract resid
  )
mw_nest3<-mwratio_2 %>% 
  filter(!date=="2018-08-23") %>% 
  filter(!date=="2018-08-30") %>%
  group_by(time) %>% 
  nest() %>% 
  mutate(model=map(data, rain_model))
mw_details3<-mw_nest3 %>% 
  mutate(
    glance_lm=model %>% map(glance),  #model summary: rsquared...
    rsq=glance_lm %>% map_dbl("r.squared"),  #extract rsquared
    
    tidy_lm=model %>% map(tidy), #model estimate: coeff...
    
    augment_lm=model %>% map(augment), #observation stats: resid,hat...
    res=augment_lm %>% map(".resid") #extract resid
  )
mw_nest4<-mwratio_2 %>% 
  filter(!date=="2018-08-24") %>% 
  filter(!date=="2018-08-24") %>%
  filter(!date=="2018-08-30") %>%
  group_by(time) %>% 
  nest() %>% 
  mutate(model=map(data, rain_model))
mw_details4<-mw_nest4 %>% 
  mutate(
    glance_lm=model %>% map(glance),  #model summary: rsquared...
    rsq=glance_lm %>% map_dbl("r.squared"),  #extract rsquared
    
    tidy_lm=model %>% map(tidy), #model estimate: coeff...
    
    augment_lm=model %>% map(augment), #observation stats: resid,hat...
    res=augment_lm %>% map(".resid") #extract resid
  )



mw_details$glance_lm[[3]]
  

e<-mwratio_2 %>% 
  filter(!date=="2018-08-24") %>% 
  filter(!date=="2018-08-30") %>% 
  ggplot(aes(x=rain, y=wm_ratio, color=time, shape=time))+
  geom_point(size=2)+
  geom_smooth(method=lm)
e
  
b<-mwratio %>% 
  ggplot(aes(y))+
  geom_point(aes(x=two_week_cum_rain, y=wm_ratio),color="blue",shape=17)+
  geom_point(aes(x=three_week_cum_rain, y=wm_ratio),color="green",shape=16)+
  geom_point(aes(x=one_week_cum_rain, y=wm_ratio),color="red",shape=15)
b


ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste(names(fit$model)[2], "~", y = names(fit$model)[1], "     ",
                       "Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

mw_reg<-lm(ratio ~ two_week_cum_rainfall, data=ai_flight3)
mw_reg2<-lm(ratio ~ two_week_cum_rainfall, data=ai_flight2)
ggplotRegression(mw_reg2)
