#####Run First####
library(dplyr)
library(tidyr)
library(ggplot2)

theme_dominion<-function(){
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        axis.line = element_line(size = 0.7), 
        axis.ticks = element_line(size = 0.75), 
        panel.grid.major = element_line(colour = "gray86", 
                                        linetype = "dashed"), 
        panel.grid.minor = element_line(colour = "gray86",
                                        linetype = "dashed"), 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12, colour = "black"), 
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        plot.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13), 
        panel.background = element_rect(fill = NA), 
        plot.background = element_rect(colour = NA), 
        legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA)) 
}

plot_save<-function(filename){
    ggsave(filename = as.character(paste0("plots/",filename,".eps")), device="eps", width=8, height=6 )
}


cp_pal<-c("north"="#FF5765", "mid" = "#FFDB15", "south"="#8A6FDF", "weir"="#A8E10C", "hobo"="#A8E10C", "kayak survey"="steelblue2")

year_color_highlight<-c("2018"="red3", "2017"="grey75", "2016"="grey75", "2015"="grey75",
                        "2014"="grey75", "2013"="grey75", "2012"="grey75", "2011"="grey75", 
                        "2010"="grey75", "2019"="darkblue")
year_size_highlight<-c("2018"=1.5, "2017"=1, "2016"=1, "2015"=1, "2014"=1, "2013"=1,
                       "2012"=1, "2011"=1, "2010"=1, "2019"=1)

pal7<-c("north"="#E69F00", "mid"="#F0E442", "south"="#009E73", "hobo"="#56B4E9")
pal8<-c("rainfall"="darkorchid3", "depth_ft"="navy", "depth_m"="navy")
pal9<-c("north"="#E69F00", "mid"="#F0E442", "south"="#009E73","depth_ft"="navy", 
        "depth_m"="navy","rainfall"="darkorchid3")

ypal2018 <- c("2011"="#fde0dd", "2012"="#fcc5c0", "2013"="#fa9fb5", "2014"="#f768a1", 
              "2015"="#dd3497", "2016"="#ae017e", "2017"="#7a0177", "2018"="#49006a", 
              "2010"="grey86")

#####Now Load Data and plot!####

#Cumulative Rainfall by water year

a<-readRDS("water_year_2010_2018.rds")
a$day<-lubridate::day(a$date)
a$days2<-if_else(a$month<10, 
                 paste0(a$month, "-", a$day, "-", 2000),
                 paste0(a$month, "-", a$day, "-", 1999))
a$days2<-lubridate::mdy(a$days2)
a$water_year<-as.factor(a$water_year)

b<-ggplot(a)+
  geom_line(aes(x=days2, y=wy_cum_rainfall, color=water_year, size=water_year))+
  scale_color_manual(values = year_color_highlight)+
  scale_size_manual(values = year_size_highlight)
b

c("2010"="grey86", "w"="chartruse4", "r"="deepskyblue2", "b"="navyblue")

#####Now Load Data and plot!####

#2018 Plots

a<-readRDS("wq_hourly_means.rds")

b<-a %>% 
  filter(datetime < "2019-01-01 00:00:00") %>% 
  ggplot(.)+
  geom_line(aes(x=datetime, y=salinity, color=station), size=1)+
  theme_dominion()+
  scale_x_datetime(date_minor_breaks="months", date_labels = "%b")+
  scale_color_manual(values=cp_pal, breaks=c("north", "mid", "south", "hobo"), name="Station")+
  xlab("Date")+ylab("Salinity (ppt)")+ggtitle("2018 Hourly Salinity Averages")
b

plot_save("2018_hourly_salinity")

####

a<-readRDS("wq_hourly_means.rds")
date<-c("02-14-2018", "02-28-2018", "03-15-2018", "04-03-2018",  "04-23-2018", "05-11-2018", 
        "05-24-2018", "06-11-2018", "07-02-2018", "08-01-2018","08-13-2018", "09-06-2018", 
        "09-19-2018", "10-4-2018", "10-19-2018", "10-31-2018", "11-19-2018", "11-30-2018")
salinity<-c(0.27,0.2, 4.48, 3.28, 2.82, 2.45, 0.72, 0.63, 0.78, 0.45, 0.48, 0.58, 0.7,0.6, 
            0.59,0.52,0.3, 0.3)
kayak<-data.frame(date, salinity)
kayak$date<-lubridate::mdy(kayak$date)
  
b<-a %>% 
  filter(datetime < "2019-01-01 00:00:00") %>% 
  ggplot(.)+
  geom_point(aes(x=datetime, y=salinity, color=station), size=0.85)+
    geom_point(data=kayak, aes(x=as.POSIXct(date), y=salinity, color="kayak survey"), shape=17, 
             size=2.5)+
  geom_point(data=kayak, aes(x=as.POSIXct(date), y=salinity), color="black",shape=2, 
             size=2.5)+
  theme_dominion()+
  scale_x_datetime(date_minor_breaks="months", date_labels = "%b")+
  scale_color_manual(values=cp_pal, 
                     breaks=c("north", "mid", "south", "hobo", "kayak survey"), 
                     name="Station",
                     labels=c("North","Mid","South","Weir","Kayak Survey"))+
  xlab("Date")+ylab("Salinity (ppt)")+ggtitle("2018 Hourly Salinity Averages")+
  guides(colour = guide_legend(override.aes = list(size=2.5,
                                                   shape=c(16,16,16,16,17))),
         fill=FALSE)
b

plot_save("2018-hourly-salinity-point-kayak")

####
a<-readRDS("wq_daily_means.rds")
rain<-readRDS("daily-rain-hail-2018.rds")
c<-full_join(a,rain)
c<-c %>% 
  select(date, station, depth_ft, salinity, rainfall) %>% 
  gather(key=variable, value=measurement, -date, -station)
c$variable<-factor(c$variable, levels = c("salinity", "depth_ft", "rainfall"))

v_names<-c(
  "salinity"="Salinity (ppt)",
  "depth_ft"="Depth (ft)",
  "rainfall"="Daily rainfall (in)"
)

d<-ggplot(c, aes(x=date, y=measurement, color=station)) + 
  geom_bar(data=subset(c,variable == "rainfall"), color="deepskyblue4", stat="identity")+
  geom_line(data=subset(c,variable == "depth_ft"), size=1)+
  geom_point(data=subset(c,variable == "salinity"), size=1)+
  scale_color_manual(values=cp_pal,
                     breaks=c("north", "mid", "south", "hobo"), 
                     labels=c("North", "Mid","South","Weir"))+
  theme_dominion()+
  facet_grid(variable~., scales="free_y", switch = "y", labeller = as_labeller(v_names))+
  ggtitle("2018 Salinity, Depth, and Rainfall at Cove Point Marsh")+
  theme(strip.background =element_rect(fill="aliceblue"))+
  theme(strip.text = element_text(size = 11))+
  theme(legend.position="top")+
  theme(legend.title=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_date(date_minor_breaks="months", date_labels = "%b")+
  theme(panel.grid.minor.y = element_blank())+
  guides(colour = guide_legend(override.aes=list(shape = 15, size = 5, linetype = 0)))+
  xlab("Date")+ylab(NULL)
d

plot_save("salinity-depth-rain-2018")

####
a<-readRDS("wq_hourly_means.rds")

b<-ggplot(a)+
  geom_point(aes(x=datetime, y=depth_ft, color=station))+
  scale_color_manual(values=cp_pal,
                     breaks=c("north", "mid", "south", "hobo"), 
                     labels=c("North", "Mid","South","Weir"),
                     name="Station")+
  theme_dominion()+
  scale_x_datetime(date_minor_breaks="months", date_labels = "%b")+
  ggtitle("2018 Hourly Depth changes at Cove Point Marsh")+xlab("Date")+ylab("Depth (ft)")+
  guides(colour = guide_legend(override.aes=list(size = 3)))
b

plot_save("hourly-depth-2018")

####

a<-readRDS("monthly_rain_2018.rds")
a$month2<-month.abb[a$month]
a$month2<-factor(a$month, levels = month.abb)

b<-ggplot(a)+
  geom_bar(aes(x=month2, y=rain), fill="steelblue4", color="black", stat = "identity")+
  scale_x_discrete(limits= month.abb)+
  theme_dominion()+
  ggtitle("Monthly Rainfall at Cove Point Marsh in 2018")+xlab("Month")+ylab("Rainfall (in)")
b

plot_save("monthly_rainfall_2018")

####

a<-readRDS("daily_average_temp_rh.rds")

b<-ggplot(a)+
  geom_point(data=subset(a, a$date>lubridate::ymd("2018-04-09")),
             aes(x=rel_hum, y=air_temp, color=date))+
  geom_path(data=subset(a, a$date>lubridate::ymd("2018-04-09")),
            aes(x=rel_hum, y=air_temp, color=date))
b
#not a good plot

####
a<-readRDS("do_temp_hourly.rds")
a$datetime<-lubridate::ymd_h(paste0(a$date, " ",a$hour))
c<-a %>% 
  ungroup()
c$date<-NULL
c$hour<-NULL
c<-c %>% 
  select(-temp_wl) %>% 
  gather(key=variable, value=measurement, -datetime, -station) %>% 
  filter(!station=="hobo")

b<-ggplot(c, aes(x=datetime, y=measurement))+
  geom_hline(aes(yintercept=4), color="black", linetype="dashed")+
  geom_point(data=subset(c, c$variable=="DO_mg"),aes(color=station), size=.8)+
  geom_smooth(data=subset(c, c$variable=="temperature"), color="darkgreen")+
  scale_color_manual(values=cp_pal,
                     breaks=c("north", "mid", "south"), 
                     labels=c("North", "Mid","South"),
                     name="Station")+
  theme_dominion()+
  scale_x_datetime(date_minor_breaks="months", date_labels = "%b")+
  guides(colour = guide_legend(override.aes=list(size = 3)))+
  ggtitle("2018 Dissolved Oxygen and Temperature")+xlab("Date")+
  ylab("Dissolved Oxygen (mg/L); Temperature (\u00B0C)")
b

plot_save("DO_Temp")

####

a<-readRDS("hourly_hypoxia.rds")
b<-ggplot(a)+
  geom_point(aes(x=date, y=hour, color=light))+
  geom_hline(aes(yintercept=12), color="black")+
  scale_x_date(date_breaks="months", date_labels= "%b")+
  theme_dominion()+
  scale_color_manual(values=c("day"="lightgoldenrod1", "night"="cadetblue3"))+
  theme(axis.line.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y= element_blank(),
        legend.title=element_blank(),
        strip.background =element_rect(fill="white"),
        strip.text.y = element_text(angle = 0, size=11))+
  facet_grid(station~.)+
  guides(colour = guide_legend(override.aes=list(shape = 15, size = 5, linetype = 0)))+
  ggtitle("Hypoxia at Cove Point Marsh")+ylab("Time of Day")+xlab("Date")
b

plot_save("hypoxia_2018")

####
a<-readRDS("hourly_rainfall.rds")
b<-readRDS("wq_hourly_means.rds")

ungroup(a)

c<-b %>% 
  select(station, date, hour, depth_ft) %>% 
  full_join(., a)
c$datetime<-lubridate::ymd_h(paste0(c$date, " ",c$hour))

c<-c %>% 
  arrange(datetime) %>% 
  mutate()


d<-c %>% 
  filter(rainfall>0) %>% 
  ggplot(.)+
  geom_point(aes(y=depth_ft, x=rainfall, color=station))+
  #scale_x_log10()+
  facet_grid(station~.)
d

# plot is bad - maybe need to think about changes in depth and rainfall instead? like
# delta depth vs rainfall?


#####Multi-year plots are hopefully fixed!####
#Water Year
a<-readRDS("water_year_2010_2018.rds")
b<-a %>% 
  filter(!water_year=="2010") %>%
  ggplot(.) + 
  geom_step(aes(x=days2, y=wy_cum_rainfall, color=water_year, size=water_year))+
  scale_color_manual(values= year_color_highlight)+
  scale_size_manual(values=year_size_highlight)+
  scale_x_date(breaks = "months", date_labels = "%b")+
  theme_dominion()+
  ggtitle("Cumulative Rainfall by Water Year (Oct-Oct)")+
  xlab("Day of year")+ylab("Cumulative Rainfall (in)")+
  guides(color=guide_legend(title="Water Year"),
         size=guide_legend(title="Water Year"))
b

plot_save("water_year_2010_2018")
#Monthly Rainfall
a<-readRDS("monthly_rainfall_2010_2018.rds")
a$year<-as.factor(a$year)
a$month<-lubridate::month(a$month)


b<-ggplot(a, aes(x=month, y=rainfall, color=year, size=year))+
  geom_line()+
  geom_point(size=2)+
  scale_x_continuous(breaks = 1:12,
    labels = month.abb)+
  ggtitle("Monthly Rainfall at Cove Point Marsh")+
  ylab("Rainfall (in)")+xlab("Month")+
  scale_color_manual(values = ypal2018)+
  scale_size_manual(values=year_size_highlight)+
  theme_dominion()+
  guides(color=guide_legend(title="Year"),
         size=guide_legend(title="Year"))
b

plot_save("monthly_rainfall_2010_2018")

c<-a %>% 
  filter(!year=="2010") %>% 
  ggplot(., aes(x=month, y=cum_month, color=year, size=year))+
  geom_step()+
  #geom_point(size=2)+
  geom_step(data=subset(a, year=="2018"),
            aes(x=month, y=cum_month, color=year, size=year))+
  scale_x_continuous(breaks = 1:12,
                     labels = month.abb)+
  ggtitle("Cumulative Monthly Rainfall at Cove Point Marsh")+
  ylab("Rainfall (in)")+xlab("Month")+
  scale_color_manual(values = ypal2018)+
  scale_size_manual(values=year_size_highlight)+
  theme_dominion()+
  guides(color=guide_legend(title="Year"),
         size=guide_legend(title="Year"))
c

plot_save("cum_monthly_rainfall_2011_2018")

###Daily Air Temperatures
a<-readRDS("daily_air_temp_2010_2018.rds")
a$year<-as.factor(a$year)
b<-ggplot(a, aes(x=doy, y=air_temp, color=year))+
  geom_point(size=0.7)+
  geom_smooth(se=FALSE)+
  geom_smooth(data=subset(a,year=="2018"), se=FALSE, size=2)+
  scale_color_manual(values=ypal2018)+
  theme_dominion()+
  guides(color=guide_legend(title="Year"),
         size=guide_legend(title="Year"))+
  ggitle("Daily Air Temperatures")+
b

c<-ggplot(a,aes(x = as.Date(lubridate::yday(date), "1970-01-01"), y=air_temp, color=year))+
            geom_point(size=0.7)+
            geom_smooth(se=FALSE)+
            geom_smooth(data=subset(a,year=="2018"), se=FALSE, size=2)+
            scale_color_manual(values=ypal2018)+
            theme_dominion()+
            scale_x_date(date_labels = "%b")+
            guides(color=guide_legend(title="Year"),
                   size=guide_legend(title="Year"))+
            ggtitle("Daily Air Temperatures")+xlab("Date")+ylab("Temperature (\u00B0F)")
c

plot_save("air_temperatures_2010_2018")

###WIND!
a<-readRDS("wind_beaufort_scale_2010_2018.rds")
library(viridis)
a<-ungroup(a)

a$beaufort <- factor(a$beaufort, levels = c("8", "7", "6", "5", "4", "3", "2", "1", "0"))

b<-a %>% 
  filter(year=="2018") %>% 
  filter(!group_dir==360) %>% 
  ggplot()+
  geom_bar(aes(x=group_dir, y=n, fill=beaufort), color="black", stat = "identity")+
  theme_minimal()+
  scale_fill_viridis(discrete=TRUE, option="magma", direction = -1)+
  ggtitle("2018 Wind Speed on the Beaufort Scale")+
  guides(fill=guide_legend(title="Beaufort Number"))+
  theme(axis.line=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank())+
  coord_polar(theta="x", start = -0.383972)+
  scale_x_continuous(breaks = c(0, 45, 90, 135,180,225,270,315),  
                     labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"), minor_breaks = NULL)
b 

plot_save("beaufort_2018") 

##
a<-readRDS("windfrequency2010-2018.rds")
a$year<-as.factor(a$year)
b<-ggplot(a)+
  geom_line(aes(x=group_dir, y=n, color=year), size=1)+
  geom_line(data=subset(a, year=="2018"), 
            aes(x=group_dir, y=n, color=year),linetype="solid", size=2)+
  theme_minimal()+
  scale_color_manual(values=ypal2018)+
  theme(axis.line=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank())+
  ggtitle("Winds at Cove Point Marsh")+
  scale_x_continuous(breaks = c(0, 45, 90, 135,180,225,270,315), limits = c(0,360), 
                     labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"), minor_breaks = NULL)+
  coord_polar(theta="x", start = )+
  guides(color=guide_legend(title="Year"))
b
  
plot_save("all_wind")

a<-readRDS("high_winds_2010_2018.rds")
a$year<-as.factor(a$year)
b<-ggplot(a)+
  geom_line(aes(x=group_dir, y=n, color=year), size=1)+
  geom_line(data=subset(a, year=="2018"), 
            aes(x=group_dir, y=n, color=year),linetype="solid", size=2)+
  theme_minimal()+
  scale_color_manual(values=ypal2018)+
  theme(axis.line=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank())+
  ggtitle("High wind events at Cove Point Marsh")+
  scale_x_continuous(breaks = c(0, 45, 90, 135,180,225,270,315), limits = c(0,360), 
                     labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"), minor_breaks = NULL)+
  coord_polar(theta="x", start = )+
  guides(color=guide_legend(title="Year"))
b

plot_save("high_winds")
###
a<-readRDS("seasonal_wind_freq_2010_2018.rds")
a$year<-as.factor(a$year)
a$season<-factor(a$season, levels=c("Winter", "Spring", "Summer", "Autumn"))

b<-ggplot(a)+
  geom_line(aes(x=group_dir, y=n, color=year), size=1)+
  geom_line(data=subset(a, year=="2018"), 
            aes(x=group_dir, y=n, color=year),linetype="solid", size=2)+
  theme_minimal()+
  scale_color_manual(values=ypal2018)+
  theme(axis.line=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(),
        legend.position="bottom",
        axis.text = element_text(size = 12, colour = "grey60"), 
        axis.text.x = element_text(size = 12), 
        plot.title = element_text(size = 16), 
        legend.text = element_text(size = 11), 
        legend.title = element_text(size = 12),
        strip.text.x = element_text(size = 13))+
  ggtitle("Winds at Cove Point Marsh by Season")+
  scale_x_continuous(breaks = c(0, 45, 90, 135,180,225,270,315), limits = c(0,360), 
                     labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"), minor_breaks = NULL)+
  coord_polar(theta="x", start = )+
  guides(color=guide_legend(title=NULL, nrow=1))+
  facet_wrap(season~., nrow=1)
b

plot_save("winds_by_season")

a<-readRDS("seasonal_highwind_freq_2010_2018.rds")
a$year<-as.factor(a$year)
a$season<-factor(a$season, levels=c("Winter", "Spring", "Summer", "Autumn"))

b<-ggplot(a)+
  geom_line(aes(x=group_dir, y=n, color=year), size=1)+
  geom_line(data=subset(a, year=="2018"), 
            aes(x=group_dir, y=n, color=year),linetype="solid", size=2)+
  theme_minimal()+
  scale_color_manual(values=ypal2018)+
  theme(axis.line=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(),
        legend.position="bottom",
        axis.text = element_text(size = 12, colour = "grey60"), 
        axis.text.x = element_text(size = 12), 
        plot.title = element_text(size = 16), 
        legend.text = element_text(size = 11), 
        legend.title = element_text(size = 12),
        strip.text.x = element_text(size = 13))+
  ggtitle("High Winds at Cove Point Marsh by Season")+
  scale_x_continuous(breaks = c(0, 45, 90, 135,180,225,270,315), limits = c(0,360), 
                     labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"), minor_breaks = NULL)+
  coord_polar(theta="x", start = )+
  guides(color=guide_legend(title=NULL, nrow=1))+
  facet_wrap(season~., nrow=1)
b

plot_save("highwinds_by_season")

#####Salinity Timeline
a<-readRDS("mean_daily_salinity2010_2018.rds")
a$year<-lubridate::year(a$date)
a$year<-as.factor(a$year)

b<-a %>% 
  filter(!year=="2019") %>% 
  ggplot(., aes(x=date, y=salinity, color=year))+
  geom_vline(aes(xintercept= as.Date("2011-03-01")), 
             linetype="longdash", 
             color="darkslategray3")+
  geom_line(size=1)+
  annotate("text",x=as.Date("2010-03-01"), y=14.5, label="Pre-Restoration", 
            color="darkslategray3", size=4.5)+
  annotate("text",x=as.Date("2012-04-15"), y=14.5, label="Post-Restoration", 
            color="darkslategray3", size=4.5)+
  scale_color_manual(values=ypal2018)+
  scale_x_date(limits = c(as.Date("2009-10-01"),as.Date("2019-03-01")))+
  theme_dominion()+
  guides(colour = guide_legend(override.aes=list(shape = 15, size = 5, linetype = 0), 
                               title = "Year"))+
  scale_y_continuous(limits= c(0, 15))+
  ggtitle("Salinity Pre and Post Restoration")+xlab("Date")+ylab("Mean Daily Salinity (ppt)")
b    
plot_save("salinity_timeline2")

#####Depth Timeline#####
wq<-readRDS(file = "rds/wq_daily_means.rds")

a<-depth %>% 
  filter(!station=="hobo") %>%
  filter(!depth>=4) %>% 
  filter(!depth<=-3) %>% 
  ggplot()+
  geom_line(aes(x=date, y=depth, color=station), size=1)+
  theme_dominion()+
  scale_color_manual(values=cp_pal)+
  ylab("Depth (ft)")+
  ggtitle("Dominion Marsh Depth 2010-2018")
  
a
ggsave("depth2010-2018.pdf")
