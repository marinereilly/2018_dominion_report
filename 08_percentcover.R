library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)

library(readxl)
pc <- read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/percent_cover/data/2018_PercentCover.xlsx", sheet = "Plot Cover")

pc_points<-st_read("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/01_GPS_waypoints/2018/pc_07_31_08_02_2018.gpx")
pc_points<-pc_points %>% 
  select(Waypoint=name,cmt, geometry)

pc_comb<-pc_points %>% 
  left_join(.,pc) %>% 
  drop_na(Transect)
View(pc_comb)

st_write(pc_comb, "pc_combine.shp")

pc_sum<-pc %>% 
  filter(Date> as.POSIXct("2018-07-01"))

a<-pc_sum %>% 
  group_by(Transect, Habitat) %>% 
  summarize(mean=mean(`Squares%`, na.rm = TRUE))
a$year<-2018
a$Transect<-paste0("Transect ", a$Transect)
a<-a %>% 
  mutate(Habitat=case_when(
    Habitat == "B"     ~"Berm",
    Habitat == "SW"    ~"Salt Water Marsh",
    Habitat == "FW"    ~"Fresh Water Marsh"
  ))

b <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/percent_cover/data/percent_cover_plot_data.csv", stringsAsFactors=FALSE)
c <- b %>%
  select(year=Year, Transect, Habitat, Percent) %>% 
  mutate(mean=Percent*100) %>% 
  bind_rows(., a)
c$Habitat<-factor(c$Habitat, levels = c("Fresh Water Marsh", "Salt Water Marsh","Berm"))

saveRDS(c, "percent-cover-means-2011-2018.rds")

d<-c %>% 
  filter(Transect=="Transect 1") %>% 
  ggplot(.) +
  geom_point(aes(x=year, y=mean, color=Habitat), size=2)+
  geom_path(aes(x=year, y=mean, color=Habitat),size=1.25)+
  scale_color_viridis_d()+
  theme_dominion() + theme(legend.position = "none")
d
ggsave(filename = "t1_pc2.eps", device="eps", width=114, height=52, units = "mm" )

d<-c %>% 
  filter(Transect=="Transect 2") %>% 
  ggplot(.) +
  geom_point(aes(x=year, y=mean, color=Habitat), size=2)+
  geom_path(aes(x=year, y=mean, color=Habitat),size=1.25)+
  scale_color_viridis_d()+
  theme_dominion() + theme(legend.position = "none")
d
ggsave(filename = "t2_pc2.eps", device="eps", width=114, height=52, units = "mm" )

d<-c %>% 
  filter(Transect=="Transect 3") %>% 
  ggplot(.) +
  geom_point(aes(x=year, y=mean, color=Habitat), size=2)+
  geom_path(aes(x=year, y=mean, color=Habitat),size=1.25)+
  scale_color_viridis_d()+
  theme_dominion() + theme(legend.position = "none")
d
ggsave(filename = "t3_pc2.eps", device="eps", width=114, height=52, units = "mm" )

d<-c %>% 
  filter(Transect=="Transect 4") %>% 
  ggplot(.) +
  geom_point(aes(x=year, y=mean, color=Habitat), size=2)+
  geom_path(aes(x=year, y=mean, color=Habitat),size=1.25)+
  scale_color_viridis_d()+
  theme_dominion() + theme(legend.position = "none")
d
ggsave(filename = "t4_pc2.eps", device="eps", width=114, height=52, units = "mm" )

d<-c %>% 
  filter(Transect=="Transect 5") %>% 
  ggplot(.) +
  geom_point(aes(x=year, y=mean, color=Habitat), size=2)+
  geom_path(aes(x=year, y=mean, color=Habitat),size=1.25)+
  scale_color_viridis_d()+
  theme_dominion() + theme(legend.position = "none")
d
ggsave(filename = "t5_pc2.eps", device="eps", width=114, height=52, units = "mm" )

d<-c %>% 
  filter(Transect=="Transect 6") %>% 
  ggplot(.) +
  geom_point(aes(x=year, y=mean, color=Habitat), size=2)+
  geom_path(aes(x=year, y=mean, color=Habitat),size=1.25)+
  scale_color_viridis_d()+
  theme_dominion() + theme(legend.position = "none")
d
ggsave(filename = "t6_pc2.eps", device="eps", width=114, height=52, units = "mm" )

##### Method Comparison Plots
p<-pc_sum %>% 
 ggplot()+
  geom_boxplot(aes(x=Transect, y=))
