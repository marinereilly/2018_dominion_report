library(sf)
library(dplyr)
library(tidyr)

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
