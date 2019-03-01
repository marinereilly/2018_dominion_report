#####Run First####
library(dplyr)
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
  ggsave(as.character(paste0("/plots/",filename,".eps")), device=eps, width=8, height=6 )
}



cp_pal<-c("north"="#FF5765", "mid" = "#FFDB15", "south"="#8A6FDF", "weir"="#A8E10C")
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
