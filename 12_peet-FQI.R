library(ggplot2)
library(dplyr)

colo3<-c("Plot A"="#69B5B3", "Plot B"="#E87770", "Plot C"="blue4")
colo4<-c("Plot A"="#d94801", "Plot B"="#2171b5", "Plot C"="#238b45")

PEETplots_FQI <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/peet_plots/data/PEETplots_FQI.csv", stringsAsFactors=FALSE)
PEETplots_FQI$Date<-lubridate::mdy(PEETplots_FQI$Date)

f<-PEETplots_FQI %>% 
  filter(Plot=="Plot A"|Plot=="Plot B"|Plot=="Plot C") %>% 
  ggplot()+
  geom_line(aes(x=Date, y=FQI_Score, color=Plot), size=1)+
  scale_color_manual(values=colo4, breaks= c("Plot A", "Plot B", "Plot C"), labels=c("Plot A", "Plot B", "Plot C"))+ 
  geom_point(aes(x=Date, y=FQI_Score, color=Plot), size=2)+
  geom_hline(yintercept=12.9, show.legend=FALSE, colour="#2171b5", linetype=4, size=1)+
  geom_hline(yintercept=8.25, show.legend=FALSE, colour="#238b45", linetype=4, size=1)+
  geom_hline(yintercept=12.0, show.legend=FALSE, colour="#d94801", linetype=4, size=1)+
  ylab("FQI Score")+ggtitle("Floristic Quality Index Scores over Time")+
  theme_dominion()
f

plot_save("FQI")

f<-PEETplots_FQI %>% 
  filter(Plot=="Plot A"|Plot=="Plot B"|Plot=="Plot C") %>% 
  ggplot()+
  geom_line(aes(x=Date, y=Adjusted_FQI, color=Plot), size=1)+
  scale_color_manual(values=colo4, breaks= c("Plot A", "Plot B", "Plot C"), labels=c("Plot A", "Plot B", "Plot C"))+ 
  geom_point(aes(x=Date, y=Adjusted_FQI, color=Plot), size=2)+
  geom_hline(yintercept=36.7, show.legend=FALSE, colour="#2171b5", linetype=4, size=1)+
  geom_hline(yintercept=25.0, show.legend=FALSE, colour="#238b45", linetype=4, size=1)+
  geom_hline(yintercept=44.0, show.legend=FALSE, colour="#d94801", linetype=4, size=1)+
  ylab("Adjusted FQI Score")+
  ggtitle("Adjusted Floristic Quality Index Scores over Time")+
  theme_dominion()
f

plot_save("adjusted-FQI")
