library(tidyverse)
library(readxl)
#####Salinity#####
salinity<-readRDS("rds/mean_daily_salinity2010_2018.rds")

a<-salinity %>% 
  filter(!date=="2014-03-16") %>%
  filter(!date=="2014-03-15") %>% 
  filter(!date=="2013-09-30") %>%
  filter(!date=="2013-10-01") %>%
  filter(!date=="2013-10-02") %>%
  filter(!date=="2013-10-03") %>%
  filter(!date=="2013-10-04") %>%
  filter(!date=="2013-10-05") %>%
  ggplot(., aes(x=date, y=salinity))+
  geom_line(color="black", size=1)+theme_classic()+
  geom_vline(aes(xintercept= as.Date("2011-05-01")), linetype = "dashed", color= "grey68")+
  scale_x_date(date_breaks = "year", date_labels = "%Y" )+
  scale_y_continuous(limits= c(0,13), minor_breaks = seq(0,14,2.5))+
  annotate("text",x=as.Date("2010-07-15"), y=13, label="Pre-Restoration", 
           color="grey53", size=4.5)+
  annotate("text",x=as.Date("2012-04-03"), y=13, label="Post-Restoration", 
           color="grey53", size=4.5)+
  xlab("Date")+ylab("Salinity (ppt)")
a

b<-salinity %>% 
  ggplot(., aes(x=date, y=salinity))+
  geom_point(color="black", size=1)+theme_classic()+scale_y_continuous(limits = c(0,75))+
  scale_x_date(limits = c(as.Date("2013-09-29"),as.Date("2013-10-15")))+xlab("Date")+ylab("Salinity (ppt)")
b



#####Shoreline#####
shoreline<-read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/beach_transects/data/fromGBA/20190327_profile-line_distances.xlsx",
                      col_types = c("date", "text", "text", 
                                    "text", "text", "text", "text", "text", 
                                    "text", "text", "text", "text", "text"), col_names = FALSE)
shore<-shoreline [,-(8:13), drop=FALSE]
shore<-shore [-(1:5),]
shore<-shore [-(17:18),]
colnames(shore)<-c("date", "LINE 1", "LINE 6", "LINE 7", "LINE 8", "LINE 9", "LINE 10")

beach<-shore %>% 
  gather(key="line", value = "ref_MLW", -date) %>% 
  mutate(date=as.Date(date),
         ref_MLW=as.numeric(ref_MLW))



lreg<-beach %>% 
  group_by(line) %>% 
  do(model= lm(ref_MLW ~ date, data = .))
lreg %>% tidy(model)
lreg %>% glance(model)


b_slope<-lreg %>% tidy(model) %>% 
  select(line, term, estimate)%>% 
  spread(., term, estimate)%>% 
  rename(slope = "date")
b_slopeb<-lreg %>% tidy(model) %>%
  select(line, term, std.error)%>% 
  spread(., term, std.error) %>% 
  rename(error="(Intercept)", std_err = "date")
b_slope<-full_join(b_slope, b_slopeb, by = "line")
rm(b_slopeb)
b_slope<-b_slope %>% 
  mutate(yr_slope=slope*365)
b_slope$sign<-ifelse(b_slope$slope > 0, "Positive", "Negative")
b_slope$line<-factor(b_slope$line, levels = c("LINE 1", "LINE 6", "LINE 7",
                                              "LINE 8", "LINE 9", "LINE 10"))
my.formula<-y~x
pos_neg_colors<-c("Positive"="green3", "Negative"="red2")

d<-ggplot(b_slope, aes(x=line, y=slope*365, fill=sign))+
  geom_errorbar(aes(ymax=slope*365+std_err*365, ymin=slope*365-std_err*365),
                width = 0.25)+
  geom_hline(yintercept=0, size = 0.5, color = "black")+
  geom_bar(stat = "identity", color = "black")+
  ggtitle("Rate of Shoreline Change by Transect (March 2011 - March 2019)")+
  xlab("Transects from North to South (Transects 2-5 are behind the revetment)")+
  ylab("Rate of Shoreline Change (ft/yr)")+
  scale_fill_manual(values = pos_neg_colors)+
  theme_classic()+
  theme(legend.position = "none", 
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(), 
        axis)+
  geom_text(aes(label= sprintf("%0.2f", 
                               round(slope*365, digits = 2))), 
            position=position_dodge(.9),
            vjust=ifelse(b_slope$slope > 0, 1.5, -1), size=3.25)
d

ggsave("plots/manuscript_shoreline.jpg")



c<-beach %>% 
  filter(line=="LINE 8") %>% 
  ggplot(aes(x=date, y=ref_MLW))+
  geom_point(shape=24, fill="lightsalmon2")+
  geom_smooth(method=lm, alpha=0.5, color="lightsalmon2", linetype="dashed", se=FALSE)+
  ggtitle("The shoreline at Line 8 is retreating ~5.37 ft per year")+
  ylab("Distance to MLW (ft)")+theme_classic()
c
ggsave("plots/line8_m2019.jpg")


e<-beach %>% 
  filter(line=="LINE 1") %>% 
  ggplot(aes(x=date, y=ref_MLW))+
  geom_point(shape=24, fill="lightsalmon2")+
  geom_smooth(method=lm, alpha=0.5, color="lightsalmon2", linetype="dashed", se=FALSE)+
  ggtitle("The shoreline at Line 1 is increasing ~11.55 ft per year")+
  ylab("Distance to MLW (ft)")+theme_classic()
e
ggsave("plots/line1_m2019.jpg")

f<-beach %>% 
  filter(line=="LINE 9") %>% 
  ggplot(aes(x=date, y=ref_MLW))+
  geom_point(shape=24, fill="lightsalmon2")+
  geom_smooth(method=lm, alpha=0.5, color="lightsalmon2", linetype="dashed", se=FALSE)+
  ggtitle("The shoreline at Line 9 is retreating ~3.21 ft per year")+
  ylab("Distance to MLW (ft)")+theme_classic()
f
ggsave("plots/line9_m2019.jpg")


g<-beach %>% 
  filter(line=="LINE 7") %>% 
  ggplot(aes(x=date, y=ref_MLW))+
  geom_point(shape=24, fill="lightsalmon2")+
  geom_smooth(method=lm, alpha=0.5, color="lightsalmon2", linetype="dashed", se=FALSE)+
  ggtitle("The shoreline at Line 7 is retreating ~2.77 ft per year")+
  ylab("Distance to MLW (ft)")+theme_classic()
g
ggsave("plots/line7_m2019.jpg")


h<-beach %>% 
  filter(line=="LINE 6") %>% 
  ggplot(aes(x=date, y=ref_MLW))+
  geom_point(shape=24, fill="lightsalmon2")+
  geom_smooth(method=lm, alpha=0.5, color="lightsalmon2", linetype="dashed", se=FALSE)+
  ggtitle("The shoreline at Line 6 is retreating ~1.12 ft per year")+
  ylab("Distance to MLW (ft)")+theme_classic()
h
ggsave("plots/line6_m2019.jpg")

k<-beach %>% 
  filter(line=="LINE 10") %>% 
  ggplot(aes(x=date, y=ref_MLW))+
  geom_point(shape=24, fill="lightsalmon2")+
  geom_smooth(method=lm, alpha=0.5, color="lightsalmon2", linetype="dashed", se=FALSE)+
  ggtitle("The shoreline at Line 10 is retreating ~1.70 ft per year")+
  ylab("Distance to MLW (ft)")+theme_classic()
k
ggsave("plots/line10_m2019.jpg")