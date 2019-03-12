#####Beach Profile Analysis for Dominion######
#####Packages#####
library(tidyverse)
library(lubridate)
library(broom)
library(ggpmisc)

#####Load the data#####
shoreline<-read.csv("H://0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/DATA/2017 Data/Beach Profiles/shorelinechange.csv")
View(shoreline)
shoreline<- separate(shoreline, SurveyDate, c("Month","Year"), sep = "-")
shoreline$Year<-paste("20", shoreline$Year, sep = "")
shoreline$survey_date<-mdy(paste(shoreline$Month, "-01-", shoreline$Year, sep = ""))
shoreline<- shoreline %>% 
  select(survey_date, Transect, ShorelineChange)

#####Linear Regressions#####
linear_regressions<-shoreline %>% 
  group_by(Transect) %>%
  do(model = lm(ShorelineChange ~ survey_date, data = .))
linear_regressions %>% tidy(model)
linear_regressions %>% glance(model)

#####Pull Slopes and errors#####
shoreline_slope<-linear_regressions %>% tidy(model) %>% 
  select(Transect, term, estimate)%>% 
  spread(., term, estimate) %>% 
  rename(slope = "survey_date")
shoreline_slopeb<-linear_regressions %>% tidy(model) %>% 
  select(Transect, term, std.error)%>% 
  spread(., term, std.error) %>% 
  rename(error="(Intercept)", std_err = "survey_date")
shoreline_slope<-full_join(shoreline_slope, shoreline_slopeb, by = "Transect")
View(shoreline_slope)
rm(shoreline_slopeb)
shoreline_slope$sign<-ifelse(shoreline_slope$slope > 0, "Positive", "Negative")
shoreline_slope$Transect<-factor(shoreline_slope$Transect, levels = c("LINE 1", "LINE 6", "LINE 7",
                                                                      "LINE 8", "LINE 9", "LINE 10"))
#####Formatting for T1 and T9######
t1<-shoreline %>% 
  filter(Transect=="LINE 1")
t9<-shoreline %>% 
  filter(Transect=="LINE 9")
t8<-shoreline %>% 
  filter(Transect=="LINE 8")

#####Plots####
my.formula<-y~x
pos_neg_colors<-c("Positive"="green3", "Negative"="red2")

a<-ggplot(shoreline, aes(x=survey_date, y=ShorelineChange, color=Transect))
b<-a+geom_point()+geom_smooth(method = lm)
b

c<-a+geom_point()+geom_smooth(method = lm)+
  stat_poly_eq(formula = my.formula, aes(x=survey_date, y=ShorelineChange, color=Transect, label = 
                                           paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, label.y.npc = .99)+
  scale_y_continuous(limits= c(0,300))
c

d<-ggplot(shoreline_slope, aes(x=Transect, y=slope*365, fill=sign))+
  geom_hline(yintercept=0, size = 1, color = "grey43")+
    geom_bar(stat = "identity", color = "black")+
  ggtitle("Rate of Shoreline Change by Transect")+
  xlab("Transects from North to South (Transects 2-5 are behind the revetment)")+
  ylab("Rate of Shoreline Change (ft/yr)")+
  scale_fill_manual(values = pos_neg_colors)+
  theme_minimal()+
  theme(legend.position = "none")+
  geom_text(aes(label= sprintf("%0.2f", 
                               round(slope*365, digits = 2))), 
            position=position_dodge(.9),
            vjust=ifelse(shoreline_slope$slope > 0, 1.5, -1), size=3.25)

d
ggsave("2017_ShorelineCHangeRate.png")

t1r2<-"R^2 == 0.869"
#https://stackoverflow.com/questions/9723239/ggplot2-annotation-with-superscripts?rq=1
e<- ggplot(data = t1, aes(x=survey_date, y=ShorelineChange))+
  geom_point(fill="lightsalmon2", shape=24, size=2.5)+
  geom_smooth(method = lm, se=FALSE, color="lightsalmon2", linetype="dashed")+
  scale_x_date(date_labels = "%b %Y", date_breaks = "year")+
  annotate("text", label = "y = 0.0336x - 477", x = as.Date("2011-06-30"), y = 104, colour = "black")+
  annotate("text", label = t1r2, x = as.Date("2011-04-28"), y = 98, colour = "black", parse=TRUE)+
  theme_minimal()+
  ggtitle("The Beach at Transect 1 is growing ~12.26 ft/year")+
  xlab("Survey Date")+
  ylab("Shoreline Change (ft)")
e

t9r2<-"R^2 == 0.631"  
f<- ggplot(data = t9, aes(x=survey_date, y=ShorelineChange))+
  geom_point(fill="lightsalmon2", shape=25, size=2.5)+
  geom_smooth(method = lm, se=FALSE, color="lightsalmon2", linetype="dashed")+
  scale_x_date(date_labels = "%b %Y", date_breaks = "year")+
  annotate("text", label = "y = 202 - 0.00716x", x = as.Date("2011-07-30"), y = 101, colour = "black")+
  annotate("text", label = t9r2, x = as.Date("2011-04-28"), y = 99, colour = "black", parse=TRUE)+
  theme_minimal()+
  ggtitle("The Beach at Transect 9 is Shrinking by ~2.6 ft/yr")+
  xlab("Survey Date")+
  ylab("Shoreline Change (ft)")
f

t8r2<- "R^2 == 0.959"
g<- ggplot(data = t8, aes(x=survey_date, y=ShorelineChange))+
  geom_point(fill="lightsalmon2", shape=25, size=2.5)+
  geom_smooth(method = lm, se=FALSE, color="lightsalmon2", linetype="dashed")+
  scale_x_date(date_labels = "%b %Y", date_breaks = "year")+
  annotate("text", label = "y = 297 - 0.0142x", x = as.Date("2011-07-15"), y = 102, colour = "black")+
  annotate("text", label = t9r2, x = as.Date("2011-04-28"), y = 98, colour = "black", parse=TRUE)+
  theme_minimal()+
  ggtitle("The Beach at Transect 8 is Shrinking by ~5.183 ft/yr")+
  xlab("Survey Date")+
  ylab("Shoreline Change (ft)")
g


#####2018 analysis#####
library(readxl)
beach <- read_excel("data/beach_transects_all_years.xlsx")
beach$Date<-ymd(beach$Date)
lreg<-beach %>% 
  group_by(Line) %>% 
  do(model= lm(ref_MLW ~ Date, data = .))
lreg %>% tidy(model)
lreg %>% glance(model)
b_slope<-lreg %>% tidy(model) %>% 
  select(Line, term, estimate)%>% 
  spread(., term, estimate) %>% 
  rename(slope = "Date")
b_slopeb<-lreg %>% tidy(model) %>%
  select(Line, term, std.error)%>% 
  spread(., term, std.error) %>% 
  rename(error="(Intercept)", std_err = "Date")
b_slope<-full_join(b_slope, b_slopeb, by = "Line")
rm(b_slopeb)
b_slope$sign<-ifelse(b_slope$slope > 0, "Positive", "Negative")
b_slope$Line<-factor(b_slope$Line, levels = c("LINE 1", "LINE 6", "LINE 7",
                                              "LINE 8", "LINE 9", "LINE 10"))
my.formula<-y~x
pos_neg_colors<-c("Positive"="green3", "Negative"="red2")

d<-ggplot(b_slope, aes(x=Line, y=slope*365, fill=sign))+
  geom_errorbar(aes(ymax=slope*365+std_err*365, ymin=slope*365-std_err*365),
                 width = 0.25)+
  geom_hline(yintercept=0, size = 1, color = "grey43")+
  geom_bar(stat = "identity", color = "black")+
  ggtitle("Rate of Shoreline Change by Transect")+
  xlab("Transects from North to South (Transects 2-5 are behind the revetment)")+
  ylab("Rate of Shoreline Change (ft/yr)")+
  scale_fill_manual(values = pos_neg_colors)+
  theme_dominion()+
  theme(legend.position = "none")+
  geom_text(aes(label= sprintf("%0.2f", 
                               round(slope*365, digits = 2))), 
            position=position_dodge(.9),
            vjust=ifelse(b_slope$slope > 0, 1.5, -1), size=3.25)
d

a<-ggplot(beach, aes(x=Date, y=ref_MLW, color=Line))
b<-a+geom_point()+geom_smooth(method = lm)
b
b_slope$yslope<-b_slope$slope*365

c<-beach %>% 
  filter(Line=="LINE 8") %>% 
  ggplot(aes(x=Date, y=ref_MLW))+
  geom_point(shape=24, fill="lightsalmon2")+
  geom_smooth(method=lm, alpha=0.5, color="lightsalmon2", linetype="dashed", se=FALSE)+
  ggtitle("The shoreline at Line 8 is retreating ~5.04 ft per year")+
  ylab("Distance to MLW (ft)")+theme_minimal()
c

e<-beach %>% 
  filter(Line=="LINE 1") %>% 
  ggplot(aes(x=Date, y=ref_MLW))+
  geom_point(shape=24, fill="lightsalmon2")+
  geom_smooth(method=lm, alpha=0.5, color="lightsalmon2", linetype="dashed", se=FALSE)+
  ggtitle("The shoreline at Line 1 is increasing ~11.74 ft per year")+
  ylab("Distance to MLW (ft)")+theme_minimal()
e

f<-beach %>% 
  filter(Line=="LINE 9") %>% 
  ggplot(aes(x=Date, y=ref_MLW))+
  geom_point(shape=24, fill="lightsalmon2")+
  geom_smooth(method=lm, alpha=0.5, color="lightsalmon2", linetype="dashed", se=FALSE)+
  ggtitle("The shoreline at Line 9 is retreating ~3.04 ft per year")+
  ylab("Distance to MLW (ft)")+theme_minimal()
f

g<-beach %>% 
  filter(Line=="LINE 7") %>% 
  ggplot(aes(x=Date, y=ref_MLW))+
  geom_point(shape=24, fill="lightsalmon2")+
  geom_smooth(method=lm, alpha=0.5, color="lightsalmon2", linetype="dashed", se=FALSE)+
  ggtitle("The shoreline at Line 7 is retreating ~2.92 ft per year")+
  ylab("Distance to MLW (ft)")+theme_minimal()
g

h<-beach %>% 
  filter(Line=="LINE 6") %>% 
  ggplot(aes(x=Date, y=ref_MLW))+
  geom_point(shape=24, fill="lightsalmon2")+
  geom_smooth(method=lm, alpha=0.5, color="lightsalmon2", linetype="dashed", se=FALSE)+
  ggtitle("The shoreline at Line 6 is retreating ~1.32 ft per year")+
  ylab("Distance to MLW (ft)")+theme_minimal()
h

i<-beach %>% 
  filter(Line=="LINE 10") %>% 
  ggplot(aes(x=Date, y=ref_MLW))+
  geom_point(shape=24, fill="lightsalmon2")+
  geom_smooth(method=lm, alpha=0.5, color="lightsalmon2", linetype="dashed", se=FALSE)+
  ggtitle("The shoreline at Line 10 is retreating ~1.71 ft per year")+
  ylab("Distance to MLW (ft)")+theme_minimal()
i
