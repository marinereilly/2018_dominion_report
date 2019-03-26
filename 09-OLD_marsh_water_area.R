#####packages#####
library(tidyverse)
library(magrittr)
library(stringr)
library(cowplot)

#####load data#####
###acreage is all delineated by Erin/Amanda RE the CERF 2017 analysis
###it doesn't go to 2010-2011 because of projection/time issues
###in the 2017 report I am using the pervious delineations (delineation)+ the 
###2017 delineation for CERF for continuity.  there is about a 10% difference in 
###marsh area between the two so I don't feel like I can use the Lora Pre-restoration
###images and the Erin post restoration delineations
library(readxl)
acreage <- read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/DATA/GPS and GIS Documents/ArcGIS_acreage.xlsx")
View(acreage)

delineation <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/DATA/2016 Data/Delineation.csv")


#####format#####
acreage %<>%
  rename(Year=X__1)
acreage$Year <- as.factor(acreage$Year)

delineation %<>%
  add_row(.,Date = "Aug-2017", Type="Marsh", Acres="78.69")
delineation %<>%
    add_row(.,Date = "Aug-2017", Type="Water", Acres="71.57") %>% 
  select(., -starts_with("X"))
delineation$Acres<-as.numeric(delineation$Acres)
delineation$Date<- factor(delineation$Date, levels = c("Aug-2010", "Aug-2011", "Aug-2012", "Sep-2013", "Aug-2014", "Aug-2015", "Aug-2016",  "Aug-2017"))

d2<-delineation 
d2<- d2 %>% 
  spread(Type, Acres)
d2<- d2 %>% 
  separate(Date, c("Month", "Year"), "-")
ai_flight$Year<-as.character(ai_flight$Year)
ai_flight <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/DATA/R code/dominion_other/ai_flight.csv")
ai_flight2<-ai_flight %>% 
  full_join(., d2, by="Year")

d3<-acreage
ai_flight3<-ai_flight %>% 
  full_join(., d3, by="Year")
  
acreage_b<-acreage %>% 
  gather(., key = Habitat, value = Acres, Water, Marsh)
#####plots#####
a<- ggplot(data = acreage_b, aes(x=Year, y=Acres, fill=forcats::fct_rev(Habitat)))+
  geom_col(position = "stack", color="black")+
  scale_fill_manual(values = c("Marsh"="chartreuse3", "Water"="cyan3"))+
  geom_text(aes(label= sprintf("%0.2f", round(Acres, digits = 2))), position=position_stack(vjust=0.5))+
  guides(fill=guide_legend(title="Habitat"))+
  ggtitle("Area of Marsh and Water since 2012")
a
#This plot is ugly
b<- ggplot(data=acreage_b, aes(x=Year, y=Acres, fill=Habitat))+
  geom_point(shape=21, size=4)+
  geom_text(aes(label= sprintf("%0.2f", round(Acres, digits = 2))), vjust=1.45)+
  scale_fill_manual(values = c("Marsh"="chartreuse3", "Water"="cyan3"))+coord_flip()+
  ggtitle("Area of Marsh and Water since 2012")
b

###Ratio Plot### Don't use yet
c<- ggplot(data=acreage, aes(x=Year, y=Water/Marsh))+
  geom_point()+
  scale_y_continuous(limits = c(0,1))+
  ggtitle("Area of Marsh and Water since 2012")
c
###delineations plot
a<- ggplot(data = acreage_b, aes(x=Year, y=Acres, fill=forcats::fct_rev(Habitat)))+
  geom_col(position = "stack", color="black")+
  scale_fill_manual(values = c("Marsh"="chartreuse3", "Water"="cyan3"))+
  geom_text(aes(label= sprintf("%0.2f", round(Acres, digits = 2))), position=position_stack(vjust=0.5))+
  guides(fill=guide_legend(title="Habitat"))+
  ggtitle("Area of Marsh and Water since 2012")
a


d<- ggplot(data = delineation, aes(x=Date, y=Acres, fill=forcats::fct_rev(Type)))+
  geom_col(position = "stack", color="black")+
  scale_fill_manual(values = c("Marsh"="chartreuse3", "Water"="cyan3"))+
  geom_text(aes(label= sprintf("%0.2f", round(Acres, digits = 2))), position=position_stack(vjust=0.5))+
  guides(fill=guide_legend(title="Habitat"))+
  ggtitle("Area of Marsh and Water at Cove Point since 2010")
d


######Plot rain v MW ratio####
e<-ggplot(ai_flight3)+
  geom_point(aes(y=Marsh/Water, x=two_week_cum_rainfall, color=Year))
e

f<-ggplot(ai_flight3)+
  geom_point(aes(y=Marsh/Water, x=wy_cum_rainfall, color=Year))
f

ai_flight3$ratio<-ai_flight3$Marsh/ai_flight3$Water
ai_flight2$ratio<-ai_flight2$Marsh/ai_flight2$Water
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
