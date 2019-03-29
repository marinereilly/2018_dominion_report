library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)

stems_2018 <- read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/percent_cover/data/2018_PercentCover.xlsx", 
                         sheet = "Stems")
stems_2018<-stems_2018 %>% 
  mutate(plot_id = paste(Transect,Habitat,Plot, sep = "_")) %>% 
  select(Date, plot_id, Transect, Habitat, Plot, Species, Length_cm)

stems_2017 <- read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/DATA/2017 Data/2017StemHeights.xlsx")

stems_2017<- stems_2017 %>% 
  mutate(Transect= 
           recode(Transect,
                  "T6"=6,
                  "T5"=5,
                  "T4"=4,
                  "T3"=3,                          
                  "T2"=2,
                  "T1"=1)) %>% 
  mutate(Habitat=
           recode(Habitat,
                  "Bm"="B",
                  "Fw"="FW",
                  "Sw"="SW")) %>% 
  mutate(plot_id = paste(Transect,Habitat,Plot, sep = "_"))%>% 
  select(Date, plot_id, Transect, Habitat, Plot, Species, Length_cm=Height)

stems<-stems_2017 %>% 
  bind_rows(., stems_2018)

saveRDS(stems, "stem_heights_2017_2018.rds")

a<-stems %>% 
  mutate(year=as.factor(lubridate::year(Date))) %>% 
  ggplot()+
  geom_bar(aes(x=Species, fill=year),postion="dodge")+
  coord_flip()
a

b<-stems %>% 
  mutate(year=as.factor(lubridate::year(Date))) %>%
  filter(Species=="Spartina patens"|
           Species=="Spartina alterniflora"|
           Species=="Distichlis spicata") %>% 
  ggplot()+
  geom_freqpoly(aes(x=Length_cm, color=Species), binwidth=10 ,size=1.2)+
  facet_grid(year~.)+theme_minimal()+
  scale_color_manual(values=wes_palette("Cavalcanti1"))+
  geom_vline(data=stats, aes(xintercept=av_length, color=Species), 
             linetype="dashed")
b

ggsave("stemheights.eps")

stats<-stems %>% 
  mutate(year=as.factor(lubridate::year(Date))) %>%
  filter(Species=="Spartina patens"|
           Species=="Spartina alterniflora"|
           Species=="Distichlis spicata") %>%
  group_by(year, Species) %>% 
  summarize(av_length=mean(Length_cm),
           se_length=plotrix::std.error(Length_cm),
           med_length=median(Length_cm), 
           max_length=max(Length_cm),
           num_measurements=length(Length_cm)
           )
View(stats)
saveRDS(stats, "stem-height-stats.rds")
