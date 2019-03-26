library(dplyr)
library(ggplot2)

#####load data in 3 parts#####
peet_2012_2016 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/peet_plots/data/SpeciesArea.csv")
peet_plots_2017 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/peet_plots/data/2017_PEET_Plots_Species_Area.csv")
library(readxl)
peet_2018 <- read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/peet_plots/data/2018 PEET Plots.xlsx",
sheet = "Area Level")

#####tidy data so you can combine them###
peet_2012_2016<-peet_2012_2016 %>% 
  rename(plot=Plot, year=Year, plot_size=Size, species_num=SpeciesNum)

peet_2017<-peet_plots_2017 %>% 
  mutate(plot=paste0("Plot ", Plot)) %>% 
  group_by(plot, Size) %>% 
  summarise(species_num=mean(Species.Number)) %>%
  rename(plot_size=Size) %>% 
  mutate(year=as.integer("2017"))
rm(peet_plots_2017)

species25<-peet_2018 %>% 
  select(Peet_plot, Subplot, `Level(m2)`, ScientificName) %>% 
  group_by(Peet_plot, Subplot, `Level(m2)`) %>%
  summarise(SpeciesNum=n_distinct(ScientificName)) %>% 
  filter(`Level(m2)`==0.25) %>% 
  rename(plot_size=`Level(m2)`)
species4<-peet_2018 %>% 
  select(Peet_plot, Subplot, `Level(m2)`, ScientificName) %>% 
  group_by(Peet_plot, Subplot) %>%
  summarise(SpeciesNum=n_distinct(ScientificName)) %>% 
  filter(!Subplot==40) %>% 
  mutate(plot_size=4)
species40<-peet_2018 %>% 
  select(Peet_plot, Subplot, `Level(m2)`, ScientificName) %>% 
  group_by(Peet_plot) %>%
  summarise(SpeciesNum=n_distinct(ScientificName)) %>% 
  mutate(plot_size=40, Subplot=40)
species<-species25 %>% 
  bind_rows(., species4, species40)
rm(species25,species4,species40)
peet_2018<-species %>% 
  mutate(plot=paste0("Plot ", Peet_plot)) %>% 
  group_by(plot, plot_size) %>% 
  summarise(species_num=mean(SpeciesNum)) %>%
  mutate(year=as.integer("2018"))

peet_all<-peet_2012_2016 %>% 
  bind_rows(., peet_2017, peet_2018)
saveRDS(peet_all, "peet_species_area_2012_2018.rds")
rm(peet_2012_2016, peet_2017, peet_2018, species)

