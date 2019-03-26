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
  mutate(year="2017")
rm(peet_plots_2017)

species<-peet_2018 %>% 
  select(Peet_plot, Subplot, `Level(m2)`, ScientificName) %>% 
  group_by(Peet_plot, Subplot, `Level(m2)`) %>%
  summarise(SpeciesNum=n_distinct(ScientificName))
