library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)

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

#####make plots#####
peet<-readRDS("peet_species_area_2012_2018.rds")

make_curve<-function(df)
  {
  library(plyr)
  library(broom)
  ddply(df, "plot_id",
        function(u){
          r<-lm(log(species_num) ~ log(plot_size), data = u)
          tidy(r) %>% 
            select(-(std.error:p.value)) %>% 
            spread(term, estimate)
        })
  }

curves<-peet %>% 
  mutate(plot_id=paste0(plot,"_",year)) %>% 
  make_curve(.)
View(curves)  

curves<-curves %>% 
  filter(!plot_id=="Plot A_2016") %>% 
  filter(!plot_id=="Plot B_2013") %>% 
  filter(!plot_id=="Plot C_2013")

curves<-curves %>% 
  separate(., col=plot_id, into = c("plot", "year"), sep = "_")

colnames(curves)<-c("plot","year","intercept","l_plot_size")

x_values <- seq(0, 40, 0.1)

prepped <- curves %>% 
  mutate(
    x_values = map(seq_along(nrow(.)), ~x_values),
    y_values = pmap(list(x_values, intercept, l_plot_size), 
                    function(x_values, a, b) exp(a) * x_values ^ b))%>%
  select(-intercept, -l_plot_size) %>%
  unnest(x_values, y_values)%>%
  group_nest(plot)

sa_plots<-map2(prepped$plot, prepped$data, 
               function(title, input){
                 ggplot(input, aes(x = x_values, y = y_values, color = year)) +
                   geom_line(size=1) +
                   labs(title = title) +
                   xlab("Plot Size m2") +
                   ylab("Number of Species") +
                   theme_classic()
               })
sa_plots

#####Species number plots


a<-peet %>% 
  filter(plot_size==40) %>% 
  ggplot()+
  geom_bar(aes(x=year, y=species_num, fill=year), stat="identity", color="black")+
  facet_wrap(plot~.)+ 
  theme_dominion()+
  xlab("Year")+
  ylab("Number of Species")+
  theme(strip.text.x = element_text(size=12, face="bold"),
        strip.background = element_rect(fill="gainsboro"),
        legend.position = "none")+
  scale_fill_gradient(low = "hotpink", high = "purple4")+
  ggtitle("Total PEET Plot Species by Year")
a    

ggsave("peet_sp_num.eps")

curves$year<-as.numeric(curves$year)

b<-curves %>% 
  ggplot()+
  geom_point(aes(x=year, y=l_plot_size), size=3)+
  geom_line(aes(x=year, y=l_plot_size), size=1)+
  facet_wrap(plot~.)+
  theme_dominion()+
  xlab("Year")+
  ylab("Exponent")+
  theme(strip.text.x = element_text(size=12, face="bold"),
                     strip.background = element_rect(fill="gainsboro"))
b

ggsave("peet_exp.eps")
