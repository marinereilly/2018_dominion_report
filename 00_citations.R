library(tidyr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(hablar)
library(RColorBrewer)
library(viridis)
library(readxl)
library(broom)
library(ggpmisc)
library(sf)
library(purrr)


sink("test.bib")
out <- sapply(names(sessionInfo()$otherPkgs), 
              function(x) print(citation(x), style = "Bibtex"))

out

