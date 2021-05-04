library(nflfastR)
library(tidyverse)


seasons<- 2019:2020
pbp<- nflfastR::load_pbp(seasons)
