library(nflfastR)
library(tidyverse)


seasons<- 2019:2020
pbp<- nflfastR::load_pbp(seasons)


pass <-pbp%>% filter(qb_dropback==1,play_type=="pass")
passEPA <- pass%>% 
  select(posteam,passer_player_name,qb_epa,pass_attempt)%>%
  group_by(passer_player_name)%>%
  summarize(qb_epa_per_play=mean(qb_epa, na.rm = TRUE),attempts=sum(pass_attempt))%>%
  filter(attempts>=200)


