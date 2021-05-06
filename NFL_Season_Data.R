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

passmain <-pbp%>% filter(qb_dropback==1,play_type=="pass",down<=3,half_seconds_remaining>=120)
passEPAMain <- passmain%>% 
  select(posteam,passer_player_name,qb_epa,pass_attempt)%>%
  group_by(passer_player_name)%>%
  summarize(qb_epa_per_play=mean(qb_epa, na.rm = TRUE),attempts=sum(pass_attempt))%>%
  filter(attempts>=200)

passtrailingend <-pbp%>% filter(qb_dropback==1,play_type=="pass",score_differential<=0,score_differential>=-8,half_seconds_remaining<=120)
passEPAtrailend <- passtrailingend%>% 
  select(posteam,passer_player_name,qb_epa,pass_attempt)%>%
  group_by(passer_player_name)%>%
  summarize(qb_epa_per_play=mean(qb_epa, na.rm = TRUE),attempts=sum(pass_attempt))%>%
  filter(attempts>=60)
