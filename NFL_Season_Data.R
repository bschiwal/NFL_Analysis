###Load Libraries

library(nflfastR)
library(tidyverse)
library(ggplot2)
library(ggrepel)

###Load play by play data
seasons<- 2019:2020
pbp<- nflfastR::load_pbp(seasons)
colr<-teams_colors_logos%>% select(team_abbr,team_color)

###Load PBP for 2021
pbp21<- nflfastR::load_pbp(2021)

##Filter to just dropback pass plays
pass <-pbp21%>% filter(qb_dropback==1,play_type=="pass")


passEPA <- pass%>% 
  select(posteam,passer_player_name,qb_epa,pass_attempt)%>%
  group_by(passer_player_name)%>%
  summarize(qb_epa_per_play=mean(qb_epa, na.rm = TRUE),attempts=sum(pass_attempt))%>%
  filter(attempts>=200)

passmain <-pbp21%>% filter(qb_dropback==1,play_type=="pass",down<=3,half_seconds_remaining>=120)
passEPAMain <- passmain%>% 
  select(posteam,passer_player_name,qb_epa,pass_attempt)%>%
  group_by(passer_player_name)%>%
  summarize(qb_epa_per_play=mean(qb_epa, na.rm = TRUE),attempts=sum(pass_attempt))%>%
  filter(attempts>=200)

passtrailingend <-pbp21%>% filter(qb_dropback==1,
                                play_type=="pass",
                                score_differential<=0,
                                score_differential>=-8,
                                half_seconds_remaining<=120)


pte20 <-pbp21%>% filter(qb_dropback==1,
               play_type=="pass",
               score_differential<=0,
               score_differential>=-8,
               half_seconds_remaining<=120,
               season==2020)
pte20f<-pte20%>%
  select(posteam,passer_player_name,qb_epa,pass_attempt)%>%
  group_by(passer_player_name)%>%
  summarize(qb_epa_per_play=mean(qb_epa, na.rm = TRUE),attempts=sum(pass_attempt))%>%
  filter(attempts>=20)
               
                
pte19 <-pbp%>% filter(qb_dropback==1,
                play_type=="pass",
                score_differential<=0,
                score_differential>=-8,
                half_seconds_remaining<=120,
                season==2019)
pte19f<-pte19%>%
  select(posteam,passer_player_name,qb_epa,pass_attempt)%>%
  group_by(passer_player_name)%>%
  summarize(qb_epa_per_play=mean(qb_epa, na.rm = TRUE),attempts=sum(pass_attempt))%>%
  filter(attempts>=20)

ptefinal<- passtrailingend

passEPAtrailend <- passtrailingend%>% 
  select(posteam,posteam,passer_player_name,qb_epa,pass_attempt,cpoe)%>%
  group_by(passer_player_name,posteam)%>%
  summarize(qb_epa_per_play=mean(qb_epa, na.rm = TRUE),attempts=sum(pass_attempt),cpoe=mean(cpoe,na.rm=TRUE))%>%
  filter(attempts>=60)%>%
  inner_join(colr,by = c("posteam"= "team_abbr"))

ggplot(passEPAtrailend, aes(cpoe, qb_epa_per_play))+
  geom_point(color=passEPAtrailend$team_color, 
             cex=passEPAtrailend$attempts/15)+ 
  geom_text_repel(aes(label=passer_player_name),force=2)+
  labs(x= "Completion % Over Expectation (CPOE)", 
       y= "QB EPA per Pass Thrown", 
       title="Quarterback Clutch Efficency 2019-2020", 
       subtitle ="Dropback Passes in Final Two Minutes of Half When Trailing by < 8 Points", 
       caption="Made by @bschiwal; Data from @nflfastR")+
  geom_hline(yintercept=0,color="red",linetype="dashed")+
  geom_vline(xintercept=0,color="red",linetype="dashed")+
  theme_bw()+
  theme(plot.title=element_text(size=14,hjust=.5,face="bold"),
        plot.subtitle = element_text(size=10,hjust=.5))+
  stat_smooth(geom="line",method="lm", alpha=.75)
  
