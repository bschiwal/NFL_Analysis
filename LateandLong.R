###Load Libraries

library(nflfastR)
library(tidyverse)
library(ggplot2)
library(ggrepel)

###Load play by play data
seasons<- 2021
pbp<- nflfastR::load_pbp(seasons)
colr<-teams_colors_logos%>% select(team_abbr,team_color)



### create df rush on 2nd down and long
rush<-pbp%>%
  filter(down==2 | down==3,ydstogo>=8,play_type=="run",qb_scramble==0, half_seconds_remaining>120,play==1)%>%
  select(posteam, rushing_yards,epa,success,play)%>%
  group_by(posteam)%>%
  summarise(avgYards=mean(rushing_yards,na.rm=TRUE),avgEPA=mean(epa),pctSuccess=mean(success),playcnt=(sum(play)))%>%
  inner_join(colr,by = c("posteam"= "team_abbr"))

pass<-  pbp%>%
  filter(down==2 | down==3,ydstogo>=8,play_type=="pass",qb_scramble==0, half_seconds_remaining>120,play==1)%>%
  select(posteam, yards_gained,epa,success,play)%>%
  group_by(posteam)%>%
  summarise(avgYards=mean(yards_gained,na.rm=TRUE),avgEPA=mean(epa),pctSuccess=mean(success),playcnt=(sum(play)))%>%
  inner_join(colr,by = c("posteam"= "team_abbr"))
rm(colr,pbp,seasons)

###Create Chart
rushplot<-ggplot(rush, aes(avgEPA, pctSuccess))+
  geom_point(color=rush$team_color, 
             cex=rush$playcnt/4)+ 
  geom_text_repel(aes(label=posteam),force=2)+
  labs(x= "Average EPA Per Play", 
       y= "Percent of Success", 
       title="Rush on Late Down and Long", 
       subtitle ="Running plays on 2nd and 3rd down and long", 
       caption="Made by @bschiwal; Data from @nflfastR")+
  geom_hline(yintercept=mean(rush$pctSuccess),color="red",linetype="dashed")+
  geom_vline(xintercept=(mean(rush$avgEPA)),color="red",linetype="dashed")+
  theme_bw()+
  theme(plot.title=element_text(size=14,hjust=.5,face="bold"),
        plot.subtitle = element_text(size=10,hjust=.5))+
  stat_smooth(geom="line",method="lm", alpha=.75)
rushplot


passplot<-ggplot(pass, aes(avgEPA, pctSuccess))+
  geom_point(color=pass$team_color, 
             cex=pass$playcnt/10)+ 
  geom_text_repel(aes(label=posteam),force=2)+
  labs(x= "Average EPA per Play", 
       y= "Percent Success", 
       title="Pass on Late Down and Long", 
       subtitle ="Passing plays on 2nd and 3rd down and long", 
       caption="Made by @bschiwal; Data from @nflfastR")+
  geom_hline(yintercept=mean(pass$pctSuccess),color="red",linetype="dashed")+
  geom_vline(xintercept=(mean(pass$avgEPA)),color="red",linetype="dashed")+
  theme_bw()+
  theme(plot.title=element_text(size=14,hjust=.5,face="bold"),
        plot.subtitle = element_text(size=10,hjust=.5))+
  stat_smooth(geom="line",method="lm", alpha=.75)
passplot
rm(rushplot,passplot)
