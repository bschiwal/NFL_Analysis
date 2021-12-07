###Load Libraries

library(nflfastR)
library(tidyverse)
library(ggplot2)
library(ggrepel)

###Load play by play data
seasons<- 2021
pbp<- nflfastR::load_pbp(seasons)
colr<-teams_colors_logos%>% select(team_abbr,team_color,team_logo_espn)



### create df rush on 2nd down and long
rush<-pbp%>%
  filter(down==2,ydstogo>=10,play_type=="run",qb_scramble==0, half_seconds_remaining>120,play==1)%>%
  select(posteam, rushing_yards,epa,success,play,series_result,series_success)%>%
  group_by(posteam)%>%
  summarise(avgYards=mean(rushing_yards,na.rm=TRUE),
            avgEPA=mean(epa),
            pctSuccess=mean(success),
            playcnt=(sum(play)),
            pctSeriesSuccess=mean(series_success))%>%
  inner_join(colr,by = c("posteam"= "team_abbr"))

pass<-  pbp%>%
  filter(down==2,ydstogo>=10,play_type=="pass",qb_scramble==0, half_seconds_remaining>120,play==1)%>%
  select(posteam, yards_gained,epa,success,play,series_result,series_success)%>%
  group_by(posteam)%>%
  summarise(avgYards=mean(yards_gained,na.rm=TRUE),
            avgEPA=mean(epa),pctSuccess=mean(success),
            playcnt=(sum(play)),
            pctSeriesSuccess=mean(series_success))%>%
  inner_join(colr,by = c("posteam"= "team_abbr"))
rm(colr,pbp,seasons)

#Join datasets together and add play count columns

rushpass<-pass%>%
  left_join(rush,by=(c("posteam","team_color","team_logo_espn")))%>%
  rename("avgYards.rush"="avgYards.y","avgEPA.rush"="avgEPA.y","pctSuccess.rush"="pctSuccess.y","playcnt.rush"="playcnt.y","pctSeriesSuccess.rush"="pctSeriesSuccess.y",
         "avgYards.pass"="avgYards.x","avgEPA.pass"="avgEPA.x","pctSuccess.pass"="pctSuccess.x","playcnt.pass"="playcnt.x","pctSeriesSuccess.pass"="pctSeriesSuccess.x")%>%
  mutate(totalplays=playcnt.rush+playcnt.pass,
         pctplay.pass = playcnt.pass/totalplays,
         pctplay.rush = playcnt.rush/totalplays
  )
##Make some Charts

successplot<-ggplot(rushpass, aes(y=pctSeriesSuccess.pass, x=pctSeriesSuccess.rush))+
  geom_image(aes(image=team_logo_espn), size = .1)+ 
  # geom_text_repel(aes(label=posteam),force=2)+
  labs(x= "Rushing Success", 
       y= "Passing Success", 
       title="Percent of Series Success After 2nd Down and 10 or More", 
       subtitle ="Success = First Down or Touchdown", 
       caption="Made by @bschiwal; Data from @nflfastR")+
  geom_hline(yintercept=mean(rushpass$pctSeriesSuccess.pass),color="red",linetype="dashed")+
  geom_vline(xintercept=(mean(rushpass$pctSeriesSuccess.rush)),color="red",linetype="dashed")+
  theme_bw()+
  theme(plot.title=element_text(size=14,hjust=.5,face="bold"),
        plot.subtitle = element_text(size=10,hjust=.5))+
  stat_smooth(geom="line",method="lm", alpha=.75)
successplot


successrush<-ggplot(rushpass, aes(y=pctSuccess.rush, x=pctSeriesSuccess.rush))+
  geom_image(aes(image=team_logo_espn), size = .1)+ 
  # geom_text_repel(aes(label=posteam),force=2)+
  labs(x= "Rushing Series Success", 
       y= "2nd Down Rushing Success", 
       title="Percent of Series Success After 2nd Down and 10 or More", 
       subtitle ="Success = First Down or Touchdown", 
       caption="Made by @bschiwal; Data from @nflfastR")+
  geom_hline(yintercept=mean(rushpass$pctSuccess.pass),color="red",linetype="dashed")+
  geom_vline(xintercept=(mean(rushpass$pctSeriesSuccess.rush)),color="red",linetype="dashed")+
  theme_bw()+
  theme(plot.title=element_text(size=14,hjust=.5,face="bold"),
        plot.subtitle = element_text(size=10,hjust=.5))+
  stat_smooth(geom="line",method="lm", alpha=.75)
successrush

