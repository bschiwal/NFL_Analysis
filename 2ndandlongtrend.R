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
  filter(down==2,ydstogo>=10,play_type=="run",qb_scramble==0, half_seconds_remaining>120,play==1)%>%
  select(posteam, rushing_yards,epa,success,play)%>%
  group_by(posteam)%>%
  summarise(avgYards=mean(rushing_yards,na.rm=TRUE),avgEPA=mean(epa),pctSuccess=mean(success),playcnt=(sum(play)))%>%
  inner_join(colr,by = c("posteam"= "team_abbr"))

pass<-  pbp%>%
  filter(down==2,ydstogo>=10,play_type=="pass",qb_scramble==0, half_seconds_remaining>120,play==1)%>%
  select(posteam, yards_gained,epa,success,play)%>%
  group_by(posteam)%>%
  summarise(avgYards=mean(yards_gained,na.rm=TRUE),avgEPA=mean(epa),pctSuccess=mean(success),playcnt=(sum(play)))%>%
  inner_join(colr,by = c("posteam"= "team_abbr"))
rm(colr,pbp,seasons)

rushpass<-pass%>%
  left_join(rush,by=(c("posteam","team_color")))%>%
  rename("avgYards.rush"="avgYards.y","avgEPA.rush"="avgEPA.y","pctSuccess.rush"="pctSuccess.y","playcnt.rush"="playcnt.y",
         "avgYards.pass"="avgYards.x","avgEPA.pass"="avgEPA.x","pctSuccess.pass"="pctSuccess.x","playcnt.pass"="playcnt.x")%>%
  mutate(totalplays=playcnt.rush+playcnt.pass,
         pctplay.pass = playcnt.pass/totalplays,
         pctplay.rush = playcnt.rush/totalplays
         )

###Create Chart
rushplot<-ggplot(rush, aes(playcnt, pctSuccess))+
  geom_point(color=rush$team_color, 
             cex=rush$playcnt/2)+ 
  geom_text_repel(aes(label=posteam),force=2)+
  labs(x= "Number of Plays", 
       y= "Percent of Success", 
       title="Rush on 2nd Down and Long", 
       subtitle ="10 yards or more to go", 
       caption="Made by @bschiwal; Data from @nflfastR")+
  geom_hline(yintercept=mean(rush$pctSuccess),color="red",linetype="dashed")+
  geom_vline(xintercept=(mean(rush$playcnt)),color="red",linetype="dashed")+
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

rushplot<-ggplot(rushpass, aes(pctplay.rush, pctSuccess.rush))+
  geom_point(color=rushpass$team_color, 
             cex=4)+ 
  geom_text_repel(aes(label=posteam),force=2)+
  labs(x= "Percent of Plays", 
       y= "Percent of Success", 
       title="Rush on 2nd Down and Long", 
       subtitle ="10 yards or more to go", 
       caption="Made by @bschiwal; Data from @nflfastR")+
  geom_hline(yintercept=mean(rushpass$pctSuccess.rush),color="red",linetype="dashed")+
  geom_vline(xintercept=(mean(rushpass$pctplay.rush)),color="red",linetype="dashed")+
  theme_bw()+
  theme(plot.title=element_text(size=14,hjust=.5,face="bold"),
        plot.subtitle = element_text(size=10,hjust=.5))+
  stat_smooth(geom="line",method="lm", alpha=.75)
rushplot

passplot<-ggplot(rushpass, aes(pctplay.pass, pctSuccess.pass))+
  geom_point(color=rushpass$team_color,
            cex=4)+ 
  geom_text_repel(aes(label=posteam),force=2)+
  labs(x= "Percent of Plays", 
       y= "Percent of Success", 
       title="Pass on 2nd Down and Long", 
       subtitle ="10 yards or more to go", 
       caption="Made by @bschiwal; Data from @nflfastR")+
  geom_hline(yintercept=mean(rushpass$pctSuccess.pass),color="red",linetype="dashed")+
  geom_vline(xintercept=(mean(rushpass$pctplay.pass)),color="red",linetype="dashed")+
  theme_bw()+
  theme(plot.title=element_text(size=14,hjust=.5,face="bold"),
        plot.subtitle = element_text(size=10,hjust=.5))+
  stat_smooth(geom="line",method="lm", alpha=.75)
passplot

effectiveplot<-ggplot(rushpass, aes(pctplay.pass*pctSuccess.pass, pctplay.rush*pctSuccess.rush))+
  geom_point(color=rushpass$team_color,
             cex=4)+ 
  geom_text_repel(aes(label=posteam),force=2)+
  labs(x= "Pass Effectiveness", 
       y= "Rush Effectiveness", 
       title="Effectiveness on 2nd Down and Long", 
       subtitle ="10 yards or more to go", 
       caption="Made by @bschiwal; Data from @nflfastR")+
  geom_hline(yintercept=mean(rushpass$pctSuccess.rush*rushpass$pctplay.rush),color="red",linetype="dashed")+
  geom_vline(xintercept=(mean(rushpass$pctplay.pass*rushpass$pctSuccess.pass)),color="red",linetype="dashed")+
  theme_bw()+
  theme(plot.title=element_text(size=14,hjust=.5,face="bold"),
        plot.subtitle = element_text(size=10,hjust=.5))+
  stat_smooth(geom="line",method="lm", alpha=.75)
effectiveplot

successplot<-ggplot(rushpass, aes(pctSuccess.rush, pctSuccess.pass))+
  geom_point(color=rushpass$team_color, 
             cex=rushpass$totalplays/10)+ 
  geom_text_repel(aes(label=posteam),force=2)+
  labs(x= "Rushing Success", 
       y= "Passing Success", 
       title="Pass on 2nd Down and Long", 
       subtitle ="10 yards or more to go", 
       caption="Made by @bschiwal; Data from @nflfastR")+
  geom_hline(yintercept=mean(rushpass$pctSuccess.pass),color="red",linetype="dashed")+
  geom_vline(xintercept=(mean(rushpass$pctSuccess.rush)),color="red",linetype="dashed")+
  theme_bw()+
  theme(plot.title=element_text(size=14,hjust=.5,face="bold"),
        plot.subtitle = element_text(size=10,hjust=.5))+
  stat_smooth(geom="line",method="lm", alpha=.75)
successplot

tendancyplot<-ggplot(rushpass, aes(pctplay.rush, pctplay.pass))+
  geom_point(color=rushpass$team_color, 
             cex=rushpass$totalplays/10)+ 
  geom_text_repel(aes(label=posteam),force=2)+
  labs(x= "Rushing Plays", 
       y= "Passing Plays", 
       title="Play Tendancy on 2nd Down and Long", 
       subtitle ="10 yards or more to go", 
       caption="Made by @bschiwal; Data from @nflfastR")+
  geom_hline(yintercept=mean(rushpass$pctplay.pass),color="red",linetype="dashed")+
  geom_vline(xintercept=(mean(rushpass$pctplay.rush)),color="red",linetype="dashed")+
  theme_bw()+
  theme(plot.title=element_text(size=14,hjust=.5,face="bold"),
        plot.subtitle = element_text(size=10,hjust=.5))+
  stat_smooth(geom="line",method="lm", alpha=.75)
tendancyplot

rm(rushplot,passplot,rushpass,tendancyplot,successplot,pass,rush, effectiveplot)
