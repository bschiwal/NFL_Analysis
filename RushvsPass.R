library(nflfastR)
library(tidyverse)
library(ggplot2)
library(ggrepel)

###Load play by play data
seasons<- 2021
pbp<- nflfastR::load_pbp(seasons)
colr<-teams_colors_logos%>% select(team_abbr,team_color)



### create df rush and df Pass
rush<-pbp%>%
  filter(play_type=="run",qb_scramble==0,play==1)%>%
  select(posteam, rushing_yards,epa,success,play)%>%
  group_by(posteam)%>%
  summarise(avgRushYards=mean(rushing_yards,na.rm=TRUE),RushEPA=mean(epa),RushSuccess=mean(success),Rushcnt=(sum(play)))%>%
  inner_join(colr,by = c("posteam"= "team_abbr"))

offEPA<-  pbp%>%
  filter(play_type=="pass",qb_scramble==0,play==1)%>%
  select(posteam, yards_gained,epa,success,play)%>%
  group_by(posteam)%>%
  summarise(avgPassYards=mean(yards_gained,na.rm=TRUE),PassEPA=mean(epa),PassSuccess=mean(success),Passcnt=(sum(play)))%>%
  inner_join(rush,by = c("posteam"= "posteam"))
  

###Create Chart 
EPAplot<-ggplot(offEPA, aes(y=PassEPA, x=RushEPA))+
  geom_point(color=offEPA$team_color, 
             cex=offEPA$Rushcnt/20)+ 
  geom_text_repel(aes(label=posteam),force=2)+
  labs(x= "Rushing EPA", 
       y= "Passing EPA", 
       title="Average EPA per PLay", 
       subtitle ="Running plays on 2nd and 3rd down and long", 
       caption="Made by @bschiwal | Data from @nflfastR")+
 # geom_hline(yintercept=mean(offEPA$RushEPA),color="red",linetype="dashed")+
  #geom_vline(xintercept=mean(offEPA$PassEPA),color="red",linetype="dashed")+
  geom_hline(yintercept = 0,color="red",linetype="dotted")+
  geom_vline(xintercept = 0,color="red",linetype="dotted")+
  theme_bw()+
  theme(plot.title=element_text(size=14,hjust=.5,face="bold"),
        plot.subtitle = element_text(size=10,hjust=.5))+
  stat_smooth(geom="line",method="lm", alpha=.75)
EPAplot
