### This project intends to determine if the vikings (or any team) has a run:pass percentage 
### that is effective when compared to the rest if the league.  

###Load Libraries

library(nflfastR)
library(tidyverse)
library(ggplot2)
library(ggrepel)

###Load play by play data
seasons<- 2021
pbp<- nflfastR::load_pbp(seasons)
colr<-teams_colors_logos%>% select(team_abbr,team_color)

### Create list of Plays
plays<- pbp %>%
  filter(play==1,play_type=="run" | play_type=="pass")%>%
  mutate(td_differential=ceiling(score_differential/8))

##Join Team Color Data
plays<-plays%>%
  select(posteam,play_type,play,pass,rush, epa, posteam_score,defteam_score,td_differential)%>%
  inner_join(colr,by = c("posteam"= "team_abbr"))
         
teamdata<-plays%>%
  group_by(posteam,td_differential,team_color)%>%
  summarize(avgrush = (mean(rush)),
            avgpass = mean(pass),
            avgepa = mean(epa),
            play_count = sum(play))
rm(colr,seasons,pbp)

### get Minnesota plays
mn<-teamdata%>%
  filter(posteam=="MIN",
         play_count>10,
         td_differential<=3,
         td_differential>=(-3))%>%
  select(!play_count)


###Top EPA Teams
topepa<-plays%>%
  group_by(posteam)%>%
  summarize(avgrush = mean(rush),
            avgpass = mean(pass),
            avgepa = mean(epa))
topepa<- arrange(topepa,desc(avgepa))%>%
  filter(row_number()<=10 | posteam=="MIN")%>%
  select(posteam)

###Average of top 10 EPA teams
leaguetopavg<-plays%>%
  right_join(topepa, by = c("posteam"="posteam"))%>%
  filter(posteam!="MIN")%>%
  group_by(td_differential)%>%
  summarize(avgrush = (mean(rush)),
            avgpass = mean(pass),
            avgepa = mean(epa),
            posteam = "AVG",
            team_color = "black",
            play_count = sum(play))%>%
  filter(play_count>10,
         td_differential<=3,
         td_differential>=(-3))%>%
  select(posteam,td_differential,team_color,avgrush,avgpass,avgepa)%>%
  union(mn)



ggplot(leaguetopavg,aes(y=avgrush,x=td_differential))+
  labs(x= "Touchdown Differential",
       y = "Rushing Percentage",
       caption = "Made by @BSchiwal | Data from @NFLfastR",
       subtitle = "Playcount >10 | TD Differential Based on 8 Points",
       title = "Minnesota Rushing Play Calls vs Top 10 EPA Offenses(avg)")+
  scale_x_continuous(breaks =c(-4,-3,-2,-1,0,1,2,3,4))+
  scale_y_continuous(labels = scales::percent)+
  geom_point(data = subset(leaguetopavg,posteam=="MIN"), aes(color="#4f2683"))+
  geom_smooth(data= subset(leaguetopavg,posteam!="MIN"),method ="lm", aes(color="blue"))+
  scale_color_identity(name = "Play Group",
                       breaks = c("#4f2683", "blue"),
                       labels = c("MIN", "Top 10"),
                       guide = "legend")+
  theme(plot.title=element_text(size=14,hjust=.5,face="bold"),
        plot.subtitle = element_text(size=10,hjust=.5))
###Save image
dev.copy(png,"MNRushvstopoff.png")
dev.off()

rm(plays,mn,teamdata,topepa,leaguetopavg)  
  
