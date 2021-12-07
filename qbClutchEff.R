###Load Libraries

library(nflfastR)
library(tidyverse)
library(ggplot2)
library(ggrepel)

###Load play by play data
seasons<- 2021
pbp<- nflfastR::load_pbp(seasons)
colr<-teams_colors_logos%>% select(team_abbr,team_color)

##Filter to just dropback pass plays in final two minutes when down in one score game
passtrailingend <-pbp%>% filter(qb_dropback==1,
                                play_type=="pass",
                                score_differential<=0,
                                score_differential>=-8,
                                half_seconds_remaining<=120 | game_half=="Overtime")

###Filter and summarize dataset by passer 
passEPAtrailend <- passtrailingend%>% 
  select(posteam,posteam,passer_player_name,qb_epa,pass_attempt,cpoe)%>%
  group_by(passer_player_name,posteam)%>%
  summarize(qb_epa_per_play=mean(qb_epa, na.rm = TRUE),attempts=sum(pass_attempt),cpoe=mean(cpoe,na.rm=TRUE))%>%
  filter(attempts>=15)%>%
  inner_join(colr,by = c("posteam"= "team_abbr"))
rm(colr,passtrailingend,pbp,seasons)
###Create Chart
qbclutch<-ggplot(passEPAtrailend, aes(cpoe, qb_epa_per_play))+
  geom_point(color=passEPAtrailend$team_color, 
             cex=passEPAtrailend$attempts/15)+ 
  geom_text_repel(aes(label=passer_player_name),force=2)+
  labs(x= "Completion % Over Expectation (CPOE)", 
       y= "QB EPA per Pass Thrown", 
       title="Quarterback Clutch Efficency 2021", 
       subtitle ="Dropback Passes in Final Two Minutes of Half or Overtime When Trailing by < 8 Points; Min 15 Attempt", 
       caption="Made by @bschiwal; Data from @nflfastR")+
  geom_hline(yintercept=0,color="red",linetype="dashed")+
  geom_vline(xintercept=0,color="red",linetype="dashed")+
  theme_bw()+
  theme(plot.title=element_text(size=14,hjust=.5,face="bold"),
        plot.subtitle = element_text(size=8,hjust=.5))+
  stat_smooth(geom="line",method="lm", alpha=.75)
qbclutch

###Save image
dev.copy(png,"qbClutchEff.png")
##qbclutch
dev.off()
rm(qbclutch,passEPAtrailend)

