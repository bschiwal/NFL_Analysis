###Load Libraries

library(nflfastR)
library(tidyverse)
library(ggplot2)
library(ggrepel)

###Load play by play data
seasons<- 2021
pbp<- nflfastR::load_pbp(seasons)
colr<-teams_colors_logos%>% select(team_abbr,team_color, team_color2,team_logo_espn)

offense<-pbp%>%
  filter(play_type =="run"|play_type=="pass")%>%
  group_by(posteam,game_id,home_team,week)%>%
  summarize(
    pass_yards = sum(passing_yards, na.rm = TRUE),
    rush_yards = sum(rushing_yards, na.rm = TRUE),
    pass_plays = sum(pass_attempt),
    rush_plays = sum(rush_attempt),
    plays = sum(play),
    qb_hit = sum(qb_hit,na.rm= TRUE),
    sack = sum(sack,na.rm=TRUE),
    fumble = sum(fumble, na.rm=TRUE),
    fumble_forced = sum(fumble_forced,na.rm=TRUE),
    fumble_lost = sum(fumble_lost,na.rm=TRUE),
    interception = sum(interception,na.rm=TRUE),
    total = mean(total,na.rm=TRUE),
    result = mean(result,na.rm=TRUE),
    epa_per_play = mean(epa, na.rm=TRUE),
    epa_cum = sum(epa,na.rm=TRUE)
    ) %>%
  mutate(result = if_else(home_team==posteam,mean(result,na.rm=TRUE),-mean(result,na.rm=TRUE)))
  
defense<-pbp%>%
  filter(play_type =="run"|play_type=="pass")%>%
  group_by(defteam,game_id, home_team,week)%>%
  summarize(
    pass_yards = sum(passing_yards, na.rm = TRUE),
    rush_yards = sum(rushing_yards, na.rm = TRUE),
    pass_plays = sum(pass_attempt),
    rush_plays = sum(rush_attempt),
    plays = sum(play),
    qb_hit = sum(qb_hit,na.rm= TRUE),
    sack = sum(sack,na.rm=TRUE),
    fumble = sum(fumble, na.rm=TRUE),
    fumble_forced = sum(fumble_forced,na.rm=TRUE),
    fumble_lost = sum(fumble_lost,na.rm=TRUE),
    interception = sum(interception,na.rm=TRUE),
    total = mean(total,na.rm=TRUE),
    result = mean(result,na.rm=TRUE),
    epa_per_play = mean(epa, na.rm=TRUE),
    epa_cum = sum(epa,na.rm=TRUE)
  )%>%
  mutate(result = if_else(home_team==defteam,mean(result,na.rm=TRUE),-mean(result,na.rm=TRUE)))

teamstats<-full_join(offense,defense,by=c("posteam"="defteam","game_id"="game_id","week"="week"),
                     suffix=c(".offense",".defense"))%>%
  left_join(colr,by = c("posteam"= "team_abbr"))


matchup<-teamstats%>%
  filter(posteam=="MIN")

ggplot(matchup,aes(y=rush_yards.offense/rush_plays.offense, x=rush_yards.offense))+
    geom_point(fill=matchup$team_color, color=matchup$team_color2, pch=21,size=5)

defense_avg <-defense%>%
  group_by(defteam)%>%
  summarise(pass_yd_per_game =mean(pass_yards) ,
            rush_yards_per_game = mean(rush_yards),
            pass_plays_per_game = mean(pass_plays),
            rush_plays_per_game = mean(rush_plays),
            plays_per_game = mean(plays),
            qb_hit_per_game = mean(qb_hit,na.rm= TRUE),
            sack_per_game = mean(sack,na.rm=TRUE),
            fumble_per_game = mean(fumble, na.rm=TRUE),
            fumble_forced_per_game = mean(fumble_forced,na.rm=TRUE),
            fumble_lost_per_game = mean(fumble_lost,na.rm=TRUE),
            interception_per_game = mean(interception,na.rm=TRUE),
            total_per_game = mean(total,na.rm=TRUE),
            result_per_game = mean(result,na.rm=TRUE),
            epa_per_play_per_game = mean(epa_per_play, na.rm=TRUE),
            epa_cum_per_game = mean(epa_cum,na.rm=TRUE))
offense_avg <-offense%>%
  group_by(posteam)%>%
  summarise(pass_yd_per_game =mean(pass_yards) ,
            rush_yards_per_game = mean(rush_yards),
            pass_plays_per_game = mean(pass_plays),
            rush_plays_per_game = mean(rush_plays),
            plays_per_game = mean(plays),
            qb_hit_per_game = mean(qb_hit,na.rm= TRUE),
            sack_per_game = mean(sack,na.rm=TRUE),
            fumble_per_game = mean(fumble, na.rm=TRUE),
            fumble_forced_per_game = mean(fumble_forced,na.rm=TRUE),
            fumble_lost_per_game = mean(fumble_lost,na.rm=TRUE),
            interception_per_game = mean(interception,na.rm=TRUE),
            total_per_game = mean(total,na.rm=TRUE),
            result_per_game = mean(result,na.rm=TRUE),
            epa_per_play_per_game = mean(epa_per_play, na.rm=TRUE),
            epa_cum_per_game = mean(epa_cum,na.rm=TRUE))
teamstats_avg<-full_join(offense_avg,defense_avg,by=c("posteam"="defteam"),
                     suffix=c(".offense",".defense"))%>%
  left_join(colr,by = c("posteam"= "team_abbr"))

ggplot(teamstats_avg,aes(y=pass_yd_per_game.offense, x=rush_yards_per_game.offense))+
  geom_point(fill=teamstats_avg$team_color, pch=21,size=5)
###Goal estimate yards passing, rushing, score, winner, and other stats for each game in a given week.
###create dataset of year to date stats for each team (rush/Pass, center/left/right) for offense and defense
###create a loop to predict matchup values for each matchup in the following week based off offensive vs defensive power
