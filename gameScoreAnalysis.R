require(nflfastR)
require(nflreadr)
require(tidyverse)
require(ggplot2)
require(ggrepel)
require(ggimage)
require(scales)
require(dplyr)

###Load Game data
seasons<- 2024
game<- nflreadr::load_schedules(seasons)
colr<-teams_colors_logos%>% select(team_abbr,team_color,team_logo_espn)

###Build Team Record Dataset
#load games
gmplayed<- game%>%
  select(game_id,game_type,home_team,away_team,home_score,away_score,result)%>%
  filter(!is.na(away_score))%>%
  rename("home_result"="result")

#distinct team list
teams<-game%>%distinct(home_team)

#create all team home games
home<- teams%>%
    left_join(gmplayed, by = "home_team")%>%
    mutate(game = 1,
           home_game = 1,
          home_win = ifelse(home_result>0,1,0),
          home_loss = ifelse(home_result<0,1,0))%>%
    rename("team"="home_team", "oppteam"= "away_team")

#create all team away games
teams<-rename(teams,"away_team"="home_team")
away<-teams%>%
  left_join(gmplayed, by = "away_team")%>%
  mutate(game = 1,
         away_game = 1,
         away_win = ifelse(home_result<0,1,0),
         away_loss = ifelse(home_result>0,1,0))%>%
  rename("team"="away_team", "oppteam"="home_team")

#create all team game results
result<-home%>%
    full_join(away, by=c("team","oppteam","game_id","game_type","home_score","away_score","home_result","game"))%>%
    select(team,oppteam,game,home_game,away_game,home_win,away_win,home_loss,away_loss)

rm(home,away,teams,game)

#Create  team total records 
#replace NA with 0s
result[is.na(result)] <- 0
result<-result%>%
    mutate(win=home_win+away_win,
           loss = home_loss+away_loss)

#Calculate records in summary df
record<- result%>%
  select(team,game,home_game,away_game,home_win,away_win,home_loss,away_loss)%>%
  mutate(win = home_win+away_win, 
         loss = home_loss+away_loss)%>%
  group_by(team)%>%
  summarise(games = sum(game),
            wins = sum(win, na.rm=TRUE),
            losses = sum(loss, na.rm = TRUE),
            home_games = sum(home_game),
            home_wins = sum(home_win),
            home_losses = sum(home_loss),
            away_games = sum(away_game),
            away_wins = sum(away_win),
            away_losses = sum(away_loss)
            )%>%
  mutate(record = round(wins/games,3),
         record_home = round(home_wins/home_games,3),
         record_away = round(away_wins/away_games,3)
         )

#Calculate ending opponent records for each game  
opprecord<- result%>%
  select(team,oppteam, win, loss)%>%
  left_join(record, by=c("oppteam"="team"))%>%
  select(team,oppteam,wins,win,loss,losses,games)%>%
  group_by(team)%>%
  summarise(opp_games = sum(games),
            opp_wins = sum(wins),
            opp_losses = sum(losses),
            opp_win_wins = sum(wins[win==1]),
            opp_win_losses = sum(losses[win==1]),
            opp_loss_wins = sum(wins[loss==1]),
            opp_loss_losses = sum(losses[loss==1])
            )%>%
  mutate(opp_record = opp_wins/opp_games,
         opp_record_wins = opp_win_wins/(opp_win_wins+opp_win_losses),
         opp_record_losses = opp_loss_wins/(opp_loss_wins+opp_loss_losses))

cumrecopp<-result%>%
    select(team,oppteam,win,loss)%>%
    left_join(record,by=c("oppteam"="team"))%>%
    group_by(team)%>%
    summarise(opp_cum_rec = sum(record),
      opp_cum_rec_home = sum(record_home),
      opp_cum_rec_win = sum(record[win==1]),
      opp_cum_rec_lose = sum(record[loss==1]),
      opp_cum_rec_away = sum(record_away)
    )
##Record Vs Winning Teams vs losing teams(.500 with losing)

recvswin<- result%>%
  select(team,oppteam, win, loss,game)%>%
  left_join(record, by=c("oppteam"="team"))%>%
  mutate(opp_win_record = ifelse(record>.5,1,0),opp_not_win_record = ifelse(record<=.5,1,0))%>%
  select(team,oppteam,game,wins,win,loss,losses,games,opp_win_record,opp_not_win_record)%>%
  group_by(team)%>%
  summarise(games_vs_winning_team = sum(game[opp_win_record==1]),
            wins_vs_winning_team = sum(win[opp_win_record==1]),
            losses_vs_winning_team = sum(loss[opp_win_record==1]),
            games_vs_not_winning_team = sum(game[opp_not_win_record==1]),
            wins_vs_not_winning_team = sum(win[opp_not_win_record==1]),
            losses_vs_not_winning_team = sum(loss[opp_not_win_record==1])
  )%>%
  mutate(record_vs_win_record = wins_vs_winning_team/games_vs_winning_team,
         record_vs_not_win_record = wins_vs_not_winning_team/games_vs_not_winning_team)
  
  
#Join opponent records to team records and visualization fields
teamrecords<-record%>%
  left_join(recvswin, by="team")%>%
  left_join(opprecord,by = "team")%>%
  left_join(cumrecopp, by = "team")%>%
  left_join(colr, by = c("team"="team_abbr"))

rm(gmplayed,opprecord,record,result,colr,recvswin,cumrecopp)

#Plot a few charts
##Strength Adjusted Record

ggplot(teamrecords, aes((opp_cum_rec_lose/opp_cum_rec)*(1-record),(opp_cum_rec_win/opp_cum_rec)*record))+
  geom_image(aes(image=team_logo_espn), size = .1)+
  labs(title="Strength Adjusted Wins and Losses",
       subtitle = "2024 Season",
       x = "Losing Percentage",
       y = "Winning Percentage",
       caption = "Wins and Losses Adjusted by The Cumulative Record of Opponents in Wins and Losses
                  Made by @bschiwal | source @nflfastR"
  )+  theme(plot.title=element_text(hjust=.5,family="serif",face="bold", size=14),
            plot.caption = element_text(family="serif", size=8,colour="black"),
            plot.subtitle=element_text(hjust=.5,family="serif", size=12))+
  scale_y_continuous(breaks=seq(0,1,.250),labels = label_number(accuracy=.001))+
  scale_x_reverse(breaks=seq(0,1,.250),labels=label_number(accuracy=.001))
###Save image
dev.copy(png,"RecordStrengthAdj.png", width = 600, height = 600)
dev.off()  

##Strength Adjusted Wins
ggplot(teamrecords, aes(losses_vs_not_winning_team*(1-record),wins_vs_winning_team*record))+
  geom_image(aes(image=team_logo_espn), size = .1)+
  labs(title="Strength Adjusted Wins and Losses",
       subtitle= "2024 Season",
       x = "Weak Losses",
       y = "Strong Wins",
       caption = "Strong Wins = (Team Record * Wins vs Teams With Winning Record) | Weak Losses = ((1-Team Record) * Losses vs Teams Without Winning Record)
                  Made by @bschiwal | source @nflfastR"
       )+
  theme(plot.title=element_text(hjust=.5,family="serif",face="bold", size=14),
        plot.caption = element_text(family="serif", size=8,colour="black"),
        plot.subtitle=element_text(hjust=.5,family="serif", size=12))+
  scale_x_reverse()

dev.copy(png,"WinsStrengthAdj.png", width = 600, height = 600)
dev.off() 

rm(teamrecords)
 