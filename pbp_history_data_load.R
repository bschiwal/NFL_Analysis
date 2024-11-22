### Load Season Data and save as CSV.
library(dplyr)
library(nflfastR)

### Set Season Data to separate historical PBP from current season data
seasons.hist<- c(2018:2023)
seasons.curr<-2024

###Load current season data
pbp.cur<- nflfastR::load_pbp(seasons.curr)

###Load historic PBP data
pbp.hist<- read.csv("pbp_history.csv")%>%
  filter(season == (seasons.hist))

###convert mismatched hist columns to character
pbp.hist$old_game_id<- as.character(pbp.hist$old_game_id)
pbp.hist$play_clock<- as.character(pbp.hist$play_clock)

pbp.full<-union(pbp.hist,pbp.cur)
###Write history to CSV file
write.csv(pbp.full,"pbp_history.csv", row.names=FALSE)

###Remove Environment
rm(pbp.cur,pbp.hist,seasons.curr,seasons.hist,pbp.full)





