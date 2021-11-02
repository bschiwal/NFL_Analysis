

###Build Reciever Dataset
rec<- pbp %>%  
          select(air_yards,
              complete_pass,
              incomplete_pass,
              passing_yards,
              yards_after_catch,
              receiver_player_name,
              touchdown)%>%
  mutate(pass_result = case_when(
    complete_pass==1~"complete",
    complete_pass==0~"incomplete"),
    pass = 1)%>%
  filter(!is.na(receiver_player_name))

###Sumarize reciever variables
recsum <-  rec %>%
  group_by(receiver_player_name)%>%
  summarise(pass=sum(pass),
            complete = sum(complete_pass),
            incomplete = sum(incomplete_pass),
            avg_air_yards = mean(air_yards),
            comp_pct = sum(complete)/ sum(pass),
            passing_yards = sum(passing_yards, na.rm=TRUE),
            yac = sum(yards_after_catch, na.rm=TRUE), 
            TD = sum(touchdown))%>%
  filter(pass>20)%>%
  mutate(TD_per_inc = case_when(TD==0 ~ 0,
                               incomplete==0 ~ 1,
                                TRUE ~ TD / incomplete))

###build chart
reciever_eff <- recsum%>%
    ggplot(aes(yac,TD_per_inc))+
  geom_text_repel(aes(label=receiver_player_name),force=2)
  
reciever_eff  
