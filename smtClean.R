#### File Retrieval ####

setwd("~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/ball_pos")
file_list = list.files(path = "~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/ball_pos")
ballpos = data.frame()

for (i in 1:length(file_list)) {
  ballpos = rbind(ballpos, read.csv(file_list[i]))
}


setwd("~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/game_events")
file_list = list.files(path = "~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/game_events")
game_events = data.frame()

for (i in 1: length(file_list)) {
  game_events = rbind(game_events, read.csv(file_list[i]))
}


setwd("~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/game_info")
file_list = list.files(path = "~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/game_info")
game_info = data.frame()

for (i in 1: length(file_list)) {
  game_info = rbind(game_info, read.csv(file_list[i]))
}






setwd("~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/player_pos/TeamA1/player_pos-1902_TeamA1")
file_list = list.files(path = "~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/player_pos/TeamA1/player_pos-1902_TeamA1")
player_pos = data.frame()
for (i in 1: length(file_list)) {
 player_pos = rbind(player_pos, read.csv(file_list[i]))
}


setwd("~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/player_pos/TeamA1/player_pos-1903_TeamA1")
file_list = list.files(path = "~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/player_pos/TeamA1/player_pos-1903_TeamA1")
for (i in 1: length(file_list)) {
  player_pos = rbind(player_pos, read.csv(file_list[i]))
}


setwd("~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/player_pos/TeamA2/player_pos-1902_TeamA2")
file_list = list.files(path = "~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/player_pos/TeamA2/player_pos-1902_TeamA2")
for (i in 1: length(file_list)) {
  player_pos = rbind(player_pos, read.csv(file_list[i]))
}

setwd("~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/player_pos/TeamA2/player_pos-1903_TeamA2")
file_list = list.files(path = "~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/player_pos/TeamA2/player_pos-1903_TeamA2")
for (i in 1: length(file_list)) {
  player_pos = rbind(player_pos, read.csv(file_list[i]))
}


setwd("~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/player_pos/TeamA3/player_pos-1901_TeamA3")
file_list = list.files(path = "~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/player_pos/TeamA3/player_pos-1901_TeamA3")
for (i in 1: length(file_list)) {
  player_pos = rbind(player_pos, read.csv(file_list[i]))
}


setwd("~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/player_pos/TeamA3/player_pos-1902_TeamA3")
file_list = list.files(path = "~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/player_pos/TeamA3/player_pos-1902_TeamA3")
for (i in 1: length(file_list)) {
  player_pos = rbind(player_pos, read.csv(file_list[i]))
}


setwd("~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/player_pos/TeamA3/player_pos-1903_TeamA3")
file_list = list.files(path = "~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/player_pos/TeamA3/player_pos-1903_TeamA3")
for (i in 1: length(file_list)) {
  player_pos = rbind(player_pos, read.csv(file_list[i]))
}


setwd("~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/player_pos/TeamB/player_pos-1900_TeamB")
file_list = list.files(path = "~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/player_pos/TeamB/player_pos-1900_TeamB")
for (i in 1: length(file_list)) {
  player_pos = rbind(player_pos, read.csv(file_list[i]))
}


setwd("~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/player_pos/TeamB/player_pos-1901_TeamB")
file_list = list.files(path = "~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/player_pos/TeamB/player_pos-1901_TeamB")
for (i in 1: length(file_list)) {
  player_pos = rbind(player_pos, read.csv(file_list[i]))
}


setwd("~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/player_pos/TeamB/player_pos-1902_TeamB")
file_list = list.files(path = "~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/player_pos/TeamB/player_pos-1902_TeamB")
for (i in 1: length(file_list)) {
  player_pos = rbind(player_pos, read.csv(file_list[i]))
}



setwd("~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/player_pos/TeamB/player_pos-1903_TeamB")
file_list = list.files(path = "~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023/player_pos/TeamB/player_pos-1903_TeamB")
for (i in 1: length(file_list)) {
  player_pos = rbind(player_pos, read.csv(file_list[i]))
}


setwd("~/Baseball Research/SMT 2023/SMT-Data-Challenge/smt_data_challenge_2023")
team_info = read.csv("team_info.csv")





#### Data Cleaning and Manipulation ####

library(dplyr)

# position and event label adjusting

game_events = game_events %>%
  rename(positionEvent = player_position) %>%
  mutate(positionEvent = ifelse(positionEvent == 1, "P", positionEvent),
         positionEvent = ifelse(positionEvent == 2, "C", positionEvent),
         positionEvent = ifelse(positionEvent == 3, "1B", positionEvent),
         positionEvent = ifelse(positionEvent == 4, "2B", positionEvent),
         positionEvent = ifelse(positionEvent == 5, "3B", positionEvent),
         positionEvent = ifelse(positionEvent == 6, "SS", positionEvent),
         positionEvent = ifelse(positionEvent == 7, "LF", positionEvent),
         positionEvent = ifelse(positionEvent == 8, "CF", positionEvent),
         positionEvent = ifelse(positionEvent == 9, "RF", positionEvent),
         positionEvent = ifelse(positionEvent == 10, "BTR", positionEvent),
         positionEvent = ifelse(positionEvent == 11, "r1B", positionEvent),
         positionEvent = ifelse(positionEvent == 12, "r2B", positionEvent),
         positionEvent = ifelse(positionEvent == 13, "r3B", positionEvent),
         positionEvent = ifelse(positionEvent == 255, "event no player", positionEvent),
         event_code = ifelse(event_code == 1, "pitch", event_code),
         event_code = ifelse(event_code == 2, "ball acquired", event_code),
         event_code = ifelse(event_code == 3, "throw (ball-in-play)", event_code),
         event_code = ifelse(event_code == 4, "ball hit into play", event_code),
         event_code = ifelse(event_code == 5, "end of play", event_code),
         event_code = ifelse(event_code == 6, "pickoff throw", event_code),
         event_code = ifelse(event_code == 7, "ball acquired - unknown field position", event_code),
         event_code = ifelse(event_code == 8, "throw (ball-in-play) - unknown field position", event_code),
         event_code = ifelse(event_code == 9, "ball deflection", event_code),
         event_code = ifelse(event_code == 10, "ball deflection off of wall", event_code),
         event_code = ifelse(event_code == 11, "home run", event_code),
         event_code = ifelse(event_code == 16, "ball bounce", event_code))


player_pos = player_pos %>%
  rename(positionEvent = player_position) %>%
  mutate(positionEvent = ifelse(positionEvent == 1, "P", positionEvent),
         positionEvent = ifelse(positionEvent == 2, "C", positionEvent),
         positionEvent = ifelse(positionEvent == 3, "1B", positionEvent),
         positionEvent = ifelse(positionEvent == 4, "2B", positionEvent),
         positionEvent = ifelse(positionEvent == 5, "3B", positionEvent),
         positionEvent = ifelse(positionEvent == 6, "SS", positionEvent),
         positionEvent = ifelse(positionEvent == 7, "LF", positionEvent),
         positionEvent = ifelse(positionEvent == 8, "CF", positionEvent),
         positionEvent = ifelse(positionEvent == 9, "RF", positionEvent),
         positionEvent = ifelse(positionEvent == 10, "BTR", positionEvent),
         positionEvent = ifelse(positionEvent == 11, "r1B", positionEvent),
         positionEvent = ifelse(positionEvent == 12, "r2B", positionEvent),
         positionEvent = ifelse(positionEvent == 13, "r3B", positionEvent),
         positionEvent = ifelse(positionEvent == 255, "event no player", positionEvent)) %>%
  rename(player_position = positionEvent)






# joining data

pos = right_join(ballpos, player_pos, by = c("game_str", "timestamp", "play_id"))
game = left_join(game_events, game_info, by = c("game_str", "play_per_game"))
full = left_join(pos, game, by = c("game_str", "play_id", "timestamp"))

full = full %>%
  select(-X.x.x, -X.x.y, -X.y.x, -X.y.y)


full %>%
  filter(!is.na(first_baserunner))


# more joining
pos_event = inner_join(pos, game_events, by = c("game_str", "timestamp"))
pos_info = inner_join(pos_event, game_info, by = c("game_str", "play_per_game"))







# working to figure out stolen bases and attempts

g1 = game %>%
  select(game_str, play_id, positionEvent, first_baserunner, second_baserunner, third_baserunner, event_code) %>%
  mutate(lead1 = lead(event_code),
         lead2 = lead(event_code, 2),
         lead3 = lead(event_code, 3),
         lead4 = lead(event_code, 4),
         lead5 = lead(event_code, 5),
         lead6 = lead(event_code, 6),
         lead7 = lead(event_code, 7),
         lead8 = lead(event_code, 8),
         lead9 = lead(event_code, 9),
         pos2 = lead(positionEvent, 2),
         pos3 = lead(positionEvent, 3),
         pos4 = lead(positionEvent, 4),
         pos5 = lead(positionEvent, 5),
         pos6 = lead(positionEvent, 6),
         pos7 = lead(positionEvent, 7),
         pos8 = lead(positionEvent, 8),
         pos9 = lead(positionEvent, 9),
         nextr1B = ifelse(lead3 == "end of play", lead(first_baserunner, 5), NA),
         nextr1B = ifelse(lead4 == "end of play", lead(first_baserunner, 6), nextr1B),
         nextr1B = ifelse(lead5 == "end of play", lead(first_baserunner, 7), nextr1B),
         nextr1B = ifelse(lead6 == "end of play", lead(first_baserunner, 8), nextr1B),
         nextr1B = ifelse(lead7 == "end of play", lead(first_baserunner, 9), nextr1B),
         nextr1B = ifelse(lead8 == "end of play", lead(first_baserunner, 10), nextr1B),
         nextr1B = ifelse(lead9 == "end of play", lead(first_baserunner, 11), nextr1B),
         nextr2B = ifelse(lead3 == "end of play", lead(second_baserunner, 5), NA),
         nextr2B = ifelse(lead4 == "end of play", lead(second_baserunner, 6), nextr2B),
         nextr2B = ifelse(lead5 == "end of play", lead(second_baserunner, 7), nextr2B),
         nextr2B = ifelse(lead6 == "end of play", lead(second_baserunner, 8), nextr2B),
         nextr2B = ifelse(lead7 == "end of play", lead(second_baserunner, 9), nextr2B),
         nextr2B = ifelse(lead8 == "end of play", lead(second_baserunner, 10), nextr2B),
         nextr2B = ifelse(lead9 == "end of play", lead(second_baserunner, 11), nextr2B),
         nextr3B = ifelse(lead3 == "end of play", lead(third_baserunner, 5), NA),
         nextr3B = ifelse(lead4 == "end of play", lead(third_baserunner, 6), nextr3B),
         nextr3B = ifelse(lead5 == "end of play", lead(third_baserunner, 7), nextr3B),
         nextr3B = ifelse(lead6 == "end of play", lead(third_baserunner, 8), nextr3B),
         nextr3B = ifelse(lead7 == "end of play", lead(third_baserunner, 9), nextr3B),
         nextr3B = ifelse(lead8 == "end of play", lead(third_baserunner, 10), nextr3B),
         nextr3B = ifelse(lead9 == "end of play", lead(third_baserunner, 11), nextr3B)) %>%
  filter(event_code == "pitch",
         first_baserunner > 0,
         second_baserunner == 0,
         !is.na(nextr2B),
         lead1 %in% c("ball bounce", "ball acquired", "ball deflection", "ball deflection off wall",
                      "ball acquired - unknown field position"),
         lead2 != "end of play", 
         lead2 !=  "home run", 
         lead2 != "ball hit into play") %>%
  mutate(SB = ifelse(first_baserunner == nextr2B | first_baserunner == nextr3B, 1, 0))

# different sequences for stolen base attempts

g2 = g1 %>%
  filter(lead1 == "ball acquired",
         lead2 == "throw (ball-in-play)",
         lead3 == "ball acquired") %>%
  mutate(SB = ifelse(first_baserunner == nextr2B | first_baserunner == nextr3B, 1, 0))
  
g3 = g1 %>%
  filter(lead1 == "ball bounce",
         lead2 == "ball acquired",
         lead3 == "throw (ball-in-play)",
         lead4 == "ball acquired") %>%
  mutate(SB = ifelse(first_baserunner == nextr2B | first_baserunner == nextr3B, 1, 0))

g4 = g1 %>%
  filter(lead1 == "ball acquired",
         lead2 == "throw (ball-in-play)",
         lead3 == "ball bounce") %>%
  mutate(SB = ifelse(first_baserunner == nextr2B | first_baserunner == nextr3B, 1, 0))

g5 = g1 %>%
  filter(lead1 == "ball bounce",
         lead2 == "ball acquired",
         lead3 == "throw (ball-in-play)",
         lead4 == "ball bounce") %>%
  mutate(SB = ifelse(first_baserunner == nextr2B | first_baserunner == nextr3B, 1, 0))

g6 = g1 %>%
  filter(lead1 == "ball deflection",
         lead2 == "ball acquired",
         lead3 == "throw (ball-in-play)",
         lead4 == "ball acquired") %>%
  mutate(SB = ifelse(first_baserunner == nextr2B | first_baserunner == nextr3B, 1, 0))

g7 = g1 %>%
  filter(lead1 == "ball acquired",
         lead2 == "throw (ball-in-play)",
         lead3 == "ball deflection") %>%
  mutate(SB = ifelse(first_baserunner == nextr2B | first_baserunner == nextr3B, 1, 0))

g8 = g1 %>%
  filter(lead1 == "ball acquired",
         lead2 == "throw (ball-in-play)",
         lead3 %in% c("ball deflection", "ball bounce"),
         lead4 == "ball acquired") %>%
  mutate(SB = ifelse(first_baserunner == nextr2B | first_baserunner == nextr3B, 1, 0))


g9 = g1 %>%
  filter(lead1 == "ball bounce",
         lead2 == "ball deflection") %>%
  mutate(SB = ifelse(first_baserunner == nextr2B | first_baserunner == nextr3B, 1, 0))


g10 = g1 %>%
  filter(lead1 == "ball bounce",
         lead4 == "ball bounce") %>%
  mutate(SB = ifelse(first_baserunner == nextr2B | first_baserunner == nextr3B, 1, 0))



g = rbind(g2, g3, g4, g5, g6, g7, g8, g9, g10) %>%
  filter(first_baserunner != nextr1B)



new2 = inner_join(pos_info, g, by = c("game_str", "play_id.x" = "play_id"))%>%
  rename(play_id = play_id.x)

new2 = unique(new2)



# adding ball and player position to each row

ballpos = new2 %>%
  group_by(game_str, play_id, timestamp) %>%
  summarise(ball_x = mean(ball_position_x),
            ball_y = mean(ball_position_y),
            ball_z = mean(ball_position_z)) %>%
  filter(!is.na(ball_x)) 

posC = new2 %>%
  filter(player_position == "C") %>%
  group_by(game_str, play_id, timestamp) %>%
  summarise(cat_x = mean(field_x),
            cat_y = mean(field_y)) %>%
  filter(!is.na(cat_x))

posSS = new2 %>%
  filter(player_position == "SS") %>%
  group_by(game_str, play_id, timestamp) %>%
  summarise(fSS_x = mean(field_x),
            fSS_y = mean(field_y)) %>%
  filter(!is.na(fSS_x))

pos2B = new2 %>%
  filter(player_position == "2B") %>%
  group_by(game_str, play_id, timestamp) %>%
  summarise(f2B_x = mean(field_x),
            f2B_y = mean(field_y)) %>%
  filter(!is.na(f2B_x))

posr1B = new2 %>%
  filter(player_position == "r1B") %>%
  group_by(game_str, play_id, timestamp) %>%
  summarise(r1B_x = mean(field_x),
            r1B_y = mean(field_y)) %>%
  filter(!is.na(r1B_x))

posP = new2 %>%
  filter(player_position == "P") %>%
  group_by(game_str, play_id, timestamp) %>%
  summarise(P_x = mean(field_x),
            P_y = mean(field_y)) %>%
  filter(!is.na(P_x))


posSB = inner_join(pos2B, posC)
posSB = inner_join(posSB, posSS)
posSB = inner_join(posSB, posr1B)

posSB = inner_join(posSB, ballpos)



new3 = new2 %>%
  select(game_str, play_id, timestamp, event_code.x, positionEvent.x, SB)


# aggregation for data and final cleaning

new4 = unique(inner_join(posSB, new3)) %>%
  arrange(game_str, play_id, timestamp) %>%
  group_by(game_str, play_id) %>%
  mutate(start_time = min(timestamp)) %>%
  ungroup() %>%
  mutate(timediff = timestamp - start_time,
         rDist2B = sqrt((0 - r1B_x)^2 + (127.3 - r1B_y)^2),
         rDist3B = sqrt((-63.64 - r1B_x)^2 + (63.64 - r1B_y)^2),
         ballDist2B = sqrt((0 - ball_x)^2 + (127.3 - ball_y)^2 + (0 - ball_z)^2)) %>%
  group_by(game_str, play_id) %>%
  mutate(min3B = min(rDist3B)) %>%
  ungroup() %>%
  mutate(SB = ifelse(min3B <= 85, 1, SB)) %>%
  filter(rDist3B >= 85,
         event_code.x != "end of play") %>%
  select(game_str, play_id, timestamp, start_time, timediff, cat_x, cat_y, f2B_x, f2B_y, fSS_x, fSS_y, r1B_x, r1B_y, 
         rDist2B, ball_x, ball_y, ball_z, ballDist2B, event_code.x, positionEvent.x, SB) %>%
  rename(event = event_code.x, 
         playerAct = positionEvent.x)





library(ggplot2)

ggplot(new4, aes(rDist2B, ballDist2B, z = SB)) +
  stat_summary_hex() +
  facet_wrap(~event)


new4 %>%
  filter(event == "ball acquired" & (playerAct %in% c("2B", "SS"))) %>%
  ggplot(aes(ball_x, ball_y, color = SB)) +
  geom_point(size = 3) +
  coord_fixed() +
  annotate("rect", xmin = -.625, xmax = .625, ymin = 126.675, ymax = 127.925, alpha = .5,
           angle = 22.5, color = "black") +
  xlim(-7, 10) +
  ylim(117.5, 132) +
  theme_bw()



new4 %>%
  filter(event == "ball acquired" & (playerAct %in% c("2B", "SS"))) %>%
  ggplot(aes(ball_x, ball_z, color = SB)) +
  geom_point(size = 3) +
  annotate("rect", xmin = -.884, xmax = .884, ymin = 0, ymax = .333, alpha = .5, color = "black") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlim(-7, 10)

new4 %>%
  filter(event == "ball acquired" & playerAct == "C") %>%
  ggplot(aes(ball_x, ball_z, color = SB)) +
  geom_point(size = 3) +
  coord_fixed() +
  xlim(-2.5, 2.5) +
  annotate("rect", xmin = -.7, xmax = .7, ymin = 1.6, ymax = 3.5, alpha = .3, color = "black", fill = "red") +
  theme_bw() +
  labs(x = "Horizontal Pitch Location", y = "Vertical Pitch Location")


new4 %>%
  filter(event == "ball acquired" & playerAct == "C", 
         ball_x <= 3 & ball_x >= -3) %>%
  ggplot(aes(ball_y, ball_z, color = SB)) +
  geom_point(size = 3) +
  coord_fixed() +
  annotate("rect", xmin = -.1, xmax = .1, ymin = 1.6, ymax = 3.5, alpha = .3, color = "black", fill = "red") +
  theme_bw() +
  labs(x = "Distance from Plate", y = "Vertical Pitch Location") +
  xlim(-10, 2.5) +
  geom_hline(yintercept = 0)


new4 %>%
  filter(event == "ball acquired" & playerAct == "C", 
         ball_x <= 3 & ball_x >= -3) %>%
  ggplot(aes(ball_x, ball_y, color = SB)) +
  geom_point(size = 3) +
  coord_fixed() +
  annotate("rect", xmin = -.7, xmax = .7, ymin = -.1, ymax = .1, alpha = .3, color = "black", fill = "red") +
  theme_bw() +
  labs(x = "Horizontal Pitch Location", y = "Distance from Plate") +
  geom_hline(yintercept = 0.1) 


new4 %>%
  filter(event == "ball acquired" & (playerAct %in% c("2B", "SS"))) %>%
  ggplot(aes(ballDist2B, rDist2B, color = SB)) +
  geom_point(size = 3) +
  xlim(0, 11.5) +
  ylim(0, 17.5) +
  theme_bw() +
  labs(x = "Ball Distance from 2B", y = "Runner Distance from 2B")





rec = SBdata %>%
  mutate(recTime = ifelse(event == "ball acquired" & playerAct == "C", timestamp, NA),
         catThrowTime = ifelse(event == "throw (ball-in-play)" & playerAct == "C", timestamp, NA), 
         rec2Btime = ifelse(event == "ball acquired" & playerAct %in% c("2B", "SS", "3B"), timestamp, NA)) %>%
  group_by(game_str, play_id) %>%
  mutate(recCtime = mean(recTime, na.rm = T),
         rec2Btime = mean(rec2Btime, na.rm = T),
         catThrTime = mean(catThrowTime, na.rm = T),
         popTime = rec2Btime - recCtime,
         exchange = catThrTime - recCtime,
         timeToPlate = recCtime - start_time, 
         timeOfThrow = rec2Btime - catThrTime)


ggplot(rec, aes(popTime, SB)) +
  geom_point()

ggplot(rec, aes(exchange, SB)) +
  geom_point()

ggplot(filter(rec, event == "ball acquired" & playerAct == "C"), aes(ball_x, ball_z, color = popTime)) +
  geom_point(size = 3) +
  scale_color_gradient2(low = "red", high = "blue", mid = "white", midpoint = 2700) +
  xlim(-3, 3) +
  annotate("rect", xmin = -.7, xmax = .7, ymin = 1.6, ymax = 3.5, alpha = .3, color = "black", fill = "red") +
  coord_fixed() +
  theme_dark()
  
ggplot(filter(rec, event == "ball acquired" & playerAct == "C"), aes(ball_x, ball_z, color = exchange)) +
  geom_point(size = 3) +
  scale_color_gradient2(low = "red", high = "blue", mid = "white", midpoint = 950) +
  xlim(-3, 3) +
  annotate("rect", xmin = -.7, xmax = .7, ymin = 1.6, ymax = 3.5, alpha = .3, color = "black", fill = "red") +
  coord_fixed() +
  theme_dark()







rec %>%
  group_by(SB) %>%
  summarise(popTime = mean(popTime, na.rm = T),
            exchange = mean(exchange, na.rm = T))


write.csv(rec, file = "SBdataPop.csv")




rec2 = rec %>%
  mutate(currSpeed = ifelse(lag(game_str) == game_str & lag(play_id) == play_id, 1000 *(lag(rDist2B) - rDist2B) / (timestamp - lag(timestamp)), NA),
         leadoff = ifelse(event == "pitch", sqrt((63.64 - r1B_x)^2 + (63.64 - r1B_y)^2), NA)) %>%
  group_by(game_str, play_id) %>%
  mutate(maxSpeed = max(currSpeed, na.rm = T),
         leadoff = mean(leadoff, na.rm = T))



add %>%
  group_by(game_str, play_id) %>%
  summarise(maxSpeed = max(currSpeed, na.rm = T),
            SB = mean(SB)) %>%
  arrange(desc(maxSpeed)) %>%
  group_by(SB) %>%
  summarise(speed = mean(maxSpeed))

add %>%
  group_by(SB) %>%
  summarise(lead = mean(leadoff, na.rm = T))



simple = rec2 %>%
  select(game_str, play_id, timestamp, timediff, playerAct, event, SB, popTime, exchange, timeOfThrow, timeToPlate, leadoff, rDist2B, maxSpeed, ball_x, ball_y, ball_z)


indiv = simple %>%
  group_by(game_str, play_id) %>%
  summarise(SB = mean(SB, na.rm = T))


simple %>%
  group_by(SB) %>%
  summarise(throwT = mean(timeOfThrow, na.rm = T)) %>%
  ggplot(aes(factor(SB), throwT)) +
  geom_col()


write.csv(simple, file = "SBsimple.csv")
