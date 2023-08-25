SBsimple = read.csv("C:/Users/kaifr/OneDrive/Productivity/Baseball Research/SMT 2023/SMT-Data-Challenge/SBsimple.csv")

library(ggplot2)
library(dplyr)

SBsimple$SB = as.factor(SBsimple$SB)
SBsimple = SBsimple %>%
  mutate(SB = ifelse(SB == 1, "Stolen Base", "Caught Stealing"))






#### Pitch ####

pitch = SBsimple %>%
  filter(playerAct == "P",
         event == "pitch")


ggplot(pitch, aes(ball_x, ball_y, color = SB)) +
  geom_point(size = 5) +
  theme_bw() +
  scale_color_manual(values = c("red", "green")) +
  coord_fixed() +
  annotate("rect", xmin = -.7, xmax = .7, ymin = 60.4, ymax = 60.6, alpha = 1,
          color = "black", fill = "white") +
  ylim(52,60.6) +
  labs(x = "Horizontal Release from Rubber", y = "Release Distance from Home Plate", 
       title = "Bird's Eye View Release Point Location with Stolen Base or Not",
       caption = "Source: 2023 SMT Data Challenge Data")


ggplot(pitch, aes(ball_x, ball_z, color = SB)) +
  geom_point(size = 5) +
  theme_bw() +
  scale_color_manual(values = c("red", "green")) +
  coord_fixed() +
  geom_hline(yintercept = 0) +
  annotate("rect", xmin = -.7, xmax = .7, ymin = 0, ymax = .1, alpha = 1,
           color = "black", fill = "white") +
  annotate("rect", xmin = -5, xmax = 5, ymin = -1, ymax = 0, fill = "brown") +
  labs(x = "Horizontal Distance from Rubber", y = "Release Height",
       title = "Batter's View of Release Point with Stolen Base or Not",
       caption = "Source: 2023 SMT Data Challenge Data")
  


pitch %>%
  group_by(SB) %>%
  summarise(meanLead = mean(leadoff)) %>%
  ggplot(aes(SB, meanLead, fill = SB)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("red", "green")) +
  theme_bw() +
  labs(x = "Stolen Base or Not", y = "Average Leadoff", title = "Average Leadoff by Stolen Base or Not",
       caption = "Source: 2023 SMT Data Challenge Data") +
  geom_text(aes(label = round(meanLead, 1)), vjust = -0.25, size = 5) +
  ylim(0, 19.75)



pitch %>%
  group_by(SB) %>%
  summarise(meanSpeed = mean(maxSpeed)) %>%
  ggplot(aes(SB, meanSpeed, fill = SB)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("red", "green")) +
  theme_bw() +
  labs(x = "Stolen Base or Not", y = "Average Speed", title = "Average Speed by Stolen Base or Not",
       caption = "Source: 2023 SMT Data Challenge Data") +
  geom_text(aes(label = round(meanSpeed, 1)), vjust = -0.25, size = 5) +
  ylim(0, 22.75)



#### Catcher Caught ####

catC = SBsimple %>%
  filter(playerAct == "C",
         event == "ball acquired",
         lag(event) == "pitch")


ggplot(catC, aes(ball_x, ball_z, color = SB)) +
  geom_point(size = 5) +
  scale_color_manual(values = c("red", "green")) +
  theme_bw() +
  annotate("rect", xmin = -.7, xmax = .7, ymin = 1.6, ymax = 3.5, alpha = .5,
           color = "black") +
  coord_fixed() +
  xlim(-2.5,2.5) +
  labs(x = "Horizontal Pitch Location", y = "Pitch Height", title = "Stolen Bases by Pitch Location",
       caption = "Source: 2023 SMT Data Challenge Data")


catC %>%
  group_by(SB) %>%
  summarise(meanTTP = mean(timeToPlate) / 1000) %>%
  ggplot(aes(SB, meanTTP, fill = SB)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("red", "green")) +
  theme_bw() +
  labs(x = "Stolen Base or Not", y = "Mean Pitch Time to Glove", 
       title = "Mean Pitch Time by Stolen Base or Not",
       subtitle = "Time in Seconds",
       caption = "Source: 2023 SMT Data Challenge Data")





#### Catcher Throw ####

catT = SBsimple %>%
  filter(playerAct == "C",
         event == "throw (ball-in-play)")


catT %>%
  group_by(SB) %>%
  summarise(meanExch = mean(exchange, na.rm = T) / 1000) %>%
  ggplot(aes(SB, meanExch, fill = SB)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("red", "green")) +
  theme_bw() +
  labs(x = "Stolen Base or Not", y = "Exchange Time", 
       title = "Exchange Time By Stolen Base or Not",
       subtitle = "Time in Seconds",
       caption = "Source: 2023 SMT Data Challenge Data")


#### Fielder Catch ####


field = SBsimple %>%
  filter(playerAct %in% c("2B", "SS"),
         lag(playerAct) == "C",
         event == "ball acquired")


field %>%
  group_by(SB) %>%
  summarise(throwspeed = mean(timeOfThrow) / 1000) %>%
  ggplot(aes(SB, throwspeed, fill = SB)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("red", "green")) +
  theme_bw() +
  labs(x = "Stolen Base or Not", y = "Time for Throw to Reach Second Base",
       title = "Throw Time By Stolen Base or Not",
       subtitle = "Time in Seconds",
       caption = "Source: 2023 SMT Data Challenge Data")





ggplot(field, aes(ball_x, ball_z, color = SB)) +
  geom_point(size = 5) +
  annotate("rect", xmin = -.625, xmax = .625, ymin = 0, ymax = .25, alpha = 1,
           color = "black", fill = "white") +
  theme_bw() +
  scale_color_manual(values = c("red", "green")) +
  geom_hline(yintercept = 0) +
  labs(x = "Horizontal Distance from Second Base", y = "Throw Height",
       title = "Stolen Base or Not by Catcher Throw Location",
       subtitle = "Distance in Feet",
       caption = "Source: 2023 SMT Data Challenge Data")
  


