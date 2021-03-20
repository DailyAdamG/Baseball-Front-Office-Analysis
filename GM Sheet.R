#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/Baseball-Front-Office-Analysis")

#Loading libraries

library(tidyverse)
library(ggprel)

#Import data

data <- read.csv("C:/Users/daily/Desktop/Repositories/Baseball-Front-Office-Analysis/Baseball GMs.csv")

#Find how many years a GM was with a team

data <- data %>%
  arrange(franchID, YearID) %>%
  group_by(GM, franchID) %>%
  mutate(Seasons_With_Team = row_number()) %>%
  ungroup()

#Find out how many GMs in each season with team

data %>%
  group_by(Seasons_With_Team) %>%
  count(Seasons_With_Team) %>%
  ungroup()

#Calculate GM stats

GM_wins <- data %>%
  group_by(GM) %>%
  summarize(Seasons = n(),
            CareerWins = sum(CurrentWins), 
            CareerLosses = sum(CurrentLosses), 
            CareerWinPct = round(sum(CurrentWins) / (sum(CurrentWins) + sum(CurrentLosses)),3),
            BestSeasonWinPct = max(round(CurrentWins / (CurrentWins + CurrentLosses),3)),
            PlayoffApp = sum(POApp),
            PlayoffPct = round(sum(POApp) / n(),3),
            Pennants = sum(LgWin),
            WorldSeriesWins = sum(WSWin)) %>%
  ungroup()

#Finding GMs who inherited a team with fewer than 70 wins in a 162 game season by win percentage

rebuild_list <- data %>%
  filter(Seasons_With_Team == 1 & PreviousWPct < .432) %>%
  select(GM, franchID)

#Getting complete data for rebuilding GMs

rebuild_data <- merge(x = rebuild_list, y = data, by = c("GM", "franchID"))

#Calculate GM stats for rebuilding GMs

Rebuild_GM_wins <- rebuild_data %>%
  group_by(GM, franchID) %>%
  summarize(Seasons = n(),
            CareerWins = sum(CurrentWins), 
            CareerLosses = sum(CurrentLosses), 
            CareerWinPct = round(sum(CurrentWins) / (sum(CurrentWins) + sum(CurrentLosses)),3),
            BestSeasonWinPct = max(round(CurrentWins / (CurrentWins + CurrentLosses),3)),
            PlayoffApp = sum(POApp),
            PlayoffPct = round(sum(POApp) / n(),3),
            Pennants = sum(LgWin),
            WorldSeriesWins = sum(WSWin)) %>%
  ungroup()

#Find thresholds for seasons on job of rebuilding team

rebuild_data %>%
  select(GM, franchID, Seasons_With_Team, CurrentWPct) %>%
  spread(Seasons_With_Team, CurrentWPct) %>%
  summary()

rebuild_data %>%
  select(GM, franchID, Seasons_With_Team, POApp) %>%
  spread(Seasons_With_Team, POApp)

#Filter to only include through season 7

shortened_rebuild_data <- rebuild_data %>%
  filter(Seasons_With_Team <= 7)

#Create visual

shortened_rebuild_data %>%
ggplot(aes(x = factor(Seasons_With_Team), y = CurrentWPct)) +
  geom_point() +
  geom_point(data = subset(shortened_rebuild_data, POApp == 1), color = "gold", size = 2) +
  geom_point(data = subset(shortened_rebuild_data, CurrentWPct < .384), color = "red", size = 2) +
  geom_boxplot(fill = "blue", alpha = .5, outlier.shape = NA) +
  geom_hline(yintercept = .5, size = 1, linetype = "dashed") +
  scale_y_continuous(breaks = seq(.25, .75, .050)) +
  labs(title = "Winning Percentage by Season with Team", 
       x = "Season with Team", 
       y = "Winning Percentage", 
       caption = "Playoff teams in Gold \n 100 Loss team pace in Red") +
      theme(plot.title = element_text(hjust = 0.5))