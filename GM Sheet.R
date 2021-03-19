#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/Baseball-Front-Office-Analysis")

#Loading libraries

library(tidyverse)

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
  summarize(CareerWins = sum(CurrentWins), CareerLosses = sum(CurrentLosses), WinPct = round(sum(CurrentWins) / (sum(CurrentWins) + sum(CurrentLosses)),3)) %>%
  ungroup()

#Finding GMs who inherited a rebuilding team

rebuild_data <- data %>%
  filter(Seasons_With_Team == 1 & PreviousWPct < .432)