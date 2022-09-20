library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)

rawData <- load_ff_opportunity(2021)

data <- rawData %>% 
  select(game_id,posteam,position,player_id,full_name,receptions,
         rec_yards_gained,rec_yards_gained_exp,pass_attempt_team,rec_attempt,rec_attempt_team) %>%
  mutate(RecYOE = rec_yards_gained - rec_yards_gained_exp) %>%
  filter(!is.na(full_name)) %>%
  filter(position == 'WR')

data <- data %>%
  group_by(full_name) %>%
  summarise(Targets = sum(rec_attempt),
            Receptions = sum(receptions),
            ExpectedRecYards = sum(rec_yards_gained_exp),
            RecYards = sum(rec_yards_gained),
            RecYOE = sum(RecYOE),
            TeamTargets = sum(rec_attempt_team)) %>%
  filter(Receptions > 30)
  
data <- data %>%
  mutate(TargetMS = Targets / TeamTargets)
