library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)

rawData <- load_ff_opportunity(2021)

data <- rawData %>% 
  select(game_id,posteam,position,player_id,full_name,receptions,
         rec_yards_gained,rec_yards_gained_exp,pass_attempt_team,rec_attempt,rec_attempt_team) %>%
  mutate(RecYOE = rec_yards_gained - rec_yards_gained_exp) %>%
  filter(!is.na(full_name))
  #filter(position == 'WR')

data <- data %>%
  group_by(full_name,position) %>%
  summarise(Targets = sum(rec_attempt),
            Receptions = sum(receptions),
            ExpectedRecYards = sum(rec_yards_gained_exp),
            RecYards = sum(rec_yards_gained),
            RecYOE = sum(RecYOE),
            TeamTargets = sum(rec_attempt_team)) %>%
  filter(Receptions > 20)
  
data <- data %>%
  mutate(TargetMS = Targets / TeamTargets)

data %>%
  ggplot(aes(x=TeamTargets,y=Targets,size=(RecYOE^2),color=position)) +
  geom_point(alpha=0.5) +
  theme_bw() +
  labs(x='Team Targets',
       y='Player Targets',
       title='Target Market Share in 2021',
       caption='By David Gornowicz | @twitterHandle') +
  theme(plot.title = element_text(size=18,face='bold',hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))

ggsave('TgtMS2021.png',width=12,height=12,dpi='retina')
  
  
  
  
