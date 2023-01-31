library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)

# read all files in 
setwd("~/project1")
folder <- "./transfermarket/"
csv_files <- list.files(folder, pattern = "*.csv", full.names = TRUE)

for (i in csv_files) {
  df <- read_csv(i, col_types = cols())
  var_name <- gsub(".csv$", "", basename(i))
  assign(var_name, df)
}
rm(df, csv_files, folder, i, var_name)

## Clean tables player_valuations, players and clubs and only select relevant columns to join together in one df

# clean player valuations
player_valuations_clean <- player_valuations %>% 
  mutate(year = year(date)) %>%
  select(-datetime, -dateweek, -current_club_id, -player_club_domestic_competition_id)

# clean players
players_clean <- players %>%
  select(-country_of_citizenship, -city_of_birth, -market_value_in_eur, -highest_market_value_in_eur,
         -first_name, -last_name, -player_code, -image_url, -url, -current_club_domestic_competition_id) %>%
  mutate(age = trunc((date_of_birth %--% Sys.Date()) / years(1))) %>%
  rename(club_id = current_club_id)

# clean clubs
clubs_clean <- clubs %>% 
  select(club_id, name, domestic_competition_id,  national_team_players, net_transfer_record, stadium_seats) %>%
  rename(club_name = name)

# count goals per player
goals_per_season <- game_events %>% 
  filter(type == 'Goals') %>%
  left_join(games, by='game_id') %>%
  mutate(year = year(date)) %>%
  select(player_id, year) %>%
  group_by(player_id, year) %>%
  summarise(goals = n())


## Join all tables together into one df
df_raw <- player_valuations_clean %>%
  left_join(players_clean, by='player_id') %>%
  left_join(clubs_clean, by='club_id') %>%
  mutate(prev_year = year - 1) %>%
  left_join(goals_per_season, by=c('prev_year' = 'year', 'player_id' = 'player_id')) %>%
  select(-prev_year) %>%
  rename(goals_prev_season = goals)

## Restrict Scope of data set
df <- df_raw %>%
  filter(year(date) >= 2010) %>%
  filter(domestic_competition_id %in% (competitions %>%
           filter(sub_type == 'first_tier') %>%
           filter(country_name %in% c('Germany', 'Spain', 'France', 'England', 'Italy')))$competition_id)

## create subset for top_transfers
top_transfers <- df %>%
  filter(market_value_in_eur > quantile(market_value_in_eur, .99))

ggplot(top_transfers, aes(x=factor(year), y=market_value_in_eur)) +
  geom_boxplot()

ggplot(df, aes(x=goals_prev_season, y=market_value_in_eur)) + 
  geom_point() + 
  facet_wrap(~position) +
  geom_smooth(method='lm')
  

