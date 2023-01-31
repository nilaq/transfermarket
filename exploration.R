library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)
library(stringi)

# read all files in 
# setwd("~/project1")
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
  group_by(player_id, year = year(date)) %>%
  arrange(player_id, year, abs(as.Date(paste0(year, "-06-30")) - date)) %>%
  slice_head(n = 1L) %>%
  ungroup() %>%
  select(-datetime, -dateweek, -current_club_id, -player_club_domestic_competition_id)

# clean players
players_clean <- players %>%
  select(-country_of_citizenship, -city_of_birth, -market_value_in_eur, -highest_market_value_in_eur,
         -first_name, -last_name, -player_code, -image_url, -url, -current_club_domestic_competition_id) %>%
  mutate(age = trunc((date_of_birth %--% Sys.Date()) / years(1))) %>%
  rename(club_id = current_club_id) %>%
  mutate(name = stri_trans_general(name, "Latin-ASCII"))

# clean player_performance
player_performance_clean <- player_performance %>%
  mutate(name = stri_trans_general(name, "Latin-ASCII"))

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
  # left_join(goals_per_season, by=c('prev_year' = 'year', 'player_id' = 'player_id')) %>%
  mutate(birthyear = year(date_of_birth)) %>%
  left_join(player_performance_clean, by=c('name' = 'name', 'year' = 'Year', 'birthyear' = 'birthyear')) %>%
  select(-prev_year)
  # rename(goals_prev_season = goals)

## Restrict Scope of data set
df <- df_raw %>%
  filter(year(date) >= 2018) %>%
  filter(!is.na(.$minutes_played))

## Check which players from performance data are not correctly linked with transfermarkt data
player_performance_not_in_df <- anti_join(player_performance_clean, df)

## create subset for top_transfers
top_transfers <- df %>%
  filter(market_value_in_eur > quantile(market_value_in_eur, .99))

ggplot(top_transfers, aes(x=factor(year), y=market_value_in_eur)) +
  geom_boxplot()

ggplot(df, aes(x=shots, y=market_value_in_eur)) + 
  geom_point() + 
  geom_smooth(method='lm')
  

