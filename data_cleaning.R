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
  group_by(player_id, year) %>%
  arrange(player_id, year, abs(as.Date(paste0(year, "-06-30")) - date)) %>%
  slice_head(n = 1L) %>%
  ungroup() %>%
  select(-datetime, -dateweek, -current_club_id, -player_club_domestic_competition_id)

# clean players
players_clean <- players %>%
  select(-country_of_citizenship, -city_of_birth, -market_value_in_eur, -highest_market_value_in_eur,
         -first_name, -last_name, -player_code, -image_url, -url, -current_club_domestic_competition_id) %>%
  mutate(birthyear = year(date_of_birth)) %>%
  rename(club_id = current_club_id) %>%
  mutate(name = stri_trans_general(name, "Latin-ASCII"))

# clean player_performance and sum across teams if there was a transfer mid-season and recalculate all relative metrics
player_performance_metadata <- player_performance %>%
  mutate(name = stri_trans_general(name, "Latin-ASCII")) %>%
  group_by(name, birthyear, Year, age, nation) %>%
  summarise(position = last(position), team = last(team), league = last(league), n = n()) %>%
  mutate(transfer = n > 1) %>%
  select(-n) %>%
  ungroup() %>%
  left_join(players_clean %>% select(name, birthyear, player_id), by = c('name', 'birthyear')) %>%
  filter(!is.na(player_id))

player_performance_raw <- player_performance %>%
  mutate(name = stri_trans_general(name, "Latin-ASCII")) %>%
  group_by(name, birthyear, Year, age, nation) %>%
  select(-position, -team, -league) %>%
  summarise(across(everything(), sum)) %>%
  mutate(
    minutes_in_90s = minutes_played / 90,
    goals_per_90 = goals / minutes_in_90s,
    assists_per_90 = assists/ minutes_in_90s,
    goals_assists_per_90 = goals_per_90 + assists_per_90,
    goals_excl_pens_per_90 = non_penalty_goals / minutes_in_90s,
    goals_assists_excl_pens_per_90 = goals_excl_pens_per_90 + assists_per_90,
    shots_per_90 = shots/ minutes_in_90s,
    shots_on_target_per_90 = shots_on_target/ minutes_in_90s,
    shots_on_target_pct = shots_on_target / shots,
    goals_per_shot = goals / shots,
    goals_per_shot_on_target = goals / shots_on_target,
    completed_passes_pct = completed_passes / (completed_passes + attempted_passes),
    short_passes_completed_pct = short_passes_completed / (short_passes_completed + short_passes_attempted),
    medium_passes_completed_pct = medium_passes_completed_pct...47 / (medium_passes_completed_pct...47 + medium_passes_attempted),
    long_passes_completed_pct = long_passes_completed / (long_passes_completed + long_passes_attempted)
  ) %>%
  select(-medium_passes_completed_pct...47, -medium_passes_completed_pct...49) %>%
  ungroup() 

player_performance_clean <- player_performance_metadata %>%
  left_join(player_performance_raw, by = c('name', 'birthyear', 'Year', 'age', 'nation')) %>%
  select(-name, -birthyear, -position)

rm(player_performance_raw, player_performance_metadata)

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
  summarise(goals = n()) %>%
  ungroup()

## Join all tables together into one df
df_raw <- player_valuations_clean %>%
  left_join(players_clean, by='player_id') %>%
  left_join(clubs_clean, by='club_id') %>%
  inner_join(player_performance_clean, by=c('player_id', 'year' = 'Year'))

## Restrict Scope of data set
df <- df_raw %>%
  filter(year >= 2018) %>%
  filter(minutes_played >= 90) %>%
  filter(position != 'Goalkeeper')

## Check which players from performance data are not correctly linked with transfermarkt data
player_performance_not_in_df <- anti_join(player_performance_clean, player_valuations_clean,  by=c('player_id', 'Year' = 'year')) %>%
  group_by(player_id) %>%
  summarise() %>%
  left_join(players_clean %>% select(player_id, name), by = 'player_id')

write.csv(df, "df.csv")

  

