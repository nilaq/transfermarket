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
  mutate(year = factor(year(date))) %>%
  select(-datetime, -dateweek, -current_club_id, -player_club_domestic_competition_id)

# clean players
players_clean <- players %>%
  select(-country_of_citizenship, -city_of_birth, -market_value_in_eur, -highest_market_value_in_eur,
         -first_name, -last_name, -player_code, -image_url, -url, -current_club_domestic_competition_id) %>%
  mutate(age = trunc((date_of_birth %--% Sys.Date()) / years(1)))

# clean clubs
clubs_clean <- clubs %>% 
  select(club_id, name, domestic_competition_id,  national_team_players, net_transfer_record, stadium_seats)

## Join all tables together into one df
df <- player_valuations_clean %>%
  left_join(players_clean, by='player_id') %>%
  rename(club_id = current_club_id) %>%
  left_join(clubs_clean, by='club_id')


ggplot(df, aes(x=year, y=market_value_in_eur)) +
  geom_boxplot()

