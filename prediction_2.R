# Could look into using random forests for feature selection and general prediction
library(tibble)
library(readr)
library(dplyr)
library(psych)
library(ggplot2)
library(stargazer)

# load data set
df <- read_csv("df.csv") %>% select(-...1) %>% arrange(season) %>% select(-market_value_in_eur)

# prepare data set for baseline regression
df_clean <- select(df, -which(colSums(is.na(df)) > 0)) %>%
  select(-c("date_valuation", "player_id", "player_name", "country_of_birth", "date_of_birth", "sub_position",
            "nation", "team", "minutes_in_90s", "goals_per_90", "non_penalty_goals", "penalty_goals", "matches_played",
            "matches_started", "assists_per_90", "goals_excl_pens_per_90")) %>%
  mutate(season = factor(season))

model1 <- lm(change_in_market_value ~ ., data = df_clean)

model2 <- lm(change_in_market_value ~ goals + assists + tackles + touches + minutes_played, data = df_clean)


# thoughts: should we normalize height to get a coefficient that is better interpretable