# Could look into using random forests for feature selection and general prediction
library(tibble)
library(readr)
library(dplyr)
library(psych)
library(ggplot2)
library(stargazer)
library(GGally)

# load data set
df <- read_csv("df.csv") %>% select(-...1) %>% arrange(season) %>% select(-market_value_in_eur) %>%
  mutate(completed_passes_pct = completed_passes/attempted_passes) %>%
  mutate(tackles_won = ifelse(tackles_won != 0, tackles_won/tackles, tackles_won))%>%
  rename(passes = attempted_passes, tackles_won_pct = tackles_won) %>%
  filter(height_in_cm != 0)

# prepare data set for baseline regression. Remove cols with NA values and those irrelevant for regression
df_full_reg <- select(df, -which(colSums(is.na(df)) > 0)) %>%
  select(-c("date_valuation", "player_id", "player_name", "country_of_birth", "date_of_birth", "sub_position",
            "nation", "team")) %>%
  mutate(season = factor(season))

model_baseline <- lm(change_in_market_value ~ ., data = df_full_reg)

# building a correlation table to check for potential collinearity (i.e. very high correlation)
cor_table <- df %>%
  select_if(is.numeric) %>%
  cor(., use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  round(.,5) %>%
  mutate_all(~ ifelse(. < 0.8, NA, .))

# adjusting the data set accordingly 
df_clean <- df_full_reg %>%
  select(-c("minutes_in_90s", "non_penalty_goals", "penalty_goals", "matches_played",
            "matches_started", "goals_excl_pens_per_90", "completed_passes", "touches_live", "touches", 
            "touches_in_mid_3rd", "passes_received", "total_progressive_distance", "blocked_shots",
            "blocked_passes", "tackles_def_3rd", "tackles_mid_3rd", "tackles_att_3rd", "tackles_vs_dribblers_success",
            "tackles_vs_dribblers_attempts", "tackles_vs_dribblers_nosuccess"))

# don't know how best to deal with _per_90 stats (assists, goals)

df_simple <- select(df, c("change_in_market_value", "height_in_cm", "age", "goals", "assists",
                         "tackles", "shots", "passes", "minutes_played", "yellow_cards"))

pairs <- ggpairs(df_basis)

df_basis_90s <- select(df, c("change_in_market_value", "height_in_cm", "age", "goals_per_90", "assists_per_90",
                             "tackles", "shots", "passes", "minutes_played", "yellow_cards")) %>%
  mutate(tackles = tackles / (minutes_played / 90), shots = shots / (minutes_played / 90),
         passes = passes / (minutes_played / 90), yellow_cards = yellow_cards / (minutes_played / 90)) %>%
  rename(tackles = tackles_per_90, shots = shots_per_90, passes = passes_per_90, yellow_cards = yellows_per_90)

pairs_90s <- ggpairs(df_basis_90s)

model_simple <- lm(change_in_market_value ~ ., data = df_simple)

model3 <- lm(change_in_market_value ~ .:., data = df_clean)

# thoughts: should we normalize height to get a coefficient that is better interpretable
