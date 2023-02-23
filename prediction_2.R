# Could look into using random forests for feature selection and general prediction
library(tibble)
library(readr)
library(dplyr)
library(psych)
library(ggplot2)
library(stargazer)
library(GGally)
library(randomForest)
library(cvTools)

# load data set
df <- read_csv("df.csv") %>% select(-...1) %>% arrange(season) %>% select(-market_value_in_eur) %>%
  mutate(completed_passes_pct = completed_passes/attempted_passes) %>%
  mutate(tackles_won = ifelse(tackles_won != 0, tackles_won/tackles, tackles_won),
         shots_on_target_pct = ifelse(shots == 0, 0, shots_on_target_pct),
         goals_per_shot = ifelse(shots == 0, 0, goals_per_shot))%>%
  rename(passes = attempted_passes, tackles_won_pct = tackles_won) %>%
  filter(height_in_cm != 0)

# prepare data set for baseline regression. Remove non-performance vars and those with NA values 
df_full_reg <- select(df, -which(colSums(is.na(df)) > 0)) %>%
  select(-c("date_valuation", "player_id", "player_name", "country_of_birth", "date_of_birth",
            "nation", "team", "season"))

# Model 1: Baseline model with all covariates
model_baseline <- lm(change_in_market_value ~ ., data = df_full_reg)
baseline_cv10 <- cvFit(model_baseline, data = df_full_reg, y = df_full_reg$change_in_market_value, K = 10, seed=123)

# Building a correlation table to check for potential collinearity (i.e. very high correlation)
cor_table <- df %>%
  select_if(is.numeric) %>%
  cor(., use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  round(.,5) %>%
  
df_clean <- df_full_reg %>%
  mutate(aerial_success_pct = aerial_duels_won/(aerial_duels_won + aerial_duels_lost)) %>%
  select(-c(gca_defensive, gca_pass_dead, gca_dribbling, gca_shots, gca_fouls_drawn, gca_defensive,
            aerial_duels_won, aerial_duels_lost, red_cards, crosses, throwins, corner_kicks,
            touches_in_attacking_3rd, touches_in_att_box, goals_per_90, assists_per_90,
            league, plays_for_top_team, transfer, own_goals, second_yellow_card, completed_passes_box, 
            progressive_passes, minutes_in_90s, non_penalty_goals, penalty_goals, matches_played,
            matches_started, goals_excl_pens_per_90, completed_passes, touches_live, touches, 
            touches_in_mid_3rd, passes_received, total_progressive_distance, blocked_shots,
            blocked_passes, tackles_def_3rd, tackles_mid_3rd, tackles_att_3rd, tackles_vs_dribblers_success,
            tackles_vs_dribblers_attempts, tackles_vs_dribblers_nosuccess, exp_goals, exp_assists, shots_on_target,
            shot_creating_actions, gca_pass_live, blocks, touches_in_def_box, touches_in_def_3rd, loose_balls_recovered,
            dribbles_attempts, progressive_passes_received, completed_crosses_box, interceptions, tackles, sub_position))

# Random forests
df_rf <- na.omit(df_full_reg)
rf.fit_full <- randomForest(change_in_market_value ~ ., data = df_rf, ntree = 500, mtry = sqrt(ncol(df_rf) - 1))

df_clean_rf <- na.omit(df_clean)
rf.fit <- randomForest(change_in_market_value ~ ., data = df_clean_rf, ntree = 500, mtry = sqrt(ncol(df_clean_rf) - 1))
varImpPlot(rf.fit, main="Feature Importance of Best Model")
  
# Building a data frame on the 12 most important predictors according to random forest
df_simple <- select(df, c(change_in_market_value, age, goal_creating_action, passes, minutes_played, goals, completed_passes_pct,
                          dribbles_success, shots, ball_control_lost_after_tackle, tackles_interceptions, clearances, assists))

# model2 <- lm(change_in_market_value ~ ., data = df_simple)
# model2_cv10 <- cvFit(model3, data = df_full_reg, y = df_full_reg$change_in_market_value, K = 10)

# create ggpairs plot for data exploration
pairs <- ggpairs(df_simple)

# # Model 3: Based on the simple model, calculate all performance stats on a "per 90 min" basis
# df_simple_90s <- df_simple %>%
#   mutate(goals = goals / (minutes_played / 90), assists = assists / (minutes_played / 90), 
#          shots = shots / (minutes_played / 90), passes = passes / (minutes_played / 90), 
#          tackles_interceptions = tackles_interceptions / (minutes_played / 90), 
#          yellow_cards = yellow_cards / (minutes_played / 90)) %>%
#   rename(goals_per_90 = goals, assists_per_90 = assists, shots_per_90 = shots, passes_per_90 = passes, 
#          tackles_interceptions_per_90 = tackles_interceptions, yellows_per_90 = yellow_cards)
# 
# model4 <- lm(change_in_market_value ~ ., data = df_simple_90s)
# model4_cv10 <- cvFit(model4, data = df_full_reg, y = df_full_reg$change_in_market_value, K = 10)
# 
# # create ggpairs plot again
# pairs_90s <- ggpairs(df_simple_90s)
  
# Model 4: Add interaction terms and logistic transformations
model5 <- lm(change_in_market_value ~ . - age + log(age) - tackles_interceptions + log(tackles_interceptions + 0.1) 
             - passes + log(passes) - goals + sqrt(goals)
             + goals:position + passes:position + age:position + goals:age + assists:age + goal_creating_action:age,
             data = df_clean)
model5_cv10 <- cvFit(model5, data = df_full_reg, y = df_full_reg$change_in_market_value, K = 10, seed=123)

## Secret Test check 
test <- read_csv("test_data.csv") %>% select(-...1) %>% arrange(season) %>% select(-market_value_in_eur) %>%
  mutate(completed_passes_pct = completed_passes/attempted_passes) %>%
  mutate(tackles_won = ifelse(tackles_won != 0, tackles_won/tackles, tackles_won),
         shots_on_target_pct = ifelse(shots == 0, 0, shots_on_target_pct),
         goals_per_shot = ifelse(shots == 0, 0, goals_per_shot))%>%
  rename(passes = attempted_passes, tackles_won_pct = tackles_won) %>%
  filter(height_in_cm != 0) %>%
  mutate(aerial_success_pct = aerial_duels_won/(aerial_duels_won + aerial_duels_lost)) %>%
  select(., -which(colSums(is.na(df)) > 0))
  
test_omit <- na.omit(test)
test_rmse <- sqrt(mean((test_omit$change_in_market_value - predict(model5, test_omit))^2))
