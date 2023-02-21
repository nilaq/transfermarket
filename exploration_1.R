library(tibble)
library(readr)
library(dplyr)
library(psych)
library(ggplot2)
library(stargazer)
library(knitr)

# load data set
df_raw <- read_csv("df.csv") %>% select(-...1) %>% arrange(season)

# split data set into test and training data set
set.seed(123)
df <- df_raw %>%
  sample_frac(0.8)
test_data <- df_raw %>%
  anti_join(df)

# create summary table
summ <- df %>%
  select(market_value_in_eur, plays_for_top_team, height_in_cm, age, 
         minutes_played, goals, assists, yellow_cards, red_cards, 
         shots, shots_on_target, completed_passes, completed_passes_pct, goal_creating_action, 
         tackles, touches, dribbles_attempts, dribbles_success, fouls, transfer, penalty_goals,
         clearances, interceptions, offside)

stargazer(as.data.frame(summ), median = T, title = 'Descriptive Statistics', omit.stat = 'N')

# observations per year per league
table(df$season, df$league)


# first plots
theme_set(theme_grey())

ggplot(df, aes(x=goals, y=market_value_in_eur)) + 
  geom_point() + 
  facet_wrap(~position) +
  geom_smooth(method='lm') +
  ggtitle('Market Value vs. Goals Scored in previous season by position')

ggplot(df, aes(x=factor(season), y=market_value_in_eur)) +
  geom_boxplot() + 
  scale_y_log10() +
  ggtitle('Market Value Distribution by year')

ggplot(df, aes(x=factor(league), y=market_value_in_eur)) +
  geom_boxplot() + 
  scale_y_log10() +
  ggtitle('Market Value Distribution by league') +
  xlab("League") +
  ylab('Market valuation (in €), log scale')

ggplot(df, aes(x=factor(position), y=market_value_in_eur)) +
  geom_boxplot() + 
  scale_y_log10() +
  ggtitle('Market Value Distribution by position')

ggplot(df, aes(x=age, y=market_value_in_eur)) +
  geom_point(shape=1) +
  geom_smooth(method='lm', se=F) +
  facet_wrap(~position) + 
  #scale_y_log10() +
  ggtitle('Market value depending on age and position of the players') +
  xlab('Age') + ylab('Market valuation (in €), log scale') + theme_grey()
  
  
df_raw %>%
  group_by(player_id) %>%
  mutate(mkt_value_lag = lag(market_value_in_eur), diff = mkt_value_lag - market_value_in_eur) %>%
  ggplot(aes(x=market_value_in_eur, y=diff)) + geom_point(shape=1) + geom_smooth(method = 'lm') + scale_x_log10() +geom_abline(slope = 0)

df %>%
  group_by(player_id) %>%
  arrange()
  mutate(mkt_value_lag = lag(market_value_in_eur)) %>%
  filter(mkt_value_lag > 110000000, market_value_in_eur < 14000000)

df %>%
  filter(player_name == "Eden Hazard") %>%
  arrange(season) %>%
  select(market_value_in_eur, date_valuation, season)

df_num <- df %>%
  select_if(is.numeric)

cor_table <- df_num %>%
  select(market_value_in_eur, plays_for_top_team, height_in_cm, age, 
         minutes_played, goals, assists, yellow_cards, red_cards, 
         shots, shots_on_target, completed_passes, goal_creating_action, 
         tackles, touches, dribbles_success, fouls, transfer, penalty_goals,
         clearances, offside) %>%
  cor(., use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  round(.,5)

  cor_table[upper.tri(cor_table, diag = FALSE)] <- NA
  colnames(cor_table)[1:21] <- paste0("(", 1:21, ")")
  rownames(cor_table) <- paste0("(", 1:nrow(cor_table), ") ", rownames(cor_table))

stargazer(cor_table, summary = FALSE, font.size = "footnotesize", digits = 2, title = "Correlation matrix for several variables")
           

colnames_df <- colnames(df)

# Split the column names into 3 equal parts
colnames_split <- split(colnames_df, ceiling(seq_along(colnames_df)/(33)))

colnames_df2 <- as.data.frame(do.call(cbind, colnames_split))

# Print the column names in a LaTeX table with 3 columns using kable()
kable(colnames_df2, "latex", caption = "Column names of the tibble", align = c("l", "l", "l"))


df %>%
  ggplot(aes(x=factor(month(date_of_birth)))) + geom_histogram()

