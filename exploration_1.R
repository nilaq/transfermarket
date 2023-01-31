library(tibble)
library(readr)
#install.packages('psych')
library(psych)
# install.packages('stargazer')
library(stargazer)

# load data set
df_raw <- read_csv("df.csv") %>% select(-...1)

# split data set into test and training data set
set.seed(123)
df <- df_raw %>%
  sample_frac(0.8)
test_data <- df_raw %>%
  anti_join(df)

# create summary table
summ <- df %>%
  select(market_value_in_eur, age, height_in_cm, minutes_played, goals, penalty_goals, assists, completed_passes, completed_passes_pct,
         dribbles_attempts, dribbles_success, tackles, interceptions, clearances, offside, fouls, yellow_cards, red_cards)

stargazer(as.data.frame(summ), median = T, title = 'Summary Statistics', omit.stat = 'N')


# first plots
theme_set(theme_light())

ggplot(df, aes(x=goals, y=market_value_in_eur)) + 
  geom_point() + 
  facet_wrap(~position) +
  geom_smooth(method='lm') +
  ggtitle('Market Value vs. Goals Scored in previous season by position')

ggplot(df, aes(x=factor(year), y=market_value_in_eur)) +
  geom_boxplot() + 
  scale_y_log10() +
  ggtitle('Market Value Distribution by year')

ggplot(df, aes(x=factor(domestic_competition_id), y=market_value_in_eur)) +
  geom_boxplot() + 
  scale_y_log10() +
  ggtitle('Market Value Distribution by league')

ggplot(df, aes(x=factor(position), y=market_value_in_eur)) +
  geom_boxplot() + 
  scale_y_log10() +
  ggtitle('Market Value Distribution by position')

ggplot(df, aes(x=age, y=market_value_in_eur)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(~position) + 
  scale_y_log10()
  
  

