library(readr)
library(ggplot2)
library(tidyverse)

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

# explore
player_valuations <- player_valuations %>% 
  mutate(year = factor(year(date)))

ggplot(player_valuations, aes(x=date, y=market_value_in_eur)) +
  geom_point()
