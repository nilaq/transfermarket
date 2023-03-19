library(tibble)
library(readr)
library(dplyr)

# load data sets
df_train <- read_csv("df_classifier.csv") %>% select(-...1)
df_test <- read_csv("df_classifier_test.csv") %>% select(-...1)


# Best model out of part 2
model_1 <- glm(plays_for_top_team ~ . ,data = df_train, family = binomial)
model_1_probs <- predict(model_1, df_train, type = "response")

# compute average 0-1 loss
loss <- mean(ifelse(model_1_probs < 0.5, 0, 1) != df_train$plays_for_top_team)

## Test check 
test_probs <- predict(model_1, df_test, type = "response")
loss_test <- mean(ifelse(test_probs < 0.5, 0, 1) != df_test$plays_for_top_team)

# perform the same thing with cross validation
cv <- function(data, index) {
  data_fold <- data[index, ]
  model <- glm(plays_for_top_team ~ ., data = data_fold, family = binomial)
  preds <- predict(model, data[-index, ], type = "response")
  loss <- mean(ifelse(preds < 0.5, 0, 1) != data$plays_for_top_team[-index])
  return(loss)
}

cv_kfolds <- function(K) {
  fold_indices <- rep(1:K, length.out = nrow(df_train))
  fold_indices <- sample(fold_indices)
  losses <- rep(0, K)
  for (i in 1:K) {
    fold_indices_i <- which(fold_indices == i)
    losses[i] <- cv(df_train, fold_indices_i)
  }
  return(mean(losses))
}

test <- rep(0, 19)
for (i in 2:20) {
  test[i] <- cv_kfolds(i)
}
test[1] <- loss
folds <- 1:20

ggplot(tibble(folds, test), aes(x=folds, y=test)) +
  geom_line() +
  ylab("avg. 0-1 loss") +
  ggtitle("0-1 loss dependend on K folds in CV")

