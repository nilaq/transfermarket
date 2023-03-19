library(tibble)
library(readr)
library(dplyr)
library(ggplot2)
library(stargazer)
library(cvTools)
library(class)
library(caret)
library(plyr)

# load data set
df <- read_csv("df.csv") %>%
  select(-...1) %>%
  filter(height_in_cm != 0) %>%
  select(-which(colSums(is.na(.)) > 0)) %>%
  filter(complete.cases(.)) %>%
  mutate(position = factor(position), foot = factor(foot), league = factor(league))

df_classifier <- df %>% 
  select(-c("market_value_in_eur", "date_valuation", "player_id", "player_name", "country_of_birth", "date_of_birth", "sub_position",
            "nation", "team"))

## Classification Models

# baseline model
basline_classifier_r <- runif(length(df_classifier$plays_for_top_team), 0, 1)
predicted_class_r <- ifelse(basline_classifier_r <= 0.25, 1, 0)

random_cm <- plot_conf_matr(predicted_class_r, T)


# define functions for confusion matrices and plots
plot_conf_matr <- function(predictions, plot_g) {
  df_plot <- tibble(df %>% select(market_value_in_eur, change_in_market_value, plays_for_top_team), predictions)
  df_plot <- df_plot %>%
    mutate(confusion =  ifelse(predictions == 1 & plays_for_top_team == 1, "TP",
                               ifelse(predictions == 0 & plays_for_top_team == 0, "TN",
                                      ifelse(predictions == 1 & plays_for_top_team == 0, "FP", "FN")))) %>%
    mutate(correct_prediction = factor(ifelse(predictions == plays_for_top_team, 1, 0)))
  if (plot_g) {
    plot <- ggplot(df_plot, aes(x=market_value_in_eur, y=change_in_market_value)) +
      geom_point(aes(color=correct_prediction), shape=1) +
      facet_wrap(~confusion)
    show(plot) 
  }
  return (confusionMatrix(data=factor(predictions), reference = factor(df$plays_for_top_team)))
}


# simple logistic regression model
df_simple_class <- select(df, c(plays_for_top_team, change_in_market_value, height_in_cm, age, goals, assists,
                                tackles, shots, minutes_played, yellow_cards, season))

simple_classifier <- glm(plays_for_top_team ~ . ,data = df_simple_class, family = binomial)
predicted_probs <- predict(simple_classifier, type = "response")
predicted_class <- ifelse(predicted_probs >= 0.5, 1, 0)

simple_cm <- plot_conf_matr(predicted_class)


# best model: Logistic regression with most covariates
logistic_classifier <- glm(plays_for_top_team ~ . ,data = df_classifier, family = binomial)
predicted_probs <- predict(logistic_classifier, type = "response")
predicted_class <- ifelse(predicted_probs >= 0.25, 1, 0)

logistic_cm <- plot_conf_matr(predicted_class, T)
est_error <- cvFit(logistic_classifier, data=df_classifier, y=df_classifier$plays_for_top_team, K = 10)

# KNN

knn_probs <- function(knn.raw) {
  return(aaply(1:length(knn.raw), 1, function(i) { 
    if (knn.raw[i] == TRUE) { return(attributes(knn.raw)$prob[i]) }
    else { return(1 - attributes(knn.raw)$prob[i]) }
  }))
}

knn_conf_matr <- function(probs, threshold, actuals) {
  knn.out = as.numeric(probs >= threshold)
  return(confusionMatrix(factor(knn.out), factor(actuals)))
}

knn_metrics <- function(conf_matr, thresh) {
  accuracy <- conf_matr$overall['Accuracy'] 
  sensitivity <- conf_matr$byClass['Sensitivity']
  specificity <- conf_matr$byClass['Specificity']
  return(tibble(thresh, accuracy, sensitivity, specificity))
}

knn_metric_plot <- function(probs) {
  metrics <- knn_metrics(knn_conf_matr(probs, 0, df_knn$plays_for_top_team), 0)
  for (t in seq(0.01,1,0.01)) {
    metrics <- rbind(metrics, knn_metrics(knn_conf_matr(probs, t, df_knn$plays_for_top_team), t))
  }
  metrics_long <- metrics %>%
    pivot_longer(cols = c("accuracy", "sensitivity", "specificity"), names_to = "metric", values_to = "value")
  print(metrics_long)
  plot <- ggplot(metrics_long, aes(x=thresh, y=value)) +
    geom_line(aes(color=metric)) +
    ggtitle("Accuracy, Sensitivity and Specificity for different thresholds for KNN with k=15") +
    ylab("Value") + xlab("Threshold") +
    geom_vline(xintercept = 0.25)
  show(plot)
}

knn_eval_k <- function(k, t=0.25) {
  knn.raw <- knn(train=df_knn, test=df_knn, cl=df_knn$plays_for_top_team, k=k, prob = T)
  knn.probs = knn_probs(knn.raw)
  cm <- knn_conf_matr(knn.probs, t, df_knn$plays_for_top_team)
  return(knn_metrics(cm, t))
}

knn_varying_k <- function() {
  metrics <- tibble(k=1, knn_eval_k(1,0.25))
  for (k in seq(2,50,1)) {
    metrics <- rbind(metrics, tibble(k=k, knn_eval_k(k,0.25)))
  }
  return(metrics)
}


# knn with all covariates
df_knn <- df_classifier %>%select(-position, -foot, -league, -season)
knn.raw <- knn(train=df_knn, test=df_knn, cl=df_knn$plays_for_top_team, k=15, prob = T)
knn.probs = knn_probs(knn.raw)

cm <- knn_conf_matr(knn.probs, 0.26, df_knn$plays_for_top_team)
knn_metrics(cm, 0.25)
knn_metric_plot(knn.probs)

metrics_for_varying_k <- knn_varying_k()
metrics_long <- metrics_for_varying_k %>%
  select(-thresh) %>%
  pivot_longer(cols = c("accuracy", "sensitivity", "specificity"), names_to = "metric", values_to = "value")
ggplot(metrics_long, aes(x=k, y=value)) +
  geom_line(aes(color=metric)) +
  ggtitle("Accuracy, Sensitivity and Specificity for different K for KNN at threshold 0.25") +
  ylab("Value") + xlab("K") 





