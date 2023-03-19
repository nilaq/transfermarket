library(boot)
library(tidyverse)

# load data sets
df_train <- read_csv("df_clean.csv")

# regression model
regr_model <- lm(data = df_train, change_in_market_value ~ . - age + log(age) - tackles_interceptions + log(tackles_interceptions + 0.1) 
   - passes + log(passes) - goals + sqrt(goals)
   + goals:position + passes:position + age:position + goals:age + assists:age + goal_creating_action:age)

regr_se <- summary(regr_model)$coefficients[,2]
regr_ci <- confint(regr_model, level = 0.95)

# create function for standard errors
coef.boot = function(data, indices) {
  fm = lm(data = data[indices,], change_in_market_value ~ . - age + log(age) - tackles_interceptions + log(tackles_interceptions + 0.1) 
          - passes + log(passes) - goals + sqrt(goals)
          + goals:position + passes:position + age:position + goals:age + assists:age + goal_creating_action:age)
  return(coef(fm))
}

# estimate standard error 
boot.out <- boot(df_train, coef.boot, 50000)

# build normal confidence interval
estimates <- colMeans(boot.out$t)
bias <- colMeans(boot.out$t)-boot.out$t0
standard_errors <- apply(boot.out$t, 2, sd)
original_coeffs <- estimates - bias

# construct the confidence intervals
results <- tibble(coefficient=names(original_coeffs), boot.ci.low=estimates-1.96*standard_errors, boot.ci.high=estimates+1.96*standard_errors, 
                  estimate = estimates, se=standard_errors, original = original_coeffs, p_original = summary(regr_model)$coefficients[,4], regr.ci.low=regr_ci[,1], regr.ci.high=regr_ci[,2] )

# check if bootstrap estimates vs 
ifelse(mean(original_coeffs-coef(regr_model)==0)==1,"correct", "bootstrap coeffs false")

# compare
mean(((standard_errors - regr_se) / regr_se))
sum((standard_errors - regr_se) > 0)
sum((standard_errors - regr_se) < 0)

# plot for 1
out <- as_tibble(boot.out$t)
ggplot(out, aes(x=V3)) +
  geom_histogram(bins=100) +
  geom_vline(xintercept = estimates[3], col="red") +
  geom_vline(xintercept = original_coeffs[3], col = "blue")



results %>% arrange(desc(estimate))
results_small <- results %>% arrange(desc(estimate)) %>% filter(estimate<0 & estimate > -1000000)


results <- results %>% 
  mutate(width.boot = abs(boot.ci.high - boot.ci.low), width.regr = abs(regr.ci.high-regr.ci.low)) %>%
  mutate(wider = (width.boot-width.regr)/width.boot)
hist(results$wider)
mean(results$wider)

results <- results %>%
  mutate(t=estimate/se) %>%
  mutate(p=2 * (1 - pnorm(abs(t))))


results %>% filter(boot.ci.low > 0 | boot.ci.high < 0) %>% filter(p_original > 0.05)
