library(tidyverse)
library(glmmTMB)
library(boot)

pups_moms <- read_rds("output/pups_moms.rds")
beach_distances <- read_rds("output/beach_distance.RDS") %>% 
  mutate(Distance = as.numeric(Distance, unit = "m"))

lags_distances <- pups_moms %>% 
  group_by(momID) %>% 
  group_modify(\(.rows, .groups) {
    cross_join(.rows, .rows) %>% 
      filter(season.x < season.y) %>% 
      mutate(lag = season.y - season.x) %>% 
      select(pup_area.x, pup_area.y, lag)
  }) %>% 
  ungroup() %>% 
  left_join(beach_distances, 
            by = c(pup_area.x = "mom_area", pup_area.y = "pup_area")) %>% 
  drop_na(Distance)

ggplot(lags_distances, aes(factor(lag), Distance)) +
  geom_boxplot() +
  theme_bw()

# Some of your Distances are 0's, meaning gamma won't work
# Trying zero-inflated gamma, using glmmTMB package

model_lags <- glmmTMB(Distance ~ log(lag) + (1 | momID),
                      ziformula = ~ lag + (1 | momID),
                      data = lags_distances,
                      family = ziGamma(link = "identity"))
summary(model_lags)

# Model predictions 
predictions_lags <- tibble(lag = 1:6) %>% 
  mutate(Distance = predict(model_lags, 
                            newdata = ., 
                            type = "response",
                            re.form = NA))

# Visualize
mean_distances <- lags_distances %>% 
  group_by(lag) %>% 
  summarise(Distance = mean(Distance))
ggplot(lags_distances, aes(factor(lag), Distance)) +
  geom_boxplot() +
  geom_line(aes(group = 1),
            data = predictions_lags, 
            color = "cornflowerblue", 
            linewidth = 1) +
  geom_point(data = mean_distances,
             color = "firebrick",
             size = 2) +
  theme_bw()

# The lag does not have a significant effect on the distance. This is largely because the mean (red dots) doesn't change very much! The median does seem to change a lot though. You can investigate that with quantile regression. 

library(quantreg)
model_lags_quantile <- rq(Distance ~ log(lag), 
                          tau = 0.5,
                          data = lags_distances)
summary(model_lags_quantile, se = "boot")
quant_preds <- tibble(lag = 1:6) %>% 
  mutate(Distance = predict(model_lags_quantile, newdata = .))
ggplot(lags_distances, aes(factor(lag), Distance)) +
  geom_boxplot() +
  geom_line(aes(group = 1),
            data = quant_preds, 
            color = "cornflowerblue", 
            linewidth = 1) +
  geom_point(data = mean_distances,
             color = "firebrick",
             size = 2) +
  theme_bw()

## This model suggests the median distance does increase with the lag. You don't have a significant p-value (0.063), so you can't say it's a significant relationship. But you can present the results and that will satisfy the reviewer.