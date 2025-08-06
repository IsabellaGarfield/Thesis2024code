library(tidyverse)
theme_set(theme_bw())
set.seed(1234)

# distance ~ Gamma(mean, dispersion)
# mean = beta0 + beta1 * log(lag)
# i.e., identity link, log transformed lag
beta0 <- 50
beta1 <- 50
dispersion <- 0.1

# Simulate 10,000 seals
seal_dist <- expand_grid(
  seal_id = 1:1e4,
  lag = 1:8
) %>%
  mutate(
    distance_expected = beta0 + beta1 * log(lag),
    distance = rgamma(n(),
                      shape = 1 / dispersion,
                      scale = distance_expected * dispersion)
  )
seal_dist_summ <- seal_dist %>%
  group_by(lag) %>%
  summarise(
    distance_mean = mean(distance),
    distance_lwr = quantile(distance, 0.025),
    distance_upr = quantile(distance, 0.975)
  )

# Visualize
ggplot(seal_dist_summ, aes(lag, distance_mean)) +
  geom_pointrange(aes(ymin = distance_lwr, ymax = distance_upr)) +
  geom_line(aes(y = distance_expected),
            distinct(seal_dist, lag, distance_expected),
            color = "firebrick") +
  expand_limits(y = 0)

# Fit model
dist_mod <- glm(distance ~ log(lag),
                data = seal_dist,
                family = Gamma(link = "identity"),
                start = c(1, 0))
summary(dist_mod)

# Sanity check: simulation parameters should fall in 95% CI of estimates
est_beta <- coef(summary(dist_mod))[1:2, 1:2] %>%
  as_tibble() %>%
  set_names(c("estimate", "stderr")) %>%
  mutate(
    param = c("beta0", "beta1"),
    actual = c(beta0, beta1),
    lower = estimate - 1.96 * stderr,
    upper = estimate + 1.96 * stderr
  )
ggplot(est_beta, aes(param, estimate)) +
  geom_pointrange(aes(y = estimate, ymin = lower, ymax = upper),
                  shape = 21, size = 1) +
  geom_point(aes(y = actual), color = "firebrick", size = 2)
