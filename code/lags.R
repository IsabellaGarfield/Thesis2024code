library(tidyverse)

#calculating lags from resights

pups_moms <- read_rds("output/pups_moms.rds")
beach_distances <- read_rds("output/beach_distance.RDS")

lags_distances <- pups_moms %>% 
  group_by(momID) %>% 
  group_modify(\(.rows, .groups) {
    cross_join(.rows, .rows) %>% 
      filter(season.x < season.y) %>% 
      mutate(lag = season.y - season.x) %>% 
      select(pup_area.x, pup_area.y, lag)
  }) %>% 
  left_join(beach_distances, by = c(pup_area.x = "mom_area", pup_area.y = "pup_area"))

ggplot(lags_distances, aes(factor(lag), Distance)) +
  geom_boxplot()

neg_log_lik <- function(beta, X, y) {
  mu <- X %*% beta
  if (any(mu <= 0)) return(Inf)  # penalize invalid values
  nll <- -sum(dgamma(y, shape = 1/phi, scale = mu * phi, log = TRUE))  # phi = dispersion
  return(nll)
}

lag_mod <- constrOptim(
  theta = c(1, 1),
  f = neg_log_lik,
  grad = NULL