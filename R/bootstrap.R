# Bella TODO: aadd some comments here so you remember what's going on
rand_mean_dist <- function(beach_1, beach_2, beach_dist) {
  # Shuffle the two vectors of beach names
  beach_1_shuffled <- sample(beach_1, length(beach_1))
  beach_2_shuffled <- sample(beach_2, length(beach_2))
  # Join the shuffled beaches to the distance matrix
  # The disance matrix has columns for "mom_area" and "pup_area"
  tibble(mom_area = beach_1_shuffled,
         pup_area = beach_2_shuffled) %>% 
    left_join(beach_dist, by = c("mom_area", "pup_area")) %>% 
    summarize(dist = mean(Distance)) %>% 
    pull(dist)
}

rand_median_dist <- function(beach_1, beach_2, beach_dist) {
  # Shuffle the two vectors of beach names
  beach_1_shuffled <- sample(beach_1, length(beach_1))
  beach_2_shuffled <- sample(beach_2, length(beach_2))
  # Join the shuffled beaches to the distance matrix
  # The disance matrix has columns for "mom_area" and "pup_area"
  tibble(mom_area = beach_1_shuffled,
         pup_area = beach_2_shuffled) %>% 
    left_join(beach_dist, by = c("mom_area", "pup_area")) %>% 
    summarize(dist = median(Distance)) %>% 
    pull(dist)
}
