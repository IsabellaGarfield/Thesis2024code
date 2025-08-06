# This code will match known pups and moms with their first location sighted.
library(tidyverse)

# Read in the data
#resights <- read.csv("data/belladatapull2013_09_15.csv")
resights <- read_csv("data/Garfield_datapull.csv")

# This function works on one animal (the group) and finds the pup site based on
# the most frequent area (location) and earliest date (observed)
find_pup_site <- function(location, observed) {
  # Create a frequency table with earliest observation
  loc_freq <- tibble(location, observed) %>%
    group_by(location) %>%
    summarize(n = n(),
              first_seen = min(observed)) %>%
    filter(n == max(n)) %>%                    # choose most frequent site
    filter(first_seen == min(first_seen)) %>%  # break ties by earliest sighting
    slice(1)                                   # still tied? just choose one
  # Return the estimated pup site
  loc_freq$location
}

# Pups with known moms ----------------------------------------------------

pup_obsage <- c('p', 'P', 'pup', 'Pup')
pups <- resights %>%
  filter(obsage %in% pup_obsage,
         momID != 0) %>%
  group_by(pupID = animalID, momID) %>%
  summarize(pup_area = find_pup_site(area, date),
            .groups = "drop")


# Moms of pups ------------------------------------------------------------

moms <- resights %>%
  semi_join(pups, by = c(animalID = "momID")) %>%
  filter(obsage %in% pup_obsage) %>%
  group_by(momID = animalID) %>%
  summarize(mom_area = find_pup_site(area, date))


# Pups and moms together --------------------------------------------------

pups_moms <- pups %>%
  right_join(moms, by = "momID")

saveRDS(pups_moms, "output/pups_moms.rds")

# distances based on first pup loc instead of natal-------------------------------------

first_pup <- pups_moms %>% 
  arrange(momID,pupID) %>% 
  slice_head(by=momID)

later_pups <- left_join(x = pups_moms, y = first_pup, by = 'momID')

later_pups <- later_pups %>%
  select(-c(pupID.y, mom_area.y, mom_area.x)) %>% 
  rename(pup_area = pup_area.x, 
         first_pup = pup_area.y,
         pupID = pupID.x)

saveRDS(later_pups, "output/later_pups.RDS")

# distances based on previous pupping site

pups_moms_season <- pups_moms %>%
  mutate(pup_season = resights$season, by = pupID)

