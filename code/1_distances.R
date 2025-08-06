library(sf)
library(tidyverse)
# do not need - setwd('/Users/bellagarfield/Desktop/THESIS2024/Preliminary R tests/')
#points <- read.csv("data/SealbeachesQGIS - Sheet2.csv")
points <- na.omit(read_csv("data/SealbeachesQGIS - Sheet2.csv"))
later_pups <- readRDS("output/later_pups.rds") %>% 
  filter(pup_area != "APBN",#no location for location APBN, so we need to filter out these entries for both data frames 'later_pups' and 'pups_moms_df'
         first_pup != "APBN") 
pups_moms_df <- readRDS("output/pups_moms.RDS") %>% 
  filter(pup_area != "APBN",
         mom_area != "APBN") 

sf_seals <- st_as_sf(x = points, coords = c("Lat", "long"),
                     crs = 32610) #Map CRS - EPSG:32610
beach_distance <- st_distance(sf_seals, sf_seals) %>% # This is finding distances between beaches
  as_tibble() %>% # convert to a tibble
  set_names(nm = points$Beach) %>% # Set column names to beaches
  mutate(Beach = as.character(points$Beach), .) %>% # calculate distance between each beach combo
  relocate(Beach) %>% # This moves the Beach column to be the first column
  pivot_longer(-Beach,names_to = "Beach2", values_to = "Distance") #creates 3-column format


beach_distance <- beach_distance %>% 
  rename(mom_area = Beach, pup_area = Beach2)
saveRDS(beach_distance, "output/beach_distance.RDS") #export as an RDS

pupmom_dist <- left_join(x = pups_moms_df, y = beach_distance, by = c("mom_area", "pup_area"))
saveRDS(pupmom_dist, "output/pupmom_dist.RDS")
#------------- Beach_distance.2 is assigning distances from first pup, not natal ----------
beach_distance.2 <- beach_distance %>%
  rename(pup_area = mom_area, first_pup = pup_area)
firstpup_dist <- left_join(x = later_pups, y = beach_distance.2, by = c("pup_area", "first_pup"))

firstpup_dist.filtered <- firstpup_dist %>%
  group_by(momID) %>%
  filter(n() >= 2) %>%
  ungroup()

saveRDS(firstpup_dist.filtered, "output/firstpup_dist.filtered.RDS")
write.csv(firstpup_dist.filtered, "output/firstpup_dist.filtered.csv")

