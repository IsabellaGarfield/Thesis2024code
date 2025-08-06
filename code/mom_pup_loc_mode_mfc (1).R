# This code will match known pups and moms with their first location sighted.


library(tidyverse)

# Read in the data
# resights <- read.csv("~/Documents/UCSC/Bella Thesis/Data/belladatapull2013_09_15.csv")
resights <- read_csv("data/belladatapull2013_09_15.csv")

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


###### IGNORE STUFF BELOW HERE FOR NOW ######

 # Insert a colum that shows the pups first location
known_moms_df$firstloc_pup <- final_pup_df$birth_loc[match(final_mom_df$birth_loc,final_pup_df)]

final_df <- known_moms_df %>%



#the location a pup was first re-sighted with its known mom
#find first sighting of each animalID
#Roxanne's code:
# ~~~~~dat=dat[dat$momID!=0,] #get rid of pups with no moms
# ~~~~~first=match(unique(dat$animalID), dat$animalID)  #pull out index values of first observation



###Set breeding parameters#(fix code, does not work)
#animalID=animalID %>%
 # mutate(proceduredoy=yday(as.Date(sighting_datetime))) %>%
  ###between Dec 1 and Mar 15
#  mutate(timeofyear=if_else(proceduredoy>274|proceduredoy<74,"Breeding",
                            ###between Mar 15 and July 1
                        #    if_else(proceduredoy>=74&proceduredoy<182,"Molt","Other")))



test <- aggregate(animalID~area,data=resights_sample, FUN=max)





 test <- resights_sample %>%
   group_by(animalID,area) %>%
   summarise()

dat$test <- fct_collapse(test$area, '1' = c("NP6", "NP5", "NP4", "NPG4", "NPD"),
                              '2' = c("NP3", "NP2", "NP1", "NPG1", "NP0", "NPG0", "NP", "NPGa", "NPC"),
                             '3' = c("BBN", "BBNN", "BBNS", "MBBL", "MBBU", "MBB"),
                             '4' = c("BMB", "BMNN", "BMD", "BMN", "BMC", "BM", "BMS", "BBSU"),
                             '5' = c("BBSL", "BBS", "AP", "SBW", "SBE", "TSW", "APGw", "APG"),
                             '6' = c("TSC", "TS", "TSE", "TSB", "FSB", "P1"))

dat$mom_area_2 <-  fct_collapse(resights$area, '1' = c("NP6", "NP5", "NP4", "NPG4", "NPD"),
                              '2' = c("NP3", "NP2", "NP1", "NPG1", "NP0", "NPG0", "NP", "NPGa", "NPC"),
                              '3' = c("BBN", "BBNN", "BBNS", "MBBL", "MBBU", "MBB"),
                              '4' = c("BMB", "BMNN", "BMD", "BMN", "BMC", "BM", "BMS", "BBSU"),
                              '5' = c("BBSL", "BBS", "AP", "SBW", "SBE", "TSW", "APGw", "APG"),
                              '6' = c("TSC", "TS", "TSE", "TSB", "FSB", "P1"))



#checking
dat[First,"area"] #how do I get mode to work#
test_mode=dat[Mode,] #datasets of first observations for each pup
subset(test,animalID==40796) #pup of 1885
subset(dat,momID==1561) #1885
subset(dat,momID==40796)

#####re-organize data for figure-making#########


# Extract 'animalID', 'area' as 'pup loc', and 'momID' from the original dataset
pup_mom_data_mode <- test %>%
  select(animalID, pup_loc = area, momID)

# Merge with itself to get 'mom loc'
pup_mom_data <- test %>%
  left_join(pup_mom_data, by = c("momID" = "animalID")) %>%
  select(pup_loc, mom_loc = area) #attempted to insert mode command here, took out#

# Remove rows with missing values in 'mom_loc'
pup_mom_data <- pup_mom_data %>%
  na.omit()
#organize locations
pup_mom_data <- pup_mom_data %>%
  mutate(
    pupsite_grouping = case_when(
      pup_loc %in% c("NP6", "NP5", "NP4", "NPG4", "NPD") ~ '1',
      pup_loc %in% c("NP3", "NP2", "NP1", "NPG1", "NP0", "NPG0", "NP", "NPGa", "NPC") ~ '2',
      pup_loc %in% c("BBN", "BBNN", "BBNS", "MBBL", "MBBU", "MBB") ~ '3',
      pup_loc %in% c("BMB", "BMNN", "BMD", "BMN", "BMC", "BM", "BMS", "BBSU") ~ '4',
      pup_loc %in% c("BBSL", "BBS", "AP", "SBW", "SBE", "TSW", "APGw", "APG") ~ '5',
      pup_loc %in% c("TSC", "TS", "TSE", "ISD", "TSB", "FSB", "P1") ~ '6',
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(pupsite_grouping =
           factor(pupsite_grouping,
                  levels = c("1", "2", "3", "4", "5", "6")
           )
  ) %>%
  mutate(
    momsite_grouping = case_when(
      mom_loc %in% c("NP6", "NP5", "NP4", "NPG4", "NPD") ~ '1',
      mom_loc %in% c("NP3", "NP2", "NP1", "NPG1", "NP0", "NPG0", "NP", "NPGa", "NPC") ~ '2',
      mom_loc %in% c("BBN", "BBNN", "BBNS", "MBBL", "MBBU", "MBB") ~ '3',
      mom_loc %in% c("BMB", "BMNN", "BMD", "BMN", "BMC", "BM", "BMS", "BBSU") ~ '4',
      mom_loc %in% c("BBSL", "BBS", "AP", "SBW", "SBE", "TSW", "APGw", "APG") ~ '5', #add location TS if that is refering to tarzans generally
      mom_loc %in% c("TSC", "TS", "TSE", "ISD", "TSB", "FSB", "P1") ~ '6',
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(momsite_grouping =
           factor(momsite_grouping,
                  levels = c("1", "2", "3", "4", "5", "6")
           )
  )

pup_mom_data_adjusted <- pup_mom_data %>%
  filter(complete.cases(pupsite_grouping, momsite_grouping)) #should mode go here?#

#####plot#####

# Create a matrix with the data
data_matrix <- table(pup_mom_data_adjusted$pupsite_grouping, pup_mom_data_adjusted$momsite_grouping)

# Get the unique binary values
unique_values <- sort(unique(c(as.numeric(rownames(data_matrix)), as.numeric(colnames(data_matrix)))))

# Reverse the order for "Pup Site Grouping"
factor_pupsite <- factor(unique_values, levels = rev(unique_values))

# Reorder the row and column names to match the desired order
data_matrix <- data_matrix[order(rownames(data_matrix)), order(colnames(data_matrix))]

# Create a heatmap using heatmap
heatmap(pups_moms,
        col = colorRampPalette(c("white", "blue"))(20),  # Choose a color palette
        xlab = "Pup location",
        ylab = "Mom location",
        main = "Pup return loc. vs. Mom birth loc.",
        Rowv = NA, Colv = NA,
        labRow = as.character(factor_pupsite),
        labCol = as.character(unique_values))
