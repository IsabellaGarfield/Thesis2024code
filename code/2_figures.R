#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")
#install.packages("viridis")
#install.packages("ncdf4")
#install.packages("raster")
#install.packages("ggmap")
#install.packages("rgdal")
#install.packages("cowplot")
#install.packages("patchwork")
#install.packages("gridExtra")
#install.packages("png")
#install.packages("broom")
#install.packages("mapdata")
#install.packages("egg")

library(egg) #for ggarange
library(mapdata)
library(broom) #using to extract pvalue of linear regession line for age test
library(units)
library(tidyverse)
library(ggplot2)
library(raster)
library(ncdf4)
library(viridis)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggmap)
library(cowplot) #attempting to combine figures 
#library(rgdal) #ugh not available for this version of R
library(patchwork)
library(grid)
library(gridExtra)
library(png)
library(ggplot2)


# All reading RDS files and sourcing functions should go here
pupmom_dist <- readRDS("output/pupmom_dist.RDS")
beach_distance <- readRDS("output/beach_distance.RDS")
source("R/bootstrap.R")
Fig1_pupmom_dist <- readRDS("output/pupmom_dist.RDS")
age_dist <- read_csv("data/age_dist.csv") %>% 
  filter(pup_area != "APBN",
         mom_area != "APBN")
age_dist$mom_age = as.factor(age_dist$mom_age) 
first.pup <- read_csv("output/first_pup_dist.edited.csv") #excludes first pups in later pup column

# This is your bootstrapping function for natal-pupping distance: mean
observed_mean_dist <- mean(pupmom_dist$Distance) #Calculated actual mean value of observed distances = 395
null_mean_dist <- replicate(n = 10000, rand_mean_dist(pupmom_dist$pup_area,
                                                     pupmom_dist$mom_area,
                                                     beach_distance))#Shuffles mom area and pup area distances so randomized values are truly randomized
null_df <- data.frame(Distance = null_mean_dist)
null_df_mean <- mean(null_df$Distance)
Fig1_pupmom_dist_nounits <- Fig1_pupmom_dist %>%
  mutate(Distances_without_units = drop_units(Distance)) #Fig1_pupmom_dist has assigned unit for each value, while most other dataframes (such as null_df) only have values with unassigned units. In order to overlay both dataframes in the same graph without getting an error, units needed to be dropped. Fig1_pupmom_dist_nounits gives you the same distance values without units. Error solved!

#bootstrap functiton for first - later pupping distance
firstpup.observed_mean_dist <- mean(first.pup$Distance)
firstpup.null_mean_dist <- replicate(n = 10000, rand_mean_dist(first.pup$pup_area,
                                                            first.pup$first_pup,
                                                            beach_distance))
first.pup_null_df <- data.frame(Distance = firstpup.null_mean_dist)
firstpup.null_mean <- mean(first.pup_null_df$Distance)



# This is your bootstrapping function for natal-pupping distance: median

observed_median_dist <- median(pupmom_dist$Distance) #Calculated actual mean value of observed distances = 395
null_median_dist <- replicate(n = 10000, rand_median_dist(pupmom_dist$pup_area,
                                                      pupmom_dist$mom_area,
                                                      beach_distance))#Shuffles mom area and pup area distances so randomized values are truly randomized
null_mediandf <- data.frame(Distance = null_median_dist)
null_df_median <- median(null_mediandf$Distance)
Fig1.5_pupmom_dist_nounits <- Fig1_pupmom_dist %>%
  mutate(Distances_without_units = drop_units(Distance)) #Fig1_pupmom_dist has assigned unit for each value, while most other dataframes (such as null_df) only have values with unassigned units. In order to overlay both dataframes in the same graph without getting an error, units needed to be dropped. Fig1_pupmom_dist_nounits gives you the same distance values without units. Error solved!

#bootstrap functiton for first - later pupping distance
firstpup.observed_median_dist <- median(first.pup$Distance)
firstpup.null_median_dist <- replicate(n = 10000, rand_median_dist(first.pup$pup_area,
                                                               first.pup$first_pup,
                                                               beach_distance))
first.pup_null_mediandf <- data.frame(Distance = firstpup.null_median_dist)
firstpup.null_median <- mean(first.pup_null_mediandf$Distance)










#-------Fig 1: combining actual vs. bootstrapped distance of natal vs. pupping sites#####

fig.1 <- ggplot() +
  geom_density(data = Fig1_pupmom_dist_nounits, aes(x = Distances_without_units, fill = "Observed Distances "), alpha = 0.5,
               na.rm = FALSE,
               orientation = NA,
               show.legend = T) +
  geom_density(data = null_df, aes(x = Distance, fill = "Null Distribution"), alpha = 0.5) +
  geom_vline(xintercept = as.numeric(observed_mean_dist), 
             linewidth = 0.5, color = "orange3", linetype = "dashed", show.legend = TRUE) + #adds vertical line depicting mean value of observed data
  geom_text(aes(x = 320, label="395m", y=0.011), colour="orange3", angle=0, text= element_text(size = 9)) + #adds vline annotation
  geom_vline(xintercept = as.numeric(null_df_mean),
             linewidth = 0.5, color = "blue3", linetype = "dashed", show.legend = T) +
  geom_text(aes(x=700, label = "613m", y=0.011), colour = "blue3", angle = 0, text = element_text(size = 9)) +
  labs(x = "Distance (Meters) between natal and pupping sites: Mean values", y = "Density") +#x, y axis titles and figure title
  theme_classic(base_size = 15) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.012)) + #sets limits for y axis
  scale_x_continuous(expand = c(0, 0), limits = c(0, 2050)) + #this line is cutting off the 2000
  scale_fill_manual(values = c("blue", "orange")) + #organized for legend
  guides(fill = guide_legend(title = "Distribution Type:"), 
         linetype = guide_legend(title = "Mean Distance")) +
  theme(legend.position = c(.8,.8))  # Moves the legend to the bottom of the plot

fig.1
max(Fig1_pupmom_dist_nounits$Distance)

ggsave("figs/fig_1.png", plot = fig.1, width = 220, height = 120, units = "mm", dpi = 700)
ggsave("figs/fig_1.resized.png", plot = fig.1, width = 290, height = 140, units = "mm", dpi = 700)

#----------fig. 1.5, median

fig.1.5 <- ggplot() +
  geom_density(data = Fig1.5_pupmom_dist_nounits, aes(x = Distances_without_units, fill = "Observed Distances "), alpha = 0.5,
               na.rm = FALSE,
               orientation = NA,
               show.legend = T) +
  geom_density(data = null_mediandf, aes(x = Distance, fill = "Null Distribution"), alpha = 0.5) +
  geom_vline(xintercept = as.numeric(observed_median_dist), 
             linewidth = 0.5, color = "orange3", linetype = "dashed", show.legend = TRUE) + #adds vertical line depicting mean value of observed data
  geom_text(aes(x = 200, label="272m", y=0.011), colour="orange3", angle=0, text= element_text(size = 9)) + #adds vline annotation
  geom_vline(xintercept = as.numeric(null_df_median),
             linewidth = 0.5, color = "blue3", linetype = "dashed", show.legend = T) +
  geom_text(aes(x=600, label = "513m", y=0.011), colour = "blue3", angle = 0, text = element_text(size = 9)) +
  labs(x = "Distance (Meters) between natal and pupping sites: Median values", y = "Density") +#x, y axis titles and figure title
  theme_classic(base_size = 15) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.012)) + #sets limits for y axis
  scale_x_continuous(expand = c(0, 0), limits = c(0, 2050)) + #this line is cutting off the 2000
  scale_fill_manual(values = c("blue", "orange")) + #organized for legend
  guides(fill = guide_legend(title = "Distribution Type:"), 
         linetype = guide_legend(title = "Mean Distance")) +
  theme(legend.position = c(.8,.8))  # Moves the legend to the bottom of the plot

fig.1.5
max(Fig1.5_pupmom_dist_nounits$Distance)

fig1.combined <- ggarrange(
  fig.1, fig.1.5,
  ncol = 1,
  labels = c("A", "B")
)

ggsave("figs/fig_1.5.png", plot = fig.1.5, width = 220, height = 120, units = "mm", dpi = 700)
ggsave("figs/fig1.combined.png", plot = fig1.combined, width = 290, height = 290, units = "mm", dpi = 700)

###Figure 3 linear regression plot####

Fig2.dataframe <- age_dist %>% 
  group_by(mom_age)

age.lm <- lm(Distance ~ mom_age, Fig2.dataframe)
summary(age.lm)$p.value
tidy(age.lm)
glance(age.lm)$p.value


Fig.2 <-
  ggplot(Fig2.dataframe, aes(x = mom_age, y = Distance)) +
  geom_point() +
  geom_smooth(aes(x = as.numeric(mom_age), y = Distance), method = 'lm', color = 'red3', linetype = 2) +
  theme_classic(base_size = 15) +
  labs(x = "Mother Age (Years)", y = "Beach Distance (Meters) between
       natal and pupping sites") +
  geom_text(aes(x=1, label="n = 2", y=2080), colour="gray50", angle=0) +
  geom_text(aes(x=2, label="n = 28", y=2080), colour="gray50", angle=0) +
  geom_text(aes(x=3, label="n = 24", y=2080), colour="gray50", angle=0) +
  geom_text(aes(x=4, label="n = 28", y=2080), colour="gray50", angle=0) +
  geom_text(aes(x=5, label="n = 19", y=2080), colour="gray50", angle=0) +
  geom_text(aes(x=6, label="n = 12", y=2080), colour="gray50", angle=0) +
  geom_text(aes(x=7, label="n = 6", y=2080), colour="gray50", angle=0) +
  geom_text(aes(x=8, label="n = 3", y=2080), colour="gray50", angle=0) +
  geom_text(aes(x=9, label="n = 2", y=2080), colour="gray50", angle=0) 

Fig.2
ggsave("Final figs/fig_2.png", plot = Fig.2, width = 220, height = 120, units = "mm", dpi = 700)
#-----Fig. 3: distance between first pupping location and later pupping location #########
# note: first_pup_dist.edited.csv was manually edited to eliminate 0 values from first pups, no code was written to do this
fig.3 = ggplot() +
  geom_density(data = first.pup, aes(x = Distance, fill = "Observed Distances"), alpha = 0.5,
               na.rm = FALSE,
               orientation = NA,
               show.legend = T) +
  geom_density(data = first.pup_null_df, aes(x = Distance, fill = "Null Distribution"), alpha = 0.5) +
  coord_cartesian(xlim = c(0,1550), ylim = c()) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.012)) + #sets limits for y axis
  scale_x_continuous(expand = c(0, 0)) + #this is screwing up the x axis a bit
  labs(
       x = "Distance (Meters) between first pupping site and later pupping sites: Mean values",
       y = "Density") +
  theme_classic(base_size = 15) +
  scale_fill_manual(values = c("blue", "seagreen")) + #organized for legend
  guides(fill = guide_legend(title = "Distribution Type:"), 
         linetype = guide_legend(title = "Mean Distance")) +
  theme(legend.position = c(.8,.8)) + # Moves the legend to the bottom of the plot
  geom_vline(xintercept = as.numeric(firstpup.null_mean), 
             linewidth = 0.5, color = "blue", linetype = "dashed", show.legend = TRUE) + 
  geom_text(aes(x = 740, label="645m", y = 0.009), colour="blue3", angle=0, text=element_text(size = 9)) +
  geom_vline(xintercept = as.numeric(firstpup.observed_mean_dist), 
             linewidth = 0.5, color = "seagreen", linetype = "dashed", show.legend = TRUE) + #adds vertical line depicting mean value of observed data
  geom_text(aes(x = 430, label="490m", y = 0.009), colour="seagreen", angle=0, text=element_text(size = 9)) #adds vline annotation

fig.3
ggsave("figs/fig_3.png", plot = fig.3, width = 220, height = 120, units = "mm", dpi = 700)

#mode for fig 3?
d <- density(first.pup$Distance)

mode.fig3 <- function(d){
  i <- which(diff(sign(diff(d$y))) < 0) + 1
  data.frame(x = d$x[i], y = d$y[i])
}

mode.fig3(d)
#mode = 131m

#stats:
max(first.pup$Distance) #max value
mean(first.pup$Distance) #mean value
mean(firstpup.null_mean_dist)


#-----Fig. 3: distance between first pupping location and later pupping location: Median #########

fig.3.5 = ggplot() +
  geom_density(data = first.pup, aes(x = Distance, fill = "Observed Distances"), alpha = 0.5,
               na.rm = FALSE,
               orientation = NA,
               show.legend = T) +
  geom_density(data = first.pup_null_mediandf, aes(x = Distance, fill = "Null Distribution"), alpha = 0.5) +
  coord_cartesian(xlim = c(0,1550), ylim = c()) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.012)) + #sets limits for y axis
  scale_x_continuous(expand = c(0, 0)) + #this is screwing up the x axis a bit
  labs(
    x = "Distance (Meters) between first pupping site and later pupping sites: Median values",
    y = "Density") +
  theme_classic(base_size = 15) +
  scale_fill_manual(values = c("blue", "seagreen")) + #organized for legend
  guides(fill = guide_legend(title = "Distribution Type:"), 
         linetype = guide_legend(title = "Median Distance")) +
  theme(legend.position = c(.8,.8)) + # Moves the legend to the bottom of the plot
  geom_vline(xintercept = as.numeric(firstpup.null_median), 
             linewidth = 0.5, color = "blue", linetype = "dashed", show.legend = TRUE) + 
  geom_text(aes(x = 640, label="555m", y = 0.009), colour="blue3", angle=0, text=element_text(size = 9)) +
  geom_vline(xintercept = as.numeric(firstpup.observed_median_dist), 
             linewidth = 0.5, color = "seagreen", linetype = "dashed", show.legend = TRUE) + #adds vertical line depicting mean value of observed data
  geom_text(aes(x = 230, label="299m", y = 0.009), colour="seagreen", angle=0, text=element_text(size = 9)) #adds vline annotation

fig.3.5
ggsave("figs/fig_3.5.png", plot = fig.3.5, width = 220, height = 120, units = "mm", dpi = 700)


fig3.combined <- ggarrange(
  fig.3, fig.3.5,
  ncol = 1,
  labels = c("A", "B")
)

ggsave("figs/fig3.combined.png", plot = fig3.combined, width = 290, height = 290, units = "mm", dpi = 700)

#------Maps: Figure 4 and 5 --------
#'points' has lat, long for each beach name
# the following chunk creates two separate data frames for natal beaches (first) and pupping beaches (second) to be used in map plots
mom_area_count <- pupmom_dist %>% 
  group_by(mom_area) %>% 
  summarise(n = n()) #counts for each mom_area beach


points2 <- read.csv("data/SealbeachesQGISupdated - Sheet1.csv")

mom_area_points <- points2 %>% 
  rename(mom_area = Beach) #df copy of points with 'Beaches' renamed to 'mom_area so it can be left-joined appropriately  

mom_spatialmap_df <- left_join(x = mom_area_count, y = mom_area_points,
                                 by = c("mom_area")) #assigned lat/long based on 'mom_area'
    

saveRDS(mom_spatialmap_df, "output/mom_spatialmap_df.RDS")

# -- end mom data frame --

#the following is creating the pup data frame
pup_area_count <- pupmom_dist %>% 
  group_by(pup_area) %>% 
  summarise(n = n()) #counts for each pup_area beach

pup_area_points <- points2 %>% 
  rename(pup_area = Beach)

pup_spatialmap_df <- left_join(x = pup_area_count, y = pup_area_points,
                               by = c("pup_area"))

saveRDS(pup_spatialmap_df, "output/pup_spatialmap_df.RDS")

# -- end pup data frame --
# --- attempting a joint df with paired mom/pup lat/long###########

  # First join for pup coordinates
pups_with_pup_coords <- pups_moms_df %>%
  left_join(points2, by = c("pup_area" = "Beach")) %>%
  rename(pup_long = Long, pup_lat = Lat)

# Second join for mom coordinates
pups_moms_with_coords <- pups_with_pup_coords %>%
  left_join(points2, by = c("mom_area" = "Beach")) %>%
  rename(mom_long = Long, mom_lat = Lat)

pups_moms_with_coords$Easting.x <- NULL
pups_moms_with_coords$Northing.x <- NULL
pups_moms_with_coords$Easting.y <- NULL
pups_moms_with_coords$Northing.y <- NULL

pups_moms_with_coords
write.csv(pups_moms_with_coords, "output/pups_moms_with_coords.csv")

#----Fig. 4: map plot for natal (using mom_spatialmap_df) -------
register_google(key = "AIzaSyCl1RSEvMi3xDbt8uXZWmW73NCvQH86iuw", write = TRUE)
#mom map
mommap_google <- get_googlemap(center = c(-122.332, 37.1178), zoom = 16, maptype = 'satellite') %>% 
  ggmap() + 
  geom_point(data = mom_spatialmap_df, 
             mapping = aes(x = Long, y = Lat, color = n, size = n, fill = n), 
             shape = 21, color = 'white', stroke = 0.8) + 
  scale_size_continuous(range = c(0, 8), limits = c(0, 16)) + 
  scale_color_gradient(low = "cyan", high = "blue4", limits = c(0, 16)) +  # Continuous color gradient
  scale_fill_gradient(low = "cyan", high = "blue4", limits = c(0, 16)) +   # Continuous fill gradient
  #guides(size = "none", 
  #       fill = guide_colorbar(title = 'Number of Seals')) + 
  guides(fill = guide_legend(), size = guide_legend()) +
  theme_classic(base_size = 16) +
  labs(title = "Natal locations",
    x = "Longitude",
       y = "Latitude") +
  annotate("rect", xmin = -122.3388, xmax = -122.3366, ymin = 37.1174, ymax = 37.12322, alpha = 0.13, color = "white") +
  annotate("rect", xmin = -122.3366, xmax = -122.330, ymin = 37.1156, ymax = 37.1184, alpha = 0.13, color = "white") +
  annotate("rect", xmin = -122.3305, xmax = -122.3252, ymin = 37.1156, ymax = 37.1126, alpha = 0.13, color = "white") +
  geom_text(aes(x=-122.335, label="Northern", y=37.1212), colour="white", angle=0) +
  geom_text(aes(x=-122.3332, label="Central", y=37.1188), colour="white", angle=0) +
  geom_text(aes(x = -122.332, label = "Southern", y = 37.1140), colour = "white", angle = 0)
dev.off()
mommap_google


#pup map

pupmap_google <- get_googlemap(center = c(-122.332, 37.1178), zoom = 16, maptype = 'satellite') %>% 
  ggmap() + 
  geom_point(data = pup_spatialmap_df, 
             mapping = aes(x = Long, y = Lat, color = n, size = n, fill = n), 
             shape = 21, color = 'white', stroke = 0.8) + 
  scale_size_continuous(range = c(0, 8), limits = c(0, 16)) + 
  scale_color_gradient(low = "cyan", high = "blue4", limits = c(0, 16)) +  # Continuous color gradient
  scale_fill_gradient(low = "cyan", high = "blue4", limits = c(0, 16)) +   # Continuous fill gradient
  guides(fill = guide_legend(), size = guide_legend()) + 
  theme_classic(base_size = 15) +
  labs(title = "Pupping locations",
    x = "Longitude",
       y = "Latitude") +
  annotate("rect", xmin = -122.3388, xmax = -122.3366, ymin = 37.1174, ymax = 37.12322, alpha = 0.13, color = "white") +
  annotate("rect", xmin = -122.3366, xmax = -122.330, ymin = 37.1156, ymax = 37.1184, alpha = 0.13, color = "white") +
  annotate("rect", xmin = -122.3305, xmax = -122.3252, ymin = 37.1156, ymax = 37.1126, alpha = 0.13, color = "white") +
  geom_text(aes(x=-122.335, label="Northern", y=37.1212), colour="white", angle=0) +
  geom_text(aes(x=-122.3332, label="Central", y=37.1188), colour="white", angle=0) +
  geom_text(aes(x=-122.332, label = "Southern", y = 37.1140), colour = "white", angle = 0)
dev.off()

pupmap_google

Fig.googlemap <- plot_grid(mommap_google, pupmap_google, labels = 'AUTO', rel_widths = c(6,6))
Fig.googlemap
ggsave("figs/Fig.googlemap.png", plot = Fig.googlemap, width = 11, height = 9, dpi = 2000)

#### Alluvial Plot ####
ogdat_sankey <- read_csv("data/ggsankey_format_ogdat.csv")

ogdat_sankey_ordered = ogdat_sankey %>%
  mutate(mom_area = factor(mom_area,
                           levels = c("NP5", "NP4", "NPG4", "NP3", "NPG3", "NP2/3", "NP2", "NPG2", "NP1", "NPG1", "NP0", "NP", "NPG", "BBN", "BBNS", "MBBU", "MBB", "BMB", "BM", "BMC", "BMS", "BBS", "BBSU", "BBSL", "AP", "APG", "TSW", "SBW", "SBE", "TSC", "TSE", "TSB", "TSBE", "P1")),
         pup_area = factor(pup_area,
                           levels = c("NP5", "NP4", "NP3", "NPG3", "NP2", "NPG2", "NP1", "NPG1", "NP0", "NPG", "NP", "BBN", "BBNS", "MBBU", "BMB", "BMC", "BMS", "BBS", "BBSU", "BBSL", "AP", "APG", "TSW", "SBW", "SBE", "TSC", "TSB", "TSBE", "P1")),
         mom_region=factor(mom_region,levels=c("Northern","Central","Southern"))) 



# Beach Plot #
beach.plot <- ggplot(as.data.frame(ogdat_sankey_ordered),
                     aes(y = n_area, axis1 = as.factor(mom_area), axis2 = as.factor(pup_area))) +
  geom_alluvium(aes(fill = as.factor(mom_region)), width = 1/12) +
  geom_stratum(width = 1/12, fill=NA,color = "grey20") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)),
             nudge_x = rep(c(-.05,.05),29)[1:57],  #this line is new - RSB
             hjust=rep(c(1,0),29)[1:57], #this line is new - RSB
             size = 2,
             fill=NA,label.size=0) + 
  scale_x_discrete(limits = c("Mom Birth 
Beach", "Pup Birth 
Beach"),
                   expand = c(.1, .1)) + #this line needs to be changed to accomodate the width of the labels - RSB
  labs(x="",y="# Seals",fill="Mom's Birth Region")+
  scale_fill_manual(values=c("lightskyblue2","cyan4","grey79"))+
  theme_few() +
  theme(axis.text = element_text(size = 12))

beach.plot
ggsave("Bella 2024_11_17.png",height=7,width=5)

# Region Plot #

ogdat_sankey_ordered_region=ogdat_sankey_ordered %>% 
  group_by(mom_region,pup_region) %>%
  summarize(n=sum(n_area))

region.plot <- ggplot(as.data.frame(ogdat_sankey_ordered_region),
                      aes(y = n, axis1 = as.factor(mom_region), axis2 = as.factor(pup_region))) +
  geom_alluvium(aes(fill = as.factor(mom_region)), width = 1/12) +
  geom_stratum(width = 1/12, fill=NA,color = "grey20") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4,
            fill=NA,angle=90) +
  scale_x_discrete(limits = c("Mom Birth 
Region", "Pup Birth 
Region"), expand = c(.05, .05)) +
  labs(x="", y = "", fill="Mom's Birth Region")+
  scale_fill_manual(values=c("lightskyblue2","cyan4","grey79"))+
  theme_few() +
  theme(axis.text = element_text(size = 12))
region.plot
ggsave("Bella Region 2024_11_17.png",height=7,width=5)


combined.plot.2 <- ggarrange(beach.plot, region.plot, ncol=2, common.legend = TRUE, legend = "bottom")
combined.plot.2
ggsave("combined.plot.2.png", height = 6, width = 7, dpi=900)


with(ggsankey_format_ogdat,table(mom_region,pup_region))
with(ogdat_sankey_ordered, table(mom_region, pup_region))


#Google earth sand dune images

#impor Google earth images
ano2021 <- readPNG("figs/2021ano.png")
ano2018 <- readPNG("figs/2018Ano.png")
ano2023 <- readPNG("figs/2023Ano.png")

#turning google earth images into graphical objects
g2021 <- rasterGrob(ano2021, interpolate=TRUE)
g2018 <- rasterGrob(ano2018, interpolate=TRUE)
g2023 <- rasterGrob(ano2023, interpolate=TRUE)

# Create labels
labelA <- textGrob("A", x = unit(0.08, "npc"), y = unit(0.95, "npc"), just=c("left", "top"), gp=gpar(fontsize=50, fontface = "bold"))
labelB <- textGrob("B", x = unit(0.08, "npc"), y = unit(0.95, "npc"), just=c("left", "top"), gp=gpar(fontsize=50, fontface = "bold"))
labelC <- textGrob("C", x = unit(0.08, "npc"), y = unit(0.95, "npc"), just=c("left", "top"), gp=gpar(fontsize=50, fontface = "bold"))


# Combine labels and images

g2018_labeled <- gTree(children=gList(g2018, labelA))
g2021_labeled <- gTree(children=gList(g2021, labelB))
g2023_labeled <- gTree(children=gList(g2023, labelC))

# Arrange the images in a grid
Anogullychange <- grid.arrange(g2018_labeled, g2021_labeled, g2023_labeled, ncol=3)

png("Anogullychange.png", width=2000, height=1000)
grid.draw(Anogullychange)
dev.off()
