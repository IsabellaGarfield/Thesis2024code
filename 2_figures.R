#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")
#install.packages("viridis")
#install.packages("ncdf4")
#install.packages("raster")
#install.packages("ggmap")
#install.packages("rgdal")
#install.packages("cowplot")
#install.packages("patchwork")
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
library(cowplot) #attempting to combinne figures 
#library(rgdal) #ugh not available for this version of R
library(patchwork)
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

# This is your bootstrapping function for natal-pupping distance
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
#-------Fig 1: combining actual vs. bootstrapped distance of natal vs. pupping sites#####

fig.1 <- ggplot() +
  geom_density(data = Fig1_pupmom_dist_nounits, aes(x = Distances_without_units, fill = "Observed Distances "), alpha = 0.5,
               na.rm = FALSE,
               orientation = NA,
               show.legend = T) +
  geom_density(data = null_df, aes(x = Distance, fill = "Null Distribution"), alpha = 0.5) +
  geom_vline(xintercept = as.numeric(observed_mean_dist), 
             linewidth = 0.5, color = "black", linetype = "dashed", show.legend = TRUE) + #adds vertical line depicting mean value of observed data
  geom_text(aes(x = 340, label = "Observed Mean = 395m", y = 0.007), colour = "orange3", angle = 90, text = element_text(size = 9)) + #adds vline annotation
  geom_vline(xintercept = as.numeric(null_df_mean),
             linewidth = 0.5, color = "black", linetype = "dashed", show.legend = T) +
  geom_text(aes(x = 525, label = "Null mean = 613m", y = 0.007), colour = "blue3", angle = 90, text = element_text(size = 9)) +
  labs(x = "Distance (Meters)", y = "Density", title = "Distance between natal and pupping sites") + #x, y axis titles and figure title
  theme_classic() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.012)) + #sets limits for y axis
  scale_x_continuous(expand = c(0, 0), limits = c(0, 2050)) + #this line is cutting off the 2000
  scale_fill_manual(values = c("blue", "orange")) + #organized for legend
  guides(fill = guide_legend(title = "Distribution Type:"), 
         linetype = guide_legend(title = "Mean Distance")) +
  theme(legend.position = c(.8,.8))  # Moves the legend to the bottom of the plot

fig.1
max(Fig1_pupmom_dist_nounits$Distance)

ggsave("figs/fig_1.png", plot = fig.1, width = 220, height = 120, units = "mm", dpi = 450)

#---------Fig. 2: Boxplots for female age#####
#note: age_dist was manually edited to evaluate mom age during pupping, no code was written to do this 



fig.2 = age_dist %>% 
  group_by(mom_age) %>% 
  ggplot(aes(x = mom_age, y = Distance)) +
  geom_boxplot(fill = "orange", alpha = 0.5) +
  scale_y_continuous(limits = c(0, 2100)) +
  theme_classic() +
  geom_smooth(method = "lm") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 6) + #adds mean diamonds in each box
  labs(x = "Mom Age (Years)", y = "Beach Distance (Meters)", title = "Distance from natal site to pupping site as a function of mom age") +
  geom_text(aes(x=1, label="n = 2", y=2080), colour="gray50", angle=0) +
  geom_text(aes(x=2, label="n = 28", y=2080), colour="gray50", angle=0) +
  geom_text(aes(x=3, label="n = 24", y=2080), colour="gray50", angle=0) +
  geom_text(aes(x=4, label="n = 28", y=2080), colour="gray50", angle=0) +
  geom_text(aes(x=5, label="n = 19", y=2080), colour="gray50", angle=0) +
  geom_text(aes(x=6, label="n = 12", y=2080), colour="gray50", angle=0) +
  geom_text(aes(x=7, label="n = 6", y=2080), colour="gray50", angle=0) +
  geom_text(aes(x=8, label="n = 3", y=2080), colour="gray50", angle=0) +
  geom_text(aes(x=9, label="n = 2", y=2080), colour="black", angle=0) 


fig.2
ggsave("figs/fig_2.png", plot = fig.2, width = 220, height = 120, units = "mm", dpi = 450)

fit = lm(Distance ~ mom_age, data = age_dist)
summary(fit)

#-----Fig. 3: distance between first pupping location and later pupping location #########
# note: first_pup_dist.edited.csv was manually edited to eliminate 0 values from first pups, no code was written to do this
fig.3 = ggplot() +
  geom_density(data = first.pup, aes(x = Distance, fill = "Observed Distances"), alpha = 0.5,
               na.rm = FALSE,
               orientation = NA,
               show.legend = T) +
  geom_density(data = first.pup_null_df, aes(x = Distance, fill = "Null Distribution"), alpha = 0.5) +
  coord_cartesian(xlim = c(0,1550), ylim = c()) +
  scale_y_continuous(expand = c(0, 0)) + #sets limits for y axis
  scale_x_continuous(expand = c(0, 0)) + #this is screwing up the x axis a bit
  labs(title = "Distance between first pupping site and later pupping site",
       x = "Distance (Meters)",
       y = "Density") +
  theme_classic() +
  scale_fill_manual(values = c("blue", "seagreen")) + #organized for legend
  guides(fill = guide_legend(title = "Distribution Type:"), 
         linetype = guide_legend(title = "Mean Distance")) +
  theme(legend.position = c(.8,.8)) + # Moves the legend to the bottom of the plot
  geom_vline(xintercept = as.numeric(firstpup.observed_mean_dist), 
             linewidth = 1, color = "black", linetype = "dashed", show.legend = TRUE) + #adds vertical line depicting mean value of observed data
  geom_text(aes(x = 460.69, label = "Observed Mean = 490m", y = 0.004), colour = "black", angle = 90, text = element_text(size = 9)) #adds vline annotation

fig.3

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

ggsave("figs/fig_3.png", plot = fig.3, width = 220, height = 120, units = "mm", dpi = 450)

#------Maps: Figure 4 and 5 --------
#'points' has lat, long for each beach name
# the following chunk creates two separate data frames for natal beaches (first) and pupping beaches (second) to be used in map plots
mom_area_count <- pupmom_dist %>% 
  group_by(mom_area) %>% 
  summarise(n = n()) #counts for each mom_area beach

mom_area_points <- points %>% 
  rename(mom_area = Beach) #df copy of points with 'Beaches' renamed to 'mom_area so it can be left-joined appropriately  

mom_spatialmap_df <- left_join(x = mom_area_count, y = mom_area_points,
                                 by = c("mom_area")) #assigned lat/long based on 'mom_area'

saveRDS(mom_spatialmap_df, "output/mom_spatialmap_df.RDS")

# -- end mom data frame --

#the following is creating the pup data frame
pup_area_count <- pupmom_dist %>% 
  group_by(pup_area) %>% 
  summarise(n = n()) #counts for each pup_area beach

pup_area_points <- points %>% 
  rename(pup_area = Beach)

pup_spatialmap_df <- left_join(x = pup_area_count, y = pup_area_points,
                               by = c("pup_area"))

saveRDS(pup_spatialmap_df, "output/pup_spatialmap_df.RDS")
#write.csv(pup_spatialmap_df, "output/pup_spatialmap_df.csv")

# -- end pup data frame --

#----Fig. 4: map plot for natal (using mom_spatialmap_df) -------

# make the map plot
fig.4 <- ggplot(data = mom_spatialmap_df) + 
  # pch is the point type, a hollow fillable circle
  geom_point(aes(x = long, y = Lat, fill = n, size = n, ylim = c()), pch = 21) + 
  #scale_fill_distiller(bquote(~bold("Legend Title \n(Change Me)")), # the \n splits the label into two lines
  #                     palette = "RdYlBu") + # this is a standard color palette for heat maps
  scale_size_continuous(range = c(0, 15)) +
  scale_fill_distiller(direction = -1, palette = "RdYlBu") +
  guides(fill = guide_legend(), size = guide_legend()) +
  theme_classic() + # this has to go before other theme calls otherwise it over rides them
  theme(legend.position = c(0.85, 0.35), legend.box.background = element_rect(color = "black", linewidth = .5)) + # adjust the legend position and other things like axis text size in this
  labs(title = "Map of natal sites in Año Nuevo Colony",
       x = "Easting",
       y = "Northing") +
  scale_y_reverse() +
  annotate("rect", xmin = 4107550, xmax = 4109000, ymin = 558935, ymax = 558600, alpha = 0.13, fill = "aquamarine4") +
  annotate("rect", xmin = 4107550, xmax = 4109000, ymin = 559520, ymax = 558935, alpha = 0.13, fill = "darkorange3") +
  annotate("rect", xmin = 4107550, xmax = 4109000, ymin = 560200, ymax = 559520, alpha = 0.13, fill = "gold3") +
  geom_text(aes(x = 4107500, label = "Northern", y = 558750), colour = "black", angle = 90) +
  geom_text(aes(x = 4107500, label = "Central", y = 559200), colour = "black", angle = 90) +
  geom_text(aes(x = 4107500, label = "Southern", y = 559820), colour = "black", angle = 90) +
  geom_text(aes(x = 4107723, label = "BBS", y = 559430), colour = "black", angle = 0) +
  geom_text(aes(x = 4108020, label = "BBN", y = 559010), colour = "black", angle = 0) +
  geom_text(aes(x = 4108120, label = "NP", y = 558800), colour = "black", angle = 0) +
  geom_label(aes(x = 4108500, label = "Mainland", y = 559600), colour = "black", angle = 0) 
  

fig.4
ggsave("figs/fig_4.png", plot = fig.4, width = 160, height = 120, units = "mm", dpi = 450)

#Fig. 5: map plot for pupping (using pup_spatialmap_df) -------
fig.5 <- ggplot(data = pup_spatialmap_df) + 
  # pch is the point type, a hollow fillable circle
  geom_point(aes(x = long, y = Lat, fill = n, size = n), pch=21) + 
  #scale_fill_distiller(bquote(~bold("Legend Title \n(Change Me)")), # the \n splits the label into two lines
  #                     palette = "RdYlBu") + # this is a standard color palette for heat maps
  scale_size_continuous(range = c(0, 15)) + #should this be out of 15 or 16?
  scale_fill_distiller(direction = -1, palette = "RdYlBu") +
  guides(fill = guide_legend(), size = guide_legend()) +
  theme_classic() +# this has to go before other theme calls otherwise it over rides them
  theme(legend.position = c(0.85, 0.35), legend.box.background = element_rect(color = "black", linewidth = .5)) + # adjust the legend position and other things like axis text size in this
  labs(title = "Map of pupping sites in Año Nuevo Colony",
       x = "Easting",
       y = "Northing") +
  scale_y_reverse() +
  annotate("rect", xmin = 4107550, xmax = 4109000, ymin = 558935, ymax = 558600, alpha = 0.13, fill = "aquamarine4") +
  annotate("rect", xmin = 4107550, xmax = 4109000, ymin = 559520, ymax = 558935, alpha = 0.13, fill = "darkorange3") +
  annotate("rect", xmin = 4107550, xmax = 4109000, ymin = 560200, ymax = 559520, alpha = 0.13, fill = "gold3") +
  geom_text(aes(x = 4107500, label = "Northern", y = 558750), colour = "black", angle = 90) +
  geom_text(aes(x = 4107500, label = "Central", y = 559200), colour = "black", angle = 90) +
  geom_text(aes(x = 4107500, label = "Southern", y = 559820), colour = "black", angle = 90) +
  geom_text(aes(x = 4107855, label = "TSB", y = 559870), colour = "black", angle = 0) +
  geom_label(aes(x = 4108500, label = "Mainland", y= 559600), colour = "black", angle = 0) 
 
fig.5 #figure out how to allign x axis values with fig. 4
ggsave("figs/fig_5.png", plot = fig.5, width = 160, height = 120, units = "mm", dpi = 450)

#----arrange fig. 4 and 5 together ----#

Fig.4_5 <- plot_grid(fig.4, fig.5, labels = "AUTO", rel_widths = c(4,4))
Fig.4_5
ggsave("figs/fig4_5.png", plot = Fig.4_5, width = 350, height = 120, units = "mm", dpi = 450)
Fig.4_5.vert <- plot_grid(fig.4, fig.5, ncol = 1, align = 'v', labels = "AUTO")
Fig.4_5.vert
ggsave("figs/fig4_5.vert.png", plot = Fig.4_5.vert, width = 200, height = 240, units = "mm", dpi = 450)
