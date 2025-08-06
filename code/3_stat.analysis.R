#install.packages("dgof")
#install.packages('outliers')
library(outliers)
library(dgof)
library(tidyverse)
library(units)
library(ggplot2)
library(raster)
library(ncdf4)
library(viridis)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggmap)

#-- testing for bimodality -#
#Figure 2
install.packages("diptest")
install.packages("LaplacesDemon")
library(diptest)
library(LaplacesDemon)
dip.test(Fig1_pupmom_dist_nounits$Distances_without_units)
is.unimodal(Fig1_pupmom_dist_nounits$Distances_without_units)

#Figure 3
install.packages("mousetrap")
library(mousetrap)


dip.test(first.pup$Distance)
is.unimodal(first.pup$Distance)he
bimodality_coefficient(first.pup$Distance) # = 0.722

#-------outliers pup_sppatialmap_df---------

grubbs_test <- grubbs.test(mom_area_count$n)

grubbs_test


# Calculate quartiles and IQR
Q1.p <- quantile(pup_spatialmap_df$n, 0.25)
Q3.p <- quantile(pup_spatialmap_df$n, 0.75)
IQR.p <- Q3.p - Q1.p
Q1.p
Q3.p
IQR.p
# Identify outliers
outliers.p <- pup_spatialmap_df$n[pup_spatialmap_df$n < Q1.p - 1.5 * IQR.p | pup_spatialmap_df$n > Q3.p + 1.5 * IQR.p]
outliers.p
#outliers: 16 = TSB

#-----outliers for mom_spatialmap_df ----#
# Calculate quartiles and IQR
Q1.m <- quantile(mom_spatialmap_df$n, 0.25)
Q3.m <- quantile(mom_spatialmap_df$n, 0.75)
IQR.m <- Q3.m - Q1.m
Q1.m
Q3.m
IQR.m
# Identify outliers
outliers.m <- mom_spatialmap_df$n[mom_spatialmap_df$n < Q1.m - 1.5 * IQR.m | mom_spatialmap_df$n > Q3.m + 1.5 * IQR.m]
outliers.m
#outliers: 15 = BBN, 15 = BBS, 12 = NP


#-----distribution of Fig1_pupmom_dist_nounits --------
Q1.pm <- quantile(Fig1_pupmom_dist_nounits$Distances_without_units, 0.25)
Q3.pm <- quantile(Fig1_pupmom_dist_nounits$Distances_without_units, 0.75)
IQR.pm <- Q3.pm - Q1.pm
Q1.pm
Q3.pm
IQR.pm

#------distribution of first.pup--------#
Q1.fp <- quantile(first.pup$Distance, 0.25)
Q3.fp <- quantile(first.pup$Distance, 0.75)
IQR.fp <- Q3.fp - Q1.fp
Q1.fp
Q3.fp
IQR.fp

#citations:
citation("tidyverse")
citation("ggplot2")
citation("sf")

#permuation test

