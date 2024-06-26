#install.packages("dgof")
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

#-------the following is ks test for natal - pupping distances compared to null distribution----#
kftest_null <- unlist(null_df) #unlist null values
kftest_null_numeric <- as.numeric(unlist(null_df)) #make null values numeric
kftest_observed <- unlist(Fig1_pupmom_dist_nounits[, "Distances_without_units", drop = FALSE]) #unlist observed values
kftest_observed_numeric <- as.numeric(kftest_observed) #make observed values numeric
ks.test(kftest_null_numeric, kftest_observed_numeric) #runs official ks test, gives D and p values


pdf("ecdf_comparison_plot.pdf") #pdf of ks plot for natal - pupping location
plot(ecdf(kftest_null_numeric), 
     xlim = range(c(kftest_null_numeric, kftest_observed_numeric)), 
     col = "blue",
     main = "ECDF Comparison of Null and Observed Data",
     ylab = "Cumulative Probability",
     xlab = "Distance (m)")

lines(ecdf(kftest_observed_numeric), 
      col = "goldenrod",
      lty = "dashed")

kstest_plot <- recordPlot()
kstest_plot   
dev.off()



#-----the following is ks test for first pup - later pupping location compared to its relative null distribution-----#


firstpup.kftest_null <- unlist(first.pup_null_df) #unlist null values
firstpup.kftest_null_numeric <- as.numeric(unlist(first.pup_null_df)) #make null values numeric
firstpup.kftest_observed <- unlist(first.pup[, "Distance", drop = FALSE]) #unlist observed values
firstpup.kftest_observed_numeric <- as.numeric(firstpup.kftest_observed) #make observed values numeric

#random noise to break ties?
firstpup_kftest_null_numeric <- firstpup.kftest_null_numeric + runif(length(firstpup.kftest_null_numeric), -1e-10, 1e-10)
firstpup_kftest_observed_numeric <- firstpup.kftest_observed_numeric + runif(length(firstpup.kftest_observed_numeric), -1e-10, 1e-10)


ks.test(firstpup_kftest_null_numeric, firstpup_kftest_observed_numeric) #runs official ks test, gives D and p values

pdf("firstpup.ecdf_comparison_plot.pdf") #pdf of ks plot for natal - pupping location
plot(ecdf(firstpup_kftest_null_numeric), 
     xlim = range(c(firstpup_kftest_null_numeric, firstpup_kftest_observed_numeric)), 
     col = "blue",
     main = "ECDF Comparison of Null and Observed Data",
     ylab = "Cumulative Probability",
     xlab = "Distance (m)")

lines(ecdf(firstpup_kftest_observed_numeric), 
      col = "seagreen",
      lty = "dashed")

kstest_plot <- recordPlot()
kstest_plot   
dev.off()

#-------outliers pup_sppatialmap_df---------

# Calculate quartiles and IQR
Q1.p <- quantile(pup_spatialmap_df$n, 0.25)
Q3.p <- quantile(pup_spatialmap_df$n, 0.75)
IQR.p <- Q3 - Q1
Q1.p
Q3.p
IQR.p
# Identify outliers
outliers.p <- pup_spatialmap_df$n[pup_spatialmap_df$n < Q1 - 1.5 * IQR | pup_spatialmap_df$n > Q3 + 1.5 * IQR]
outliers.p
#outliers: 16 = TSB

#-----outliers for mom_spatialmap_df ----#
# Calculate quartiles and IQR
Q1.m <- quantile(mom_spatialmap_df$n, 0.25)
Q3.m <- quantile(mom_spatialmap_df$n, 0.75)
IQR.m <- Q3 - Q1
Q1.m
Q3.m
IQR.m
# Identify outliers
outliers.p <- mom_spatialmap_df$n[mom_spatialmap_df$n < Q1 - 1.5 * IQR | mom_spatialmap_df$n > Q3 + 1.5 * IQR]
outliers.p
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

