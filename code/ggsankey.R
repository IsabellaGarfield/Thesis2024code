
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("forcats")
#install.packages("devtools")
#install.packages("ggalluvial")
#devtools::install_github("davidsjoberg/ggsankey")
#install.packages("networkD3")
install.packages("ggrepel")
library(ggrepel)
library(ggsankey)
library(ggalluvial)
library(devtools)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(networkD3)
library(readr)


ogdat_sankey <- read_csv("data/ggsankey_format_ogdat.csv")
ogdat <- read_csv("data/pups_moms_with_coords.csv")
region_sankey <- read_csv("data/region_sankey.csv")

region_sankey=region_sankey %>%
  mutate(mom_region=factor(mom_region,
                           levels=c("northern_mom","central_mom",
                                    "southern_mom")),
         pup_region=factor(pup_region,
                           levels=c("northern_pup","central_pup",
                                    "southern_pup")))

ogdat_sankey_ordered = ogdat_sankey %>%
  mutate(mom_area = factor(mom_area,
                           levels = c("NP5", "NP4", "NPG4", "NP3", "NPG3", "NP2/3", "NP2", "NPG2", "NP1", "NPG1", "NP0", "NP", "NPG", "BBN", "BBNS", "MBBU", "MBB", "BMB", "BM", "BMC", "BMS", "BBS", "BBSU", "BBSL", "AP", "APG", "TSW", "SBW", "SBE", "TSC", "TSE", "TSB", "TSBE", "P1")),
         pup_area = factor(pup_area,
                           levels = c("NP5", "NP4", "NP3", "NPG3", "NP2", "NPG2", "NP1", "NPG1", "NP0", "NPG", "NP", "BBN", "BBNS", "MBBU", "BMB", "BMC", "BMS", "BBS", "BBSU", "BBSL", "AP", "APG", "TSW", "SBW", "SBE", "TSC", "TSB", "TSBE", "P1")))

write.csv(ogdat_sankey_ordered, "/eseal/data/ogdat_sankey_ordered.csv")

####alluvium plot by three regions
region_sankey$n <- as.factor(data$n)


alluvium_1 <- ggplot(data = region_sankey,
       aes(axis1 = mom_region, 
           axis2 = pup_region, 
           y = n,
           label = pup_region)) +
  geom_alluvium(aes(fill = mom_region))+
  geom_stratum(aes(fill = mom_region)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("mom_region", "pup_region"), expand = c(.15, .05))+
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_manual(values = c("northern_mom" = "blue", "central_mom" = "green", "southern_mom" = "orange")) + #manually assign colors
  theme_minimal() 

sankey_plot
ggsave("eseal/figures/alluvium_1", plot = alluvium_1, width = 250, height = 500, units = "mm", dpi = 450)

###working sankey plot#####
region_sankey_ordered <- fct_relevel(region_sankey$mom_region, "northern", "central", "southern")

ggplot(data = region_sankey,
       aes(axis1 = mom_region, 
           axis2 = pup_region,
           y = n)) +
  geom_alluvium(aes(fill = mom_region)) +
  geom_alluvium(aes(fill = pup_region)) +
  geom_stratum(aes(fill = mom_region)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("mom_region", "pup_region"), expand = c(.15, .05)) +
  scale_fill_manual(values = c("northern_mom" = "blue",  "central_mom" = "green", "southern_mom" = "orange")) 

####Sankey by n###


ggplot(as.data.frame(region_sankey),
       aes(y = n, axis1 = mom_region, axis2 = pup_region)) +
  geom_alluvium(aes(fill = n), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("mom_region", "pup_region"), expand = c(.05, .05)) +
 # scale_fill_manual(values = c("northern_mom" = "blue",  "central_mom" = "green", "southern_mom" = "orange")) +
  ggtitle("Northern elephant seal straying rates")


###Sankey by beach####

ggplot(as.data.frame(ogdat_sankey_ordered),
       aes(y = n, axis1 = as.factor(mom_area), axis2 = as.factor(pup_area))) +
  geom_alluvium(aes(fill = as.factor(mom_region)), width = 1/12) +
  geom_stratum(aes(data = mom_area, fill = mom_region), width = 1/12, color = "grey") +
  #geom_stratum(aes(data = pup_area, fill = pup_region), width = 1/12, color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)), size = 2) +
  scale_x_discrete(limits = c("mom_area", "pup_area"), expand = c(.05, .05)) +
  guides(fill = guide_legend(title = "Beach region")) +
  ggtitle("Northern elephant seal straying rates")
 

