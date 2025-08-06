library(ggsankey)
library(ggalluvial)
library(devtools)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(readr)
library(ggthemes) 
library(ggrepel) #rsb added this
library(cowplot)
#install.packages("ggpubr")
library(ggpubr)

#setwd("C:/Users/roxan/Documents/Collaborations/Bella Garfield")

ogdat_sankey <- read_csv("data/ggsankey_format_ogdat.csv")

ogdat_sankey_ordered = ogdat_sankey %>%
  mutate(mom_area = factor(mom_area,
                           levels = c("NP5", "NP4", "NPG4", "NP3", "NPG3", "NP2/3", "NP2", "NPG2", "NP1", "NPG1", "NP0", "NP", "NPG", "BBN", "BBNS", "MBBU", "MBB", "BMB", "BM", "BMC", "BMS", "BBS", "BBSU", "BBSL", "AP", "APG", "TSW", "SBW", "SBE", "TSC", "TSE", "TSB", "TSBE", "P1")),
         pup_area = factor(pup_area,
                           levels = c("NP5", "NP4", "NP3", "NPG3", "NP2", "NPG2", "NP1", "NPG1", "NP0", "NPG", "NP", "BBN", "BBNS", "MBBU", "BMB", "BMC", "BMS", "BBS", "BBSU", "BBSL", "AP", "APG", "TSW", "SBW", "SBE", "TSC", "TSB", "TSBE", "P1")),
         mom_region=factor(mom_region,levels=c("Northern","Central","Southern"))) 

### New Beach Plot ###
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
  
  
### Region Plot ###
  
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
