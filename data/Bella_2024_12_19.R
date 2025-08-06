library(ggsankey)
library(ggalluvial)
library(devtools)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(readr)
library(ggthemes) #rsb added
library(nnet)
library(broom)
library(multicool)

#setwd("C:/Users/roxan/Documents/Collaborations/Bella Garfield")
#setwd("C:/marm/research/roxanne/bella")
#NOTE: THIS DOESN'T MATCH THE DATA IN ggsankey_format_ogdat.csv SEE BELOW!!!
region_sankey <- read_csv("region_sankey.csv") 
region_sankey


#rsb added to order
region_sankey=region_sankey %>% 
  mutate(mom_region=factor(mom_region,
                           levels=c("northern_mom","central_mom",
                                    "southern_mom")),
         pup_region=factor(pup_region,
                           levels=c("northern_pup","central_pup",
                                    "southern_pup")))

ggplot(as.data.frame(region_sankey),
       aes(y = n, axis1 = mom_region, axis2 = pup_region)) +
  geom_alluvium(aes(group=mom_region),width = 1/12) +
  geom_stratum(width = 1/12, color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("mom_region", "pup_region"), 
                   expand = c(.05, .05)) +
  scale_fill_manual(values=c("blue","green","orange")) +
  ggtitle("Northern elephant seal straying rates")+
  theme_few()

#scale_fill_manual is not working for discrete variables here. 

dat <- 'ggsankey_format_ogdat.csv'
dat[pup_region]=as.factor(dat$pup_region)
dat$pup_region=relevel(dat$pup_region, ref = "southern")
dat$mom_region=as.factor(dat$mom_region)
dat$mom_region=relevel(dat$mom_region, ref = "southern")
test=multinom(pup_region ~ mom_region-1, data = dat);tidy(test) #use this one! easier to interpret
#test=multinom(pup_region ~ mom_region, data = dat)
summary(test)
out=tidy(test);out

with(dat,table(mom_region,pup_region)) #Table of occurrences
#NOTE: THIS DOESN'T MATCH THE DATA IN ggsankey.csv!!!

#confirming that odds for southerns independent of other moms
test=multinom(pup_region ~ mom_region-1, data = dat[dat$mom_region=="southern",]);tidy(test)
#Note: exp(1.47) = 1/exp(-1.47)
1/exp(-1.47) #likelihood of southern mamas producing pups in northern regions as compared to southern regions

1/exp(-2.56) #likelihood of southern mamas producing pups in central regions as compared to southern regions

exp(1.24) #likelihoods of central moms producing northern region pups as compared to southern moms pupping in the northern region

exp(2.41)
