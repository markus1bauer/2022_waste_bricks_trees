## Script to prepare data of tree experiment ###



### Packages ---------------------------------------------------------------------------------------------
library(tidyverse)

### Start----------------------------------------------------------------------------------------------
#installr::updateR(browse_news = F, install_R = T, copy_packages = T, copy_Rprofile.site = T, keep_old_packages = T, update_packages = T, start_new_R = F, quit_R = T)
rm(list = ls())
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_trees/data/raw")

### Load data ----------------------------------------------------------------------------------------
(edata <- read_table2("data_raw.txt", col_names = T, na = "na", col_types = 
                       cols(
                         .default = col_double(),
                         plot = col_factor(),
                         block = col_factor(),
                         date1 = col_date(),
                         date2 = col_date(),
                         date3 = col_date(),
                         replanted = col_factor(),
                         species = col_factor(),
                         mycorrhiza = col_factor(levels = c("Control","Mycorrhiza")),
                         substrate = col_factor(),
                         soilType = col_factor(),
                         brickRatio = col_factor(levels = c("5","30")),
                         acid = col_factor(levels = c("Control","Acid")),
                         comment = col_factor()
                         )        
))

### Create variables ----------------------------------------------------------------------------------
edata <- unite(edata, "acidbrickRatioTreat", acid, brickRatio, sep = "_", remove = F)
edata$conf.low <- c(1:100);
edata$conf.high <- c(1:100)
edata$dateDiff13 <- as.numeric(edata$date3 - edata$date1)
edata$dateDiff12 <- as.numeric(edata$date2 - edata$date1)
edata$dateDiff23 <- as.numeric(edata$date3 - edata$date2)
#Relative growth rate according to Kramer-Walter & Laughlin 2017 Plant Soil:
edata$rgr13 <- (log(edata$diameter3 * edata$height3) - log(edata$diameter1 * edata$height1)) / edata$dateDiff13
edata$rgr12 <- (log(edata$diameter3 * edata$height3) - log(edata$diameter1 * edata$height1)) / edata$dateDiff12
edata$rgr23 <- (log(edata$diameter3 * edata$height3) - log(edata$diameter1 * edata$height1)) / edata$dateDiff23
edata <- edata %>%
  mutate(stemMass = stemMassTotal - bagMassStem) %>%
  mutate(leafMass = restleafMassTotal - bagMassRestleaf + leaf1Mass + leaf2Mass + leaf3Mass) %>%
  mutate(sla1 = leaf1Area / leaf1Mass) %>% #specific leaf area of leaf 1 of one individual
  mutate(sla2 = leaf2Area / leaf2Mass) %>% #specific leaf area of leaf 2 of one individual
  mutate(sla3 = leaf3Area / leaf3Mass) %>% #specific leaf area of leaf 3 of one individual
  mutate(absorptivefinerootMass = absorptivefinerootMassTotal - bagMassAbsorptivefineroot) %>%
  mutate(transportfinerootMass = transportfinerootMassTotal - bagMassTransportfineroot) %>%
  mutate(restrootMass = restrootMassTotal - bagMassRestroot)
edata <- edata %>%
  mutate(rootMass = transportfinerootMass + absorptivefinerootMass + restrootMass) %>%
  mutate(abstransRatio = absorptivefinerootMass / transportfinerootMass) %>%
  mutate(srl = (rootLength/100) / absorptivefinerootMass) %>% #specific root length
  mutate(rtd = rootVolume / absorptivefinerootMass) #root tissue density
edata <- edata %>%
  mutate(rmf = rootMass / (rootMass + leafMass + stemMass)) %>% #root mass fraction
  mutate(lmf = leafMass / (rootMass + leafMass + stemMass)) %>% #leaf mass fraction
  mutate(smf = stemMass / (rootMass + leafMass + stemMass)) %>% #stem mass fraction
  mutate(rootshootRatio = rootMass / (leafMass + stemMass)) #root-to-shoot ratio
edata <- select(edata, -(date1:date3), -(diameter1:transportfinerootMassTotal), -threshold, -rootMass, -(dateDiff13:dateDiff23), -(stemMass:leafMass), -(absorptivefinerootMass:restrootMass))


### Create data frame for mycorrhiza:soilType:brickRatio ----------------------------------------------------------------
edata1 <- filter(edata, acid != "Control")

### Create data frame for acid:soilType ----------------------------------------------------------------
edata2 <- filter(edata, mycorrhiza != "Mycorrhiza");

### Save processed data-------------------------------------------------------------------------------
#write.table(edata1, "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_trees/data/processed/data_processed_brickRatio.txt", sep = "\t", row.names = F, quote = F)
#write.table(edata2, "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_trees/data/processed/data_processed_acid.txt", sep = "\t", row.names = F, quote = F)
