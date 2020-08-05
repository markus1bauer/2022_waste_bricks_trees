## Script to prepare data of tree experiment ###



### Packages ---------------------------------------------------------------------------------------------
library(tidyverse)

### Start----------------------------------------------------------------------------------------------
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
                         acid = col_factor(levels = c("Control","Acid"))
                         )        
))

### Create variables ----------------------------------------------------------------------------------
edata <- unite(edata, "acidbrickRatioTreat", acid, brickRatio, sep = "_", remove = F)
edata$conf.low <- c(1:100);
edata$conf.high <- c(1:100)
edata$dateDiff13 <- as.numeric(edata$date3 - edata$date1)
edata$dateDiff12 <- as.numeric(edata$date2 - edata$date1)
edata$dateDiff23 <- as.numeric(edata$date3 - edata$date2)
edata$rgr13 <- (log(edata$diameter3 * edata$height3) - log(edata$diameter1 * edata$height1)) / edata$dateDiff13
edata$rgr12 <- (log(edata$diameter3 * edata$height3) - log(edata$diameter1 * edata$height1)) / edata$dateDiff12
edata$rgr23 <- (log(edata$diameter3 * edata$height3) - log(edata$diameter1 * edata$height1)) / edata$dateDiff23
edata <- edata %>%
  mutate(stemMass = stemMassTotal - bagWeightStem) %>%
  mutate(leafMass = leafMassTotal - bagWeightLeaf + leaf1Weight + leaf2Weight + leaf3Weight) %>%
  mutate(rootMass = rootMassTotal - bagWeightRoot) %>%
  mutate(sla1 = leaf1Area / leaf1Weight) %>%
  mutate(sla2 = leaf2Area / leaf2Weight) %>%
  mutate(sla3 = leaf3Area / leaf3Weight)
edata <- edata %>%
  mutate(rmf = rootMass / (rootMass + leafMass + stemMass)) %>%
  mutate(lmf = leafMass / (rootMass + leafMass + stemMass)) %>%
  mutate(rootshootRatio = rootMass / (leafMass + stemMass))
edata <- select(edata, -(diameter1:stemMassTotal), -(dateDiff13:dateDiff23), -(bagWeightLeaf:rootMassTotal))


### Create data frame for mycorrhiza:soilType:brickRatio ----------------------------------------------------------------
edata1 <- filter(edata, acid != "Control")

### Create data frame for acid:soilType ----------------------------------------------------------------
edata2 <- filter(edata, mycorrhiza != "Mycorrhiza");

### Save processed data-------------------------------------------------------------------------------
write.table(edata1, "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_trees/data/processed/data_processed_brickRatio.txt", sep="\t", row.names=F)
write.table(edata2, "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_trees/data/processed/data_processed_acid.txt", sep="\t", row.names=F)
