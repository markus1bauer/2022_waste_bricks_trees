## Script to prepare data of tree experiment ####
# Markus Bauer
# Citation: Markus Bauer, Martin Krause, Valentin Heizinger & Johannes Kollmann  (2021) ...
# DOI: ...


### Packages ---------------------------------------------------------------------------------------------
library(tidyverse)
library(naniar)

### Start----------------------------------------------------------------------------------------------
#installr::updateR(browse_news = F, install_R = T, copy_packages = T, copy_Rprofile.site = T, keep_old_packages = T, update_packages = T, start_new_R = F, quit_R = T)
rm(list = ls())
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_trees/data/raw")
#library(installr);updateR(browse_news=F, install_R=T, copy_packages = T,copy_Rprofile.site = T,keep_old_packages = T, update_packages = T)


### 1 Load data #####################################################################################

(data <- read_csv("data_raw.csv", col_names = T, na = "na", col_types = 
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
vis_miss(data, cluster = F, sort_miss = T)


### 2 Create variables #####################################################################################


### a Dummies for confidence interval -------------------------------------------------------------------------------------------
data$conf.low <- c(1:100);
data$conf.high <- c(1:100)

### b Growing periods -------------------------------------------------------------------------------------------
data$dateDiff13 <- as.numeric(data$date3 - data$date1)
data$dateDiff12 <- as.numeric(data$date2 - data$date1)
data$dateDiff23 <- as.numeric(data$date3 - data$date2)

### c Factor: combination of acid and brickRatio -------------------------------------------------------------------------------------------
data <- unite(data, "acidbrickRatioTreat", acid, brickRatio, sep = "_", remove = F)

### d Relative growth rates according toKramer-Walter & Laughlin 2017 Plant Soil -------------------------------------------------------------------------------------------
data$rgr13 <- (log(data$diameter3 * data$height3) - log(data$diameter1 * data$height1)) / data$dateDiff13
data$rgr12 <- (log(data$diameter3 * data$height3) - log(data$diameter1 * data$height1)) / data$dateDiff12
data$rgr23 <- (log(data$diameter3 * data$height3) - log(data$diameter1 * data$height1)) / data$dateDiff23

### e Further functional traits -------------------------------------------------------------------------------------------
data <- data %>%
  mutate(stemMass = stemMassTotal - bagMassStem) %>%
  mutate(leafMass = restleafMassTotal - bagMassRestleaf + leaf1Mass + leaf2Mass + leaf3Mass) %>%
  mutate(sla1 = leaf1Area / leaf1Mass) %>% #specific leaf area of leaf 1 of one individual
  mutate(sla2 = leaf2Area / leaf2Mass) %>% #specific leaf area of leaf 2 of one individual
  mutate(sla3 = leaf3Area / leaf3Mass) %>% #specific leaf area of leaf 3 of one individual
  mutate(absorptivefinerootMass = absorptivefinerootMassTotal - bagMassAbsorptivefineroot) %>%
  mutate(transportfinerootMass = transportfinerootMassTotal - bagMassTransportfineroot) %>%
  mutate(restrootMass = restrootMassTotal - bagMassRestroot)
data <- data %>%
  mutate(rootMass = transportfinerootMass + absorptivefinerootMass + restrootMass) %>%
  mutate(abstransRatio = absorptivefinerootMass / transportfinerootMass) %>%
  mutate(srl = (rootLength/100) / absorptivefinerootMass) %>% #specific root length
  mutate(rtd = rootVolume / absorptivefinerootMass) %>% #root tissue density
  mutate(branchingIntensity = rootTips / rootLength) #branching intensity
data <- data %>%
  mutate(rmf = rootMass / (rootMass + leafMass + stemMass)) %>% #root mass fraction
  mutate(lmf = leafMass / (rootMass + leafMass + stemMass)) %>% #leaf mass fraction
  mutate(smf = stemMass / (rootMass + leafMass + stemMass)) %>% #stem mass fraction
  mutate(rootshootRatio = rootMass / (leafMass + stemMass)) #root-to-shoot ratio



### 3 Prepare and separate data sets into 849318 and 0318 #####################################################################################

data <- select(data, -(date1:date3), -(soilMoisture1:soilMoisture6), -(diameter1:comment), -rootMass, -(dateDiff13:dateDiff23), -(stemMass:leafMass), -(absorptivefinerootMass:restrootMass))
### Create data frame for mycorrhiza:soilType:brickRatio ----------------------------------------------------------------
data1 <- filter(data, acid != "Control")

### Create data frame for acid:soilType ----------------------------------------------------------------
data2 <- filter(data, mycorrhiza != "Mycorrhiza");

### Check missingness ----------------------------------------------------------------
miss_var_summary(data1)
vis_miss(data1, cluster = F, sort_miss = T)
gg_miss_var(data1)
gg_miss_case(data1, order_cases = F)
gg_miss_upset(data1)
vis_miss(data2, cluster = F, sort_miss = T)

### Save processed data-------------------------------------------------------------------------------
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_trees/data/processed")
write_csv2(data1, "data_processed_brickRatio.csv")
write_csv2(data2, "data_processed_acid.csv")
