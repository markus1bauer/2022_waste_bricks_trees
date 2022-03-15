# Waste bricks for tree substrates
# Prepare data for models and plots ####
# Markus Bauer
# 2022-03-15


### Packages -----------------------------------------------------------------
library(here)
library(tidyverse)
library(naniar)

### Start---------------------------------------------------------------------
#installr::updateR(browse_news = FALSE, install_R = TRUE, copy_packages = TRUE, copy_Rprofile.site = TRUE, keep_old_packages = TRUE, update_packages = TRUE, start_new_R = FALSE, quit_R = TRUE, print_R_versions = TRUE, GUI = TRUE)
sessionInfo()
#remotes::install_github(file.path("inbo", "checklist"))
#checklist::setup_source()
#checklist::check_source()

rm(list = ls())
setwd(here("data", "raw"))


### 1 Load data ##############################################################

(data <- read_csv("data_raw.csv", col_names = TRUE, na = "na", col_types =
                       cols(
                         .default = "d",
                         plot = "f",
                         block = "f",
                         date1 = col_date(),
                         date2 = col_date(),
                         date3 = col_date(),
                         replanted = "f",
                         species = "f",
                         mycorrhiza =
                           col_factor(levels = c("Control", "Mycorrhiza")),
                         substrate = "f",
                         soilType = "f",
                         brickRatio = col_factor(levels = c("5", "30")),
                         acid = col_factor(levels = c("Control", "Acid")),
                         comment = "f"
                         )
                  )
 )
vis_miss(data, cluster = FALSE, sort_miss = TRUE)


### 2 Create variables ######################################################


### a Dummies for confidence interval ---------------------------------------
data <- data %>%
  mutate(conf.low = c(1:100),
         conf.high = c(1:100)) %>%

### b Growing periods -------------------------------------------------------
data <- data %>%
  mutate(dateDiff13 = as.numeric(data$date3 - data$date1),
         dateDiff12 = as.numeric(data$date2 - data$date1),
         dateDiff23 = as.numeric(data$date3 - data$date2))

### c Factor: combination of acid and brickRatio ----------------------------
data <- data %>%
  unite("acidbrickRatioTreat", acid, brickRatio, sep = "_", remove = FALSE)

### d Relative growth rates according toKramer-Walter & Laughlin 2017 Plant Soil
data <- data %>%
  mutate(rgr13 = (log(data$diameter3 * data$height3) -
                    log(data$diameter1 * data$height1)) / data$dateDiff13,
         rgr12 = (log(data$diameter3 * data$height3) -
                    log(data$diameter1 * data$height1)) / data$dateDiff12,
         rgr23 = (log(data$diameter3 * data$height3) -
                    log(data$diameter1 * data$height1)) / data$dateDiff23)

### e Further functional traits ----------------------------------------------
data <- data %>%
  mutate(stemMass = stemMassTotal - bagMassStem,
         leafMass = restleafMassTotal -
           bagMassRestleaf + leaf1Mass + leaf2Mass + leaf3Mass,
         sla1 = leaf1Area / leaf1Mass,
         sla2 = leaf2Area / leaf2Mass,
         sla3 = leaf3Area / leaf3Mass,
         absorptivefinerootMass =
           absorptivefinerootMassTotal - bagMassAbsorptivefineroot,
         transportfinerootMass =
           transportfinerootMassTotal - bagMassTransportfineroot,
         restrootMass = restrootMassTotal - bagMassRestroot) %>%
  mutate(rootMass =
           transportfinerootMass + absorptivefinerootMass + restrootMass,
         abstransRatio = absorptivefinerootMass / transportfinerootMass,
         srl = (rootLength / 100) / absorptivefinerootMass,
         rtd = rootVolume / absorptivefinerootMass,
         branchingIntensity = rootTips / rootLength)  %>%
  mutate(rmf = rootMass / (rootMass + leafMass + stemMass),
         lmf = leafMass / (rootMass + leafMass + stemMass),
         smf = stemMass / (rootMass + leafMass + stemMass),
         rootshootRatio = rootMass / (leafMass + stemMass))



### 3 Prepare and separate data sets into 849318 and 0318 ####################

data <- data %>%
  select(-(date1:date3), -(soilMoisture1:soilMoisture6), -(diameter1:comment),
         -rootMass, -(dateDiff13:dateDiff23), -(stemMass:leafMass),
         -(absorptivefinerootMass:restrootMass))
### Create data frame for mycorrhiza:soilType:brickRatio ---------------------
data1 <- filter(data, acid != "Control")

### Create data frame for acid:soilType --------------------------------------
data2 <- filter(data, mycorrhiza != "Mycorrhiza")

### Check missingness --------------------------------------------------------
miss_var_summary(data1)
vis_miss(data1, cluster = FALSE, sort_miss = TRUE)
gg_miss_var(data1)
gg_miss_case(data1, order_cases = FALSE)
gg_miss_upset(data1)
vis_miss(data2, cluster = FALSE, sort_miss = TRUE)

### Save processed data-------------------------------------------------------
write_csv(data1, here("data", "processed", "data_processed_brickRatio.csv"))
write_csv(data2, here("data", "processed", "data_processed_acid.csv"))
