# Waste bricks for tree substrates
# Prepare data for supplementary data ####
# Markus Bauer
# 2022-03-15



### Packages -----------------------------------------------------------------
library(here)
library(tidyverse)

### Start---------------------------------------------------------------------
rm(list = ls())
setwd(here("data", "raw"))

### Load data ###
edata <- read_table2("supp_data_raw.txt",
                     col_names = TRUE, na = "na", col_types =
                       cols(
                         .default = "d"
                       )
                     ) %>%
  gather("substrate", "ratio", 2:9) %>%
  group_by(substrate) %>%
  mutate(grainSizeCum = cumsum(ratio))

### Create data frame for experiment 1 ---------------------------------------
edata1 <- edata %>%
  filter(substrate != "substrate_3" & substrate != "substrate_6") %>%
  mutate(substrateAbb =
           recode(substrate,
                  "substrate_1" = "Rich_30%_bricks",
                  "substrate_2" = "Rich_5%_bricks",
                  "substrate_4" = "Poor_30%_bricks",
                  "substrate_5" = "Poor_5%_bricks"))

### Create data frame for experiment 2 ---------------------------------------
edata <- edata %>%
  mutate(substrateAbb =
           recode(substrate,
                  "substrate_1" = "Rich_30%_bricks_Acid",
                  "substrate_2" = "Rich_5%_bricks_Acid",
                  "substrate_3" = "Rich_30%_bricks_Control",
                  "substrate_4" = "Poor_30%_bricks_Acid",
                  "substrate_5" = "Poor_5%_bricks_Acid",
                  "substrate_6" = "Poor_30%_bricks_Control"))

### Save processed data-------------------------------------------------------
write.table(edata1, sep = "\t", row.names = FALSE,
            here("data", "processed", "supp_data_processed_brickRatio.txt"))
write.table(edata, sep = "\t", row.names = FALSE,
            here("data", "processed", "supp_data_processed_acid.txt"))
