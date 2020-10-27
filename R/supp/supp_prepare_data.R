### Script to prepare data of grain size distribution ###



### Packages ---------------------------------------------------------------------------------------------
library(tidyverse)

### Start----------------------------------------------------------------------------------------------
rm(list = ls())
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_trees/data/raw")

### Load data ###
edata <- read_table2("supp_data_raw.txt", col_names = T, na = "na", col_types = 
                       cols(
                         .default = col_double()
                       )        
)

edata <- gather(edata, "substrate", "ratio", 2:9)
edata <- edata %>%
  group_by(substrate) %>%
  mutate(grainSizeCum = cumsum(ratio), )

### Create data frame for experiment 1 ----------------------------------------------------------------
edata1 <- filter(edata, substrate != "substrate_3" & substrate != "substrate_6")
edata1$substrateAbb <- dplyr::recode(edata1$substrate,
                                     "substrate_1" = "Rich_30%_bricks",
                                     "substrate_2" = "Rich_5%_bricks",
                                     "substrate_4" = "Poor_30%_bricks",
                                     "substrate_5" = "Poor_5%_bricks",
)

### Create data frame for experiment 2 ----------------------------------------------------------------
edata$substrateAbb <- dplyr::recode(edata$substrate,
                                     "substrate_1" = "Rich_30%_bricks_Acid",
                                     "substrate_2" = "Rich_5%_bricks_Acid",
                                     "substrate_3" = "Rich_30%_bricks_Control",
                                     "substrate_4" = "Poor_30%_bricks_Acid",
                                     "substrate_5" = "Poor_5%_bricks_Acid",
                                     "substrate_6" = "Poor_30%_bricks_Control"
)

### Save processed data-------------------------------------------------------------------------------
write.table(edata1,"Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_trees/data/processed/supp_data_processed_brickRatio.txt", sep="\t", row.names=F)
write.table(edata,"Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_trees/data/processed/supp_data_processed_acid.txt", sep="\t", row.names=F)
