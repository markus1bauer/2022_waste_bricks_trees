
## Script to prepare data of experiment 1 and 2 ###



### Packages ---------------------------------------------------------------------------------------------
library(tidyverse)

### Start----------------------------------------------------------------------------------------------
rm(list = ls())
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/raw")

### Load data ----------------------------------------------------------------------------------------
edata <- read_table2("data_raw.txt", col_names = T, na = "na", col_types = 
                       cols(
                         .default = col_double(),
                         pot = col_factor(),
                         block = col_factor(),
                         brickRatio = col_factor(levels = c("5","30")),
                         acid = col_factor(levels = c("Control","Acid")),
                         mycorrhiza = col_factor(levels = c("Control","Mycorrhiza"))
                       )        
)

### Create variables ----------------------------------------------------------------------------------
edata$conf.low <- c(1:100);
edata$conf.high <- c(1:100)
edata$dateDiff <- as.numeric(as.Date(as.character(edata$date3),format="%d.%m.%Y") - as.Date(as.character(edata$date1),format="%d.%m.%Y"))
edata$rgr <- (log(edata$diameter3 * edata$height3) - log(edata$diameter1 * edata$height1)) / edata$dateDiff

### Create data frame for acid:soilType ----------------------------------------------------------------
edata1 <- filter(edata, mcorrhiza != "Mycorrhiza");

### Create data frame for mycorrhiza:soilType:brickRatio ----------------------------------------------------------------
edata2 <- filter(edata, acid = "Acid")

### Save processed data-------------------------------------------------------------------------------
#write.table(edata1,"data_acid_processed.txt", sep="\t", row.names=F)
#write.table(edata2,"data_brickRatio_processed.txt", sep="\t", row.names=F)