# Model for experiment acid and root mass fraction ####



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(tidyverse)
library(ggbeeswarm)
library(lmerTest)
library(DHARMa)
library(emmeans)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_trees/data/processed")

### Load data ###
(edata <- read_table2("data_processed_acid.txt", col_names = T, na = "na", col_types = 
                        cols(
                          .default = col_double(),
                          plot = col_factor(),
                          block = col_factor(),
                          replanted = col_factor(),
                          species = col_factor(),
                          mycorrhiza = col_factor(),
                          substrate = col_factor(),
                          soilType = col_factor(levels = c("poor","rich")),
                          brickRatio = col_factor(levels = c("5","30")),
                          acid = col_factor(levels = c("Control","Acid")),
                          acidbrickRatioTreat = col_factor(levels = c("Control_30","Acid_5","Acid_30"))
                        )        
))
edata <- select(edata, rmf, plot, block, replanted, species, acidbrickRatioTreat, soilType)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #####################################################################################

#### a Graphs ---------------------------------------------------------------------------------------------
#simple effects:
par(mfrow = c(2,2))
plot(rmf ~ species, edata)
plot(rmf ~ soilType, edata)
plot(rmf ~ acidbrickRatioTreat, edata)
plot(rmf ~ block, edata)
#2way (species:soilType):
ggplot(edata, aes(soilType, rmf, color = species)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (species:replanted):
ggplot(edata, aes(replanted, rmf, color = species)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (acidbrickRatioTreat:soilType):
ggplot(edata, aes(acidbrickRatioTreat, rmf)) + facet_grid(~soilType) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:acid:species):
ggplot(edata, aes(acidbrickRatioTreat, rmf)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#4way
ggplot(edata, aes(soilType, rmf, color = acidbrickRatioTreat)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#interactions with block:
ggplot(edata, aes(species, rmf, color = acidbrickRatioTreat)) + geom_boxplot() + facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(edata, aes(acidbrickRatioTreat, rmf)) + geom_boxplot() + facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(edata, aes(block, rmf, color = soilType)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)

##### b Outliers, zero-inflation, transformations? -----------------------------------------------------
par(mfrow = c(2,2))
dotchart((edata$rmf), groups = factor(edata$species), main = "Cleveland dotplot")
dotchart((edata$rmf), groups = factor(edata$soilType), main = "Cleveland dotplot")
dotchart((edata$rmf), groups = factor(edata$acidbrickRatioTreat), main = "Cleveland dotplot")
dotchart((edata$rmf), groups = factor(edata$block), main = "Cleveland dotplot")
par(mfrow=c(1,1));
boxplot(edata$rmf);#identify(rep(1,length(edata$rmf)), edata$rmf, labels = c(edata$no))
plot(table((edata$rmf)), type = "h", xlab = "Observed values", ylab = "Frequency")
ggplot(edata, aes(rmf)) + geom_density()
ggplot(edata, aes(log(rmf))) + geom_density()



## 2 Model building ################################################################################

#### a models ----------------------------------------------------------------------------------------
#random structure --> no random factor needed
m1 <- lmer(rmf ~ species * acidbrickRatioTreat + (1|block), edata, REML = F)
VarCorr(m1)
#3w-model
m2 <- lm(rmf ~ species * soilType * acidbrickRatioTreat, edata)
simulateResiduals(m2, plot = T)
#full 2w-model
m3 <- lm(rmf ~ (species + soilType + acidbrickRatioTreat)^2, edata)
simulateResiduals(m3, plot = T)
#2w-model reduced
m4 <- lm(rmf ~ species + soilType + acidbrickRatioTreat +
             acidbrickRatioTreat:species + acidbrickRatioTreat:soilType, edata)
simulateResiduals(m4, plot = T)

#### b comparison -----------------------------------------------------------------------------------------
anova(m2,m3,m4) # --> m4
rm(m1,m2,m3)

#### c model check -----------------------------------------------------------------------------------------
simulationOutput <- simulateResiduals(m4, plot = T)
par(mfrow=c(2,2));
plotResiduals(main = "species", simulationOutput$scaledResiduals, edata$species)
plotResiduals(main = "soilType", simulationOutput$scaledResiduals,edata$soilType)
plotResiduals(main = "acidbrickRatioTreat", simulationOutput$scaledResiduals, edata$acidbrickRatioTreat)
plotResiduals(main = "block", simulationOutput$scaledResiduals, edata$block)


## 3 Chosen model output ################################################################################

### Model output ---------------------------------------------------------------------------------------------
m4 <- lm(rmf ~ species + soilType + acidbrickRatioTreat + 
           acidbrickRatioTreat:species + acidbrickRatioTreat:soilType, edata)
summary(m4) #r2 = 0.282, r2a = 0.169
car::Anova(m4, type = 3)

### Effect sizes -----------------------------------------------------------------------------------------
(emm <- emmeans(m4, revpairwise ~ acidbrickRatioTreat | species, type = "response"))
plot(emm, comparison = T)
contrast(emmeans(m4, ~ acidbrickRatioTreat | species, type = "response"), "trt.vs.ctrl", ref = 1)
(emm <- emmeans(m4, revpairwise ~ acidbrickRatioTreat | soilType, type = "response"))
plot(emm, comparison = T)
