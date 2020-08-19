# Model for experiment acid and root-shoot ratio ####



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(tidyverse)
library(ggplot2)
library(ggbeeswarm)
library(car); #Anova(); vif(): variance inflation factors --> checking for dependence (Collinearity) (below 3 is ok)
library(nlme); #use for vif()
library(lme4)
library(lmerTest)
library(DHARMa)
#library(vcd)
library(sjPlot) #plot random effects
library(MuMIn)
library(emmeans)
library(ggeffects)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_trees/data/processed")

### Load data ###
(edata <- read_table2("data_processed_acid.txt", col_names = T, na = "na", col_types = 
                        cols(
                          .default = col_double(),
                          plot = col_factor(),
                          block = col_factor(),
                          date1 = col_date(),
                          date2 = col_date(),
                          date3 = col_date(),
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
edata <- select(edata, rootshootRatio, plot, block, replanted, species, acid, brickRatio, acidbrickRatioTreat, soilType)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #####################################################################################

#### a Graphs ---------------------------------------------------------------------------------------------
#simple effects:
par(mfrow = c(2,2))
plot(rootshootRatio ~ species, edata)
plot(rootshootRatio ~ soilType, edata)
plot(rootshootRatio ~ acidbrickRatioTreat, edata)
plot(rootshootRatio ~ block, edata)
#2way (acidbrickRatioTreat):
ggplot(edata,aes(acidbrickRatioTreat, rootshootRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (acidbrickRatioTreat:soilType):
ggplot(edata,aes(soilType, rootshootRatio, color = acidbrickRatioTreat)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (acidbrickRatioTreat:soilType):
ggplot(edata,aes(acidbrickRatioTreat, rootshootRatio)) + facet_grid(~soilType) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:acid:species):
ggplot(edata,aes(acidbrickRatioTreat, rootshootRatio)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#4way
ggplot(edata,aes(soilType, rootshootRatio, color = acidbrickRatioTreat)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
# interactions with block:
ggplot(edata,aes(species, rootshootRatio, color = acidbrickRatioTreat)) + geom_boxplot() + facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(acidbrickRatioTreat, rootshootRatio)) + geom_boxplot() + facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(block, rootshootRatio, color = soilType)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)

##### b Outliers, zero-inflation, transformations? -----------------------------------------------------
par(mfrow = c(2,2))
dotchart((edata$rootshootRatio), groups = factor(edata$species), main = "Cleveland dotplot")
dotchart((edata$rootshootRatio), groups = factor(edata$soilType), main = "Cleveland dotplot")
dotchart((edata$rootshootRatio), groups = factor(edata$brickRatio), main = "Cleveland dotplot")
dotchart((edata$rootshootRatio), groups = factor(edata$acid), main = "Cleveland dotplot")
par(mfrow=c(1,1));
boxplot(edata$rootshootRatio, ylim = c(0,2.5));#identify(rep(1,length(edata$rootshootRatio)),edata$rootshootRatio, labels = c(edata$no))
par(mfrow = c(2,2));
plot(table((edata$rootshootRatio)),type = "h", xlab = "Observed values", ylab = "Frequency")
plot(table(log(edata$rootshootRatio)), type = "h", xlab = "Observed values", ylab = "Frequency");
ggplot(edata, aes(rootshootRatio)) + geom_density()
ggplot(edata, aes(sqrt(rootshootRatio))) + geom_density()


## 2 Model building ################################################################################

#### a models ----------------------------------------------------------------------------------------
#random structure --> no random factor needed
m1 <- lmer((rootshootRatio) ~ species * acidbrickRatioTreat + (1|block), edata, REML = F)
VarCorr(m1)
#3w-model
m2 <- lm(log(rootshootRatio) ~ species * soilType * acidbrickRatioTreat, edata)
simulationOutput <- simulateResiduals(m2, plot = T)
#full 2w-model
m3 <- lm(log(rootshootRatio) ~ (species + soilType + acidbrickRatioTreat)^2, edata)
simulationOutput <- simulateResiduals(m3, plot = T)
#2w-model reduced
m4 <- lm(log(rootshootRatio) ~ species + soilType + acidbrickRatioTreat +
             acidbrickRatioTreat:species + acidbrickRatioTreat:soilType, edata)
simulationOutput <- simulateResiduals(m4, plot = T)

#### b comparison -----------------------------------------------------------------------------------------
anova(m2,m3,m4) # --> m4
rm(m1,m2,m3)

#### c model check -----------------------------------------------------------------------------------------
simulationOutput <- simulateResiduals(m4, plot = F)
par(mfrow=c(2,2));
plotResiduals(main = "species", simulationOutput$scaledResiduals, edata$species)
plotResiduals(main = "soilType", simulationOutput$scaledResiduals,edata$soilType)
plotResiduals(main = "acidbrickRatioTreat", simulationOutput$scaledResiduals, edata$acidbrickRatioTreat)
plotResiduals(main = "block", simulationOutput$scaledResiduals, edata$block)


## 3 Chosen model output ################################################################################

### Model output ---------------------------------------------------------------------------------------------
m4 <- lm(log(rootshootRatio) ~ species + soilType + acidbrickRatioTreat +
           acidbrickRatioTreat:species + acidbrickRatioTreat:soilType, edata)
summary(m4) #r2 = 0.283, r2a = 0.171
Anova(m4, type = 3)

### Effect sizes -----------------------------------------------------------------------------------------
(emm <- emmeans(m4, revpairwise ~ acidbrickRatioTreat | species, type = "response"))
plot(emm, comparison = T)
contrast(emmeans(m4, ~ acidbrickRatioTreat | species, type = "response"), "trt.vs.ctrl", ref = 1)
(emm <- emmeans(m4, revpairwise ~ acidbrickRatioTreat | soilType, type = "response"))
plot(emm, comparison = T)
(emm <- emmeans(m4, revpairwise ~ soilType | acidbrickRatioTreat, type = "response"))
