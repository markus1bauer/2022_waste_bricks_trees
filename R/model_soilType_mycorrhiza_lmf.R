# Model for experiment mycorrhiza and soil type and leaf mass fraction ####



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
(edata <- read_table2("data_processed_brickRatio.txt", col_names = T, na = "na", col_types = 
                        cols(
                          .default = col_double(),
                          plot = col_factor(),
                          block = col_factor(),
                          replanted = col_factor(),
                          species = col_factor(),
                          mycorrhiza = col_factor(levels = c("Control","Mycorrhiza")),
                          substrate = col_factor(),
                          soilType = col_factor(levels = c("poor","rich")),
                          brickRatio = col_factor(levels = c("5","30")),
                          acid = col_factor(levels = c("Acid")),
                          acidbrickRatioTreat = col_factor()
                        )        
))
(edata <- select(edata, lmf, plot, block, replanted, species, brickRatio, soilType, mycorrhiza))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #####################################################################################

#### a Graphs ---------------------------------------------------------------------------------------------
#simple effects:
par(mfrow = c(2,2))
plot(lmf ~ species, edata)
plot(lmf ~ brickRatio, edata)
plot(lmf ~ soilType, edata)
plot(lmf ~ mycorrhiza, edata)
par(mfrow = c(2,2))
plot(lmf ~ block, edata)
#2way (brickRatio:species):
ggplot(edata, aes(species, lmf, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (brickRatio:soilType):
ggplot(edata, aes(soilType, lmf, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (brickRatio:mycorrhiza):
ggplot(edata, aes(mycorrhiza, lmf, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (species:soilType):
ggplot(edata, aes(species, lmf, color = soilType)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (species:mycorrhiza):
ggplot(edata, aes(species, lmf, color = mycorrhiza)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (soilType:mycorrhiza):
ggplot(edata, aes(soilType, lmf, color = mycorrhiza)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:species:soilType):
ggplot(edata, aes(soilType, lmf, color = brickRatio)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:species:mycorrhiza):
ggplot(edata, aes(mycorrhiza, lmf, color = brickRatio)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (species:soilType:mycorrhiza):
ggplot(edata, aes(soilType, lmf, color = mycorrhiza)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#4way
ggplot(edata,aes(soilType, lmf, color = brickRatio, shape = mycorrhiza)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
# interactions with block:
ggplot(edata,aes(brickRatio, lmf, color = species)) + geom_boxplot() + facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(block, lmf, color = species)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(block, lmf, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(block, lmf, color = soilType)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(block, lmf, color = mycorrhiza)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)

##### b Outliers, zero-inflation, transformations? -----------------------------------------------------
par(mfrow = c(2,2))
dotchart((edata$lmf), groups = factor(edata$species), main = "Cleveland dotplot")
dotchart((edata$lmf), groups = factor(edata$brickRatio), main = "Cleveland dotplot")
dotchart((edata$lmf), groups = factor(edata$soilType), main = "Cleveland dotplot")
dotchart((edata$lmf), groups = factor(edata$mycorrhiza), main = "Cleveland dotplot")
dotchart((edata$lmf), groups = factor(edata$block), main = "Cleveland dotplot")
par(mfrow=c(1,1));
boxplot(edata$lmf);#identify(rep(1,length(edata$lmf)),edata$lmf, labels = c(edata$no))
plot(table((edata$lmf)), type = "h", xlab = "Observed values", ylab = "Frequency")
ggplot(edata, aes(lmf)) + geom_density()
ggplot(edata, aes(log(lmf))) + geom_density()


## 2 Model building ################################################################################

#### a models ----------------------------------------------------------------------------------------
#random structure --> no random effect needed
m1 <- lmer(lmf ~ species * brickRatio + (1|block), edata, REML = F)
VarCorr(m1)
#4w-model
m2 <- lm(log(lmf) ~ species * brickRatio * soilType * mycorrhiza, edata)
simulateResiduals(m2, plot = T)
#full 3w-model
m3 <- lm(log(lmf) ~ (species + brickRatio + soilType + mycorrhiza)^3, edata)
simulateResiduals(m3, plot = T)
#3w-model reduced
m4 <- lm(log(lmf) ~ (species + brickRatio + soilType + mycorrhiza)^2 +
             species:brickRatio:soilType + species:brickRatio:mycorrhiza, edata)
simulateResiduals(m4, plot = T)
#2w-model full
m5 <- lm(log(lmf) ~ (species + brickRatio + soilType + mycorrhiza)^2, edata)
simulateResiduals(m5, plot = T)
#2w-model reduces
m6 <- lm(log(lmf) ~ (species + brickRatio + soilType + mycorrhiza) +
             species:brickRatio + species:soilType + species:mycorrhiza, edata)
simulateResiduals(m6, plot = T);
#1w-model full
m7 <- lm(log(lmf) ~ (species + brickRatio + soilType + mycorrhiza), edata)
simulateResiduals(m7, plot = T);

#### b comparison -----------------------------------------------------------------------------------------
anova(m2,m3,m4,m5,m6,m7) # --> m7 BUT use m4 because of 3-fold interaction
rm(m1,m2,m3,m5,m6,m7)

#### c model check -----------------------------------------------------------------------------------------
simulationOutput <- simulateResiduals(m4, plot = T)
par(mfrow=c(2,2));
plotResiduals(main = "species", simulationOutput$scaledResiduals, edata$species)
plotResiduals(main = "brickRatio", simulationOutput$scaledResiduals, edata$brickRatio)
plotResiduals(main = "soilType", simulationOutput$scaledResiduals, edata$soilType)
plotResiduals(main = "mycorrhiza", simulationOutput$scaledResiduals, edata$mycorrhiza)
plotResiduals(main = "block", simulationOutput$scaledResiduals, edata$block)


## 3 Chosen model output ################################################################################

### Model output ---------------------------------------------------------------------------------------------
m4 <- lm(sqrt(lmf) ~ (species + brickRatio + soilType + mycorrhiza)^2 +
           species:brickRatio:soilType + species:brickRatio:mycorrhiza, edata)
summary(m4) #r2 = 0.651, r2adj = 0.589
car::Anova(m4, type = 3)

### Effect sizes -----------------------------------------------------------------------------------------
(emm <- emmeans(m4, revpairwise ~ brickRatio * soilType | species, type="response"))
plot(emm, comparison = T)
contrast(emmeans(m4, ~ brickRatio * soilType | species, type = "response"), "trt.vs.ctrl", ref = 1)
(emm <- emmeans(m4, revpairwise ~ brickRatio | mycorrhiza | species, type = "response"))
plot(emm, comparison = T)
contrast(emmeans(m4, ~ brickRatio * mycorrhiza | species, type = "response"), "trt.vs.ctrl", ref = 1)
