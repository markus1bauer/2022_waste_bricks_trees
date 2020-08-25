# Model for experiment mycorrhiza and soil type and root mass fraction ####



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
edata <- select(edata, rmf, plot, block, replanted, species, brickRatio, soilType, mycorrhiza)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #####################################################################################

#### a Graphs ---------------------------------------------------------------------------------------------
#simple effects:
par(mfrow = c(2,2))
plot(rmf ~ species, edata)
plot(rmf ~ brickRatio, edata)
plot(rmf ~ soilType, edata)
plot(rmf ~ mycorrhiza, edata)
par(mfrow = c(2,2))
plot(rmf ~ block, edata)
#2way (brickRatio:species):
ggplot(edata, aes(species, rmf, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (brickRatio:soilType):
ggplot(edata, aes(soilType, rmf, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (brickRatio:mycorrhiza):
ggplot(edata, aes(mycorrhiza, rmf, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (species:soilType):
ggplot(edata, aes(species, rmf, color = soilType)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (species:mycorrhiza):
ggplot(edata, aes(species, rmf, color = mycorrhiza)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (soilType:mycorrhiza):
ggplot(edata, aes(soilType, rmf, color = mycorrhiza)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:species:soilType):
ggplot(edata, aes(soilType, rmf, color = brickRatio)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:species:mycorrhiza):
ggplot(edata, aes(mycorrhiza, rmf, color = brickRatio)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (species:soilType:mycorrhiza):
ggplot(edata, aes(soilType, rmf, color = mycorrhiza)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#4way
ggplot(edata,aes(soilType, rmf, color = brickRatio, shape = mycorrhiza)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
# interactions with block:
ggplot(edata,aes(brickRatio, rmf, color = species)) + geom_boxplot() + facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(block, rmf, color = species)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(block, rmf, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(block, rmf, color = soilType)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(block, rmf, color = mycorrhiza)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)

##### b Outliers, zero-inflation, transformations? -----------------------------------------------------
par(mfrow = c(2,2))
dotchart((edata$rmf), groups = factor(edata$species), main = "Cleveland dotplot")
dotchart((edata$rmf), groups = factor(edata$brickRatio), main = "Cleveland dotplot")
dotchart((edata$rmf), groups = factor(edata$soilType), main = "Cleveland dotplot")
dotchart((edata$rmf), groups = factor(edata$mycorrhiza), main = "Cleveland dotplot")
dotchart((edata$rmf), groups = factor(edata$block), main = "Cleveland dotplot")
par(mfrow=c(1,1));
boxplot(edata$rmf);#identify(rep(1,length(edata$rmf)),edata$rmf, labels = c(edata$no))
plot(table((edata$rmf)), type = "h", xlab = "Observed values", ylab = "Frequency")
ggplot(edata, aes(rmf)) + geom_density()
ggplot(edata, aes(log(rmf))) + geom_density()


## 2 Model building ################################################################################

#### a models ----------------------------------------------------------------------------------------
#random structure
m1 <- lmer(rmf ~ species * brickRatio + (1|block), edata, REML = F)
VarCorr(m1)
#4w-model
m2 <- lmer(rmf ~ species * brickRatio * soilType * mycorrhiza +
             (1|block), edata, REML = F)
isSingular(m2)
simulateResiduals(m2, plot = T)
#full 3w-model
m3 <- lmer(rmf ~ (species + brickRatio + soilType + mycorrhiza)^3 +
             (1|block), edata, REML = F)
isSingular(m3)
simulateResiduals(m3, plot = T)
#3w-model reduced
m4 <- lmer(rmf ~ (species + brickRatio + soilType + mycorrhiza)^2 +
             species:brickRatio:soilType + species:brickRatio:mycorrhiza +
             (1|block), edata, REML = F)
isSingular(m4)
simulateResiduals(m4, plot = T)
#2w-model full
m5 <- lmer(rmf ~ (species + brickRatio + soilType + mycorrhiza)^2 +
             (1|block), edata, REML = F)
isSingular(m5)
simulateResiduals(m5, plot = T)
#2w-model reduces
m6 <- lmer(rmf ~ (species + brickRatio + soilType + mycorrhiza) +
             species:brickRatio + species:soilType + species:mycorrhiza +
             (1|block), edata, REML = F)
isSingular(m6)
simulateResiduals(m6, plot = T);
#1w-model full
m7 <- lmer(rmf ~ (species + brickRatio + soilType + mycorrhiza) +
             (1|block), edata, REML = F)
isSingular(m7)
simulateResiduals(m7, plot = T);

#### b comparison -----------------------------------------------------------------------------------------
anova(m2,m3,m4,m5,m6,m7) # --> m7 BUT use m4 because of 3-fold interaction
rm(m1,m2,m3,m5,m6)

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
m4 <- lmer(rmf ~ (species + brickRatio + soilType + mycorrhiza)^2 +
             species:brickRatio:soilType + species:brickRatio:mycorrhiza +
             (1|block), edata, REML = F)
MuMIn::r.squaredGLMM(m4) #R2m = 0.232, R2c = 0.307
VarCorr(m4)
sjPlot::plot_model(m4, type = "re", show.values = T)
car::Anova(m4, type = 3)

### Effect sizes -----------------------------------------------------------------------------------------
(emm <- emmeans(m4, revpairwise ~ brickRatio * soilType | species, type="response"))
plot(emm, comparison = T)
contrast(emmeans(m4, ~ brickRatio * soilType | species, type = "response"), "trt.vs.ctrl", ref = 1)
(emm <- emmeans(m4, revpairwise ~ brickRatio | mycorrhiza | species, type = "response"))
plot(emm, comparison = T)
contrast(emmeans(m4, ~ brickRatio * mycorrhiza | species, type = "response"), "trt.vs.ctrl", ref = 1)
