# Model for experiment mycorrhiza and soil type and specific root length ####



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
edata <- select(edata, srl, plot, block, replanted, species, brickRatio, soilType, mycorrhiza)
#Exclude 2 outlier
edata <- filter(edata, srl > -1000)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #####################################################################################

#### a Graphs ---------------------------------------------------------------------------------------------
#simple effects:
par(mfrow = c(2,2))
plot(srl ~ species, edata)
plot(srl ~ brickRatio, edata)
plot(srl ~ soilType, edata)
plot(srl ~ mycorrhiza, edata)
par(mfrow = c(2,2))
plot(srl ~ block, edata)
#2way (brickRatio:species):
ggplot(edata, aes(species, srl, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (brickRatio:soilType):
ggplot(edata, aes(soilType, srl, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (brickRatio:mycorrhiza):
ggplot(edata, aes(mycorrhiza, srl, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (species:soilType):
ggplot(edata, aes(species, srl, color = soilType)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (species:mycorrhiza):
ggplot(edata, aes(species, srl, color = mycorrhiza)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (soilType:mycorrhiza):
ggplot(edata, aes(soilType, srl, color = mycorrhiza)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (species:replanted):
ggplot(edata, aes(species, srl, color = replanted)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:species:soilType):
ggplot(edata, aes(soilType, srl, color = brickRatio)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:species:mycorrhiza):
ggplot(edata, aes(mycorrhiza, srl, color = brickRatio)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (species:soilType:mycorrhiza):
ggplot(edata, aes(soilType, srl, color = mycorrhiza)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#4way
ggplot(edata,aes(soilType, srl, color = brickRatio, shape = mycorrhiza)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#interactions with block:
ggplot(edata,aes(brickRatio, srl, color = species)) + geom_boxplot() + facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(block, srl, color = species)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(block, srl, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(block, srl, color = soilType)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(block, srl, color = mycorrhiza)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)

##### b Outliers, zero-inflation, transformations? -----------------------------------------------------
par(mfrow = c(2,2))
dotchart((edata$srl), groups = factor(edata$species), main = "Cleveland dotplot")
dotchart((edata$srl), groups = factor(edata$brickRatio), main = "Cleveland dotplot")
dotchart((edata$srl), groups = factor(edata$soilType), main = "Cleveland dotplot")
dotchart((edata$srl), groups = factor(edata$mycorrhiza), main = "Cleveland dotplot")
dotchart((edata$srl), groups = factor(edata$block), main = "Cleveland dotplot")
par(mfrow=c(1,1));
boxplot(edata$srl);
identify(rep(1, length(edata$srl)), edata$srl, labels = c(edata$plot))
plot(table((edata$srl)), type = "h", xlab = "Observed values", ylab = "Frequency")
ggplot(edata, aes(srl)) + geom_density()
ggplot(edata, aes(log(srl))) + geom_density()


## 2 Model building ################################################################################

#### a models ----------------------------------------------------------------------------------------
#random structure
m1 <- lmer(log(srl) ~ species * brickRatio + (1|block), edata, REML = F)
VarCorr(m1)
#full-model
m2 <- lmer(log(srl) ~ species * brickRatio * soilType * mycorrhiza +
             (1|block), edata, REML = F)
isSingular(m2)
simulateResiduals(m2, plot = T)
#full 3w-model
m3 <- lmer(log(srl) ~ (species + brickRatio + soilType + mycorrhiza)^3 +
             (1|block), edata, REML = F)
isSingular(m3)
simulateResiduals(m3, plot = T)
#3w-model reduced
m4 <- lmer(log(srl) ~ (species + brickRatio + soilType + mycorrhiza)^2 +
             species:brickRatio:soilType + species:brickRatio:mycorrhiza +
             (1|block), edata, REML = F)
isSingular(m4)
simulateResiduals(m4, plot = T)
#2w-model full
m5 <- lmer(log(srl) ~ (species + brickRatio + soilType + mycorrhiza)^2 +
             (1|block), edata, REML = F)
isSingular(m5)
simulateResiduals(m5, plot = T)
#2w-model reduces
m6 <- lmer(log(srl) ~ (species + brickRatio + soilType + mycorrhiza) +
             species:brickRatio + species:soilType + species:mycorrhiza +
             (1|block), edata, REML = F)
isSingular(m6)
simulateResiduals(m6, plot = T);
#1w-model full
m7 <- lmer(log(srl) ~ (species + brickRatio + soilType + mycorrhiza) +
             (1|block), edata, REML = F)
isSingular(m7)
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
m4 <- lmer(log(srl) ~ (species + brickRatio + soilType + mycorrhiza)^2 +
             species:brickRatio:soilType + species:brickRatio:mycorrhiza +
             (1|block), edata, REML = F)
MuMIn::r.squaredGLMM(m4) #R2m = 0.466, R2c = 0.499
VarCorr(m4)
sjPlot::plot_model(m4, type = "re", show.values = T)
car::Anova(m4, type = 3)

### Effect sizes -----------------------------------------------------------------------------------------
(emm <- emmeans(m4, revpairwise ~ brickRatio * soilType | species, type="response"))
plot(emm, comparison = T)
contrast(emmeans(m4, ~ brickRatio * soilType | species, type = "response"), "trt.vs.ctrl", ref = 1)
(emm <- emmeans(m4, revpairwise ~ brickRatio * mycorrhiza | species, type = "response"))
plot(emm, comparison = T)
contrast(emmeans(m4, ~ brickRatio * mycorrhiza | species, type = "response"), "trt.vs.ctrl", ref = 1)
(emm <- emmeans(m4, revpairwise ~ soilType * mycorrhiza | species, type = "response"))
plot(emm, comparison = T)
