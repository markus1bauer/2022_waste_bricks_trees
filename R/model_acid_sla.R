# Model for experiment acid and specific leaf area ####



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
edata <- gather(edata, "leaf", "sla", sla1, sla2, sla3, factor_key = T)
edata <- select(edata, leaf, sla, plot, block, replanted, species, acid, brickRatio, acidbrickRatioTreat, soilType)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #####################################################################################

#### a Graphs ---------------------------------------------------------------------------------------------
#simple effects:
par(mfrow = c(2,2))
plot(sla ~ species, edata)
plot(sla ~ soilType, edata)
plot(sla ~ brickRatio, edata)
plot(sla ~ acid, edata)
par(mfrow = c(2,2))
plot(sla ~ block, edata)
#2way (brickRatio:acid):
ggplot(edata, aes(brickRatio, sla, color = acid)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (brickRatio:soilType):
ggplot(edata, aes(soilType, sla,color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (soilType:acid):
ggplot(edata, aes(soilType, sla, color = acid)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:acid:soilType):
ggplot(edata, aes(brickRatio, sla, color = acid)) + facet_grid(~soilType) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:acid:species):
ggplot(edata, aes(brickRatio, sla, color = acid)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#4way
ggplot(edata, aes(soilType, sla, color = brickRatio, shape = acid)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
# interactions with block:
ggplot(edata, aes(species, sla, color = brickRatio, shape = acid)) + geom_boxplot() + facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(edata, aes(brickRatio, sla, color = acid)) + geom_boxplot() + facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(edata, aes(block, sla, color = soilType)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)

##### b Outliers, zero-inflation, transformations? -----------------------------------------------------
par(mfrow = c(2,2))
dotchart((edata$sla), groups = factor(edata$species), main = "Cleveland dotplot")
dotchart((edata$sla), groups = factor(edata$soilType), main = "Cleveland dotplot")
dotchart((edata$sla), groups = factor(edata$brickRatio), main = "Cleveland dotplot")
dotchart((edata$sla), groups = factor(edata$acid), main = "Cleveland dotplot")
par(mfrow=c(1,1));
boxplot(edata$sla, ylim = c(100,400));identify(rep(1, length(edata$sla)), edata$sla, labels = c(edata$plot))
par(mfrow = c(2,2));
plot(table((edata$sla)),type = "h", xlab = "Observed values", ylab = "Frequency")
plot(table(log(edata$sla)), type = "h", xlab = "Observed values", ylab = "Frequency");
ggplot(edata, aes(sla)) + geom_density()
ggplot(edata, aes(log(sla))) + geom_density()


## 2 Model building ################################################################################

#### a models ----------------------------------------------------------------------------------------
#random structure
m1 <- lmer(sla ~ species * acidbrickRatioTreat + (1|block/plot), edata, REML = F)
VarCorr(m1)
#3w-model
m2 <- lmer((sla) ~ species * soilType * acidbrickRatioTreat + 
             (1|block/plot), edata, REML= F)
isSingular(m2)
simulationOutput <- simulateResiduals(m2, plot = T)
#full 2w-model
m3 <- lmer((sla) ~ (species + soilType + acidbrickRatioTreat)^2 +
             (1|block/plot), edata, REML= F)
isSingular(m3)
simulationOutput <- simulateResiduals(m3, plot = T)
#2w-model reduced
m4 <- lmer((sla) ~ species + soilType + acidbrickRatioTreat +
             acidbrickRatioTreat:species + acidbrickRatioTreat:soilType +
             (1|block/plot), edata, REML= F)
isSingular(m4)
simulationOutput <- simulateResiduals(m4, plot = T)

#### b comparison -----------------------------------------------------------------------------------------
anova(m2,m3,m4) # --> m2
rm(m1,m3,m4)

#### c model check -----------------------------------------------------------------------------------------
simulationOutput <- simulateResiduals(m2, plot = F)
par(mfrow=c(2,2));
plotResiduals(main = "species", simulationOutput$scaledResiduals, edata$species)
plotResiduals(main = "soilType", simulationOutput$scaledResiduals,edata$soilType)
plotResiduals(main = "acidbrickRatioTreat", simulationOutput$scaledResiduals, edata$acidbrickRatioTreat)
plotResiduals(main = "block", simulationOutput$scaledResiduals, edata$block)


## 3 Chosen model output ################################################################################

### Model output ---------------------------------------------------------------------------------------------
m2 <- lmer((scaled.sla) ~ species * soilType * acidbrickRatioTreat + 
             (1|block/plot), edata, REML= F)
r.squaredGLMM(m2) #r2m = 0.466, r2c = 0.567
Anova(m2, type = 3)

### Effect sizes -----------------------------------------------------------------------------------------
(emm <- emmeans(m2, revpairwise ~ acidbrickRatioTreat * soilType | species, type = "response"))
plot(emm, comparison = T)
contrast(emmeans(m2, ~ acidbrickRatioTreat * soilType | species, type = "response"), "trt.vs.ctrl", ref = 1)
