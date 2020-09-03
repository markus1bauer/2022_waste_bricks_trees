# Model for experiment acid and specific root length ####



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
edata <- read_table2("data_processed_acid.txt", col_names = T, na = "na", col_types = 
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
                          acidbrickRatioTreat = col_factor(levels = c("Control_30","Acid_5","Acid_30")),
                          comment = col_factor()
                        )        
)
edata <- select(edata, srl, plot, block, replanted, species, acid, brickRatio, acidbrickRatioTreat, soilType)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #####################################################################################

#### a Graphs ---------------------------------------------------------------------------------------------
#simple effects:
par(mfrow = c(2,2))
plot(srl ~ species, edata)
plot(srl ~ soilType, edata)
plot(srl ~ acidbrickRatioTreat, edata)
plot(srl ~ block, edata)
#2way (species:soilType):
ggplot(edata, aes(species, srl, color = soilType)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (species:replanted):
ggplot(edata, aes(species, srl, color = replanted)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (species:replanted):
ggplot(edata, aes(species, srl, color = replanted)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (acidbrickRatioTreat:soilType):
ggplot(edata, aes(acidbrickRatioTreat, srl)) + facet_grid(~soilType) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:acid:species):
ggplot(edata, aes(acidbrickRatioTreat, srl)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#4way
ggplot(edata, aes(soilType, srl, color = acidbrickRatioTreat)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#interactions with block:
ggplot(edata, aes(species, srl, color = acidbrickRatioTreat)) + geom_boxplot() + facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(edata, aes(acidbrickRatioTreat, srl)) + geom_boxplot() + facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(edata, aes(block, srl, color = soilType)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)

##### b Outliers, zero-inflation, transformations? -----------------------------------------------------
par(mfrow = c(2,2))
dotchart((edata$srl), groups = factor(edata$species), main = "Cleveland dotplot")
dotchart((edata$srl), groups = factor(edata$soilType), main = "Cleveland dotplot")
dotchart((edata$srl), groups = factor(edata$brickRatio), main = "Cleveland dotplot")
dotchart((edata$srl), groups = factor(edata$acid), main = "Cleveland dotplot")
par(mfrow=c(1,1));
boxplot(edata$srl);#identify(rep(1,length(edata$srl)),edata$srl, labels = c(edata$no))
plot(table((edata$srl)), type = "h", xlab = "Observed values", ylab = "Frequency")
ggplot(edata, aes(srl)) + geom_density()
ggplot(edata, aes(log(srl))) + geom_density()


## 2 Model building ################################################################################

#### a models ----------------------------------------------------------------------------------------
#random structure
m1 <- lmer(log(srl) ~ species * acidbrickRatioTreat + (1|block), edata, REML = F)
VarCorr(m1)
#3w-model
m2 <- lmer(log(srl) ~ species * soilType * acidbrickRatioTreat +
             (1|block), edata, REML = F)
isSingular(m2)
simulateResiduals(m2, plot = T)
#full 2w-model
m3 <- lmer(log(srl) ~ (species + soilType + acidbrickRatioTreat)^2 +
             (1|block), edata, REML = F)
isSingular(m3)
simulateResiduals(m3, plot = T)
#2w-model reduced
m4 <- lmer(log(srl) ~ species + soilType + acidbrickRatioTreat +
             acidbrickRatioTreat:species + acidbrickRatioTreat:soilType +
             (1|block), edata, REML = F)
isSingular(m4)
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
m4 <- lmer(log(srl) ~ species + soilType + acidbrickRatioTreat +
             acidbrickRatioTreat:species + acidbrickRatioTreat:soilType +
             (1|block), edata, REML = F)
MuMIn::r.squaredGLMM(m4) #R2m = 0.559, R2c = 0.590
VarCorr(m4)
sjPlot::plot_model(m4, type = "re", show.values = T)
car::Anova(m4, type = 3)

### Effect sizes -----------------------------------------------------------------------------------------
(emm <- emmeans(m4, revpairwise ~ acidbrickRatioTreat | species, type = "response"))
plot(emm, comparison = T)
contrast(emmeans(m4, ~ acidbrickRatioTreat | species, type = "response"), "trt.vs.ctrl", ref = 1)
(emm <- emmeans(m4, revpairwise ~ acidbrickRatioTreat | soilType, type = "response"))
plot(emm, comparison = T)
