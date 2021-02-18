# Model for experiment with acid treatment and relative growth rate ####
# Markus Bauer
# Citation: Markus Bauer, Martin Krause, Valentin Heizinger & Johannes Kollmann  (2021) ...
# DOI: ...



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
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2021_waste_bricks_trees/data/processed")

### Load data ###
(data <- read_csv2("data_processed_acid.csv", col_names = T, na = "na", col_types = 
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
                   )
  )
data <- select(data, rgr13, plot, block, replanted, species, acidbrickRatioTreat, soilType)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #####################################################################################

#### a Graphs ---------------------------------------------------------------------------------------------
#simple effects:
par(mfrow = c(2,2))
plot(rgr13 ~ species, data)
plot(rgr13 ~ soilType, data)
plot(rgr13 ~ acidbrickRatioTreat, data)
plot(rgr13 ~ block, data)
#2way (species:soilType):
ggplot(data, aes(soilType, rgr13, color = species)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (species:replanted):
ggplot(data, aes(replanted, rgr13, color = species)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (acidbrickRatioTreat:soilType):
ggplot(data, aes(acidbrickRatioTreat, rgr13)) + facet_grid(~soilType) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:acid:species):
ggplot(data, aes(acidbrickRatioTreat, rgr13)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#4way
ggplot(data, aes(soilType, rgr13, color = acidbrickRatioTreat)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#interactions with block:
ggplot(data, aes(species, rgr13, color = acidbrickRatioTreat)) + geom_boxplot() + facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(data, aes(acidbrickRatioTreat, rgr13)) + geom_boxplot() + facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(data, aes(block, rgr13, color = soilType)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)

##### b Outliers, zero-inflation, transformations? -----------------------------------------------------
par(mfrow = c(2,2))
dotchart((data$rgr13), groups = factor(data$species), main = "Cleveland dotplot")
dotchart((data$rgr13), groups = factor(data$soilType), main = "Cleveland dotplot")
dotchart((data$rgr13), groups = factor(data$acidbrickRatioTreat), main = "Cleveland dotplot")
dotchart((data$rgr13), groups = factor(data$block), main = "Cleveland dotplot")
par(mfrow=c(1,1));
boxplot(data$rgr13);#identify(rep(1, length(data$rgr13)), data$rgr13, labels = c(data$no))
par(mfrow = c(2,2));
plot(table((data$rgr13)), type = "h", xlab = "Observed values", ylab = "Frequency")
ggplot(data, aes(rgr13)) + geom_density()
ggplot(data, aes(log(rgr13))) + geom_density()


## 2 Model building ################################################################################

#### a models ----------------------------------------------------------------------------------------
#random structure
m1 <- lmer(rgr13 ~ species * acidbrickRatioTreat + (1|block), data, REML = F)
VarCorr(m1)
#3w-model
m2 <- lmer(rgr13 ~ species * soilType * acidbrickRatioTreat +
             (1|block), data, REML = F)
isSingular(m2)
simulateResiduals(m2, plot = T)
#full 2w-model
m3 <- lmer(rgr13 ~ (species + soilType + acidbrickRatioTreat)^2 +
             (1|block), data, REML = F)
isSingular(m3)
simulateResiduals(m3, plot = T)
#2w-model reduced
m4 <- lmer((rgr13) ~ species + soilType + acidbrickRatioTreat +
             acidbrickRatioTreat:species + acidbrickRatioTreat:soilType +
             (1|block), data, REML = F)
isSingular(m4)
simulateResiduals(m4, plot = T)

#### b comparison -----------------------------------------------------------------------------------------
anova(m2,m3,m4) # --> m4
rm(m1,m2,m3)

#### c model check -----------------------------------------------------------------------------------------
simulationOutput <- simulateResiduals(m4, plot = T)
par(mfrow=c(2,2));
plotResiduals(main = "species", simulationOutput$scaledResiduals, data$species)
plotResiduals(main = "soilType", simulationOutput$scaledResiduals,data$soilType)
plotResiduals(main = "acidbrickRatioTreat", simulationOutput$scaledResiduals, data$acidbrickRatioTreat)
plotResiduals(main = "block", simulationOutput$scaledResiduals, data$block)


## 3 Chosen model output ################################################################################

### Model output ---------------------------------------------------------------------------------------------
m4 <- lmer(rgr13 ~ species + soilType + acidbrickRatioTreat +
             acidbrickRatioTreat:species + acidbrickRatioTreat:soilType +
             (1|block), data, REML = F)
MuMIn::r.squaredGLMM(m4) #R2m = 0.323, R2c = 0.366
VarCorr(m4)
sjPlot::plot_model(m4, type = "re", show.values = T)
car::Anova(m4, type = 3)

### Effect sizes -----------------------------------------------------------------------------------------
(emm <- emmeans(m4, revpairwise ~ acidbrickRatioTreat | species, type = "response"))
plot(emm, comparison = T)
contrast(emmeans(m4, ~ acidbrickRatioTreat | species, type = "response"), "trt.vs.ctrl", ref = 1)
(emm <- emmeans(m4, revpairwise ~ acidbrickRatioTreat | soilType, type = "response"))
plot(emm, comparison = T)
