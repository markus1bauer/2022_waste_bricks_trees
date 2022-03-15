# Model for experiment acid and specific root length ####
# Markus Bauer
# Citation: Markus Bauer, Martin Krause, Valentin Heizinger & Johannes Kollmann  (2021) ...
# DOI: ...



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ##############################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(here)
library(tidyverse)
library(ggbeeswarm)
library(lmerTest)
library(DHARMa)
library(emmeans)

### Start ###
rm(list = ls())
setwd(here("data", "processed"))

### Load data ###
data <- read_csv2("data_processed_acid.csv", col_names = T, na = "na", col_types = 
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
data <- select(data, srl, plot, block, replanted, species, acid, brickRatio, acidbrickRatioTreat, soilType)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #####################################################################################

#### a Graphs ---------------------------------------------------------------------------------------------
#simple effects:
par(mfrow = c(2,2))
plot(srl ~ species, data)
plot(srl ~ soilType, data)
plot(srl ~ acidbrickRatioTreat, data)
plot(srl ~ block, data)
#2way (species:soilType):
ggplot(data, aes(species, srl, color = soilType)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (species:replanted):
ggplot(data, aes(species, srl, color = replanted)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (species:replanted):
ggplot(data, aes(species, srl, color = replanted)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (acidbrickRatioTreat:soilType):
ggplot(data, aes(acidbrickRatioTreat, srl)) + facet_grid(~soilType) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:acid:species):
ggplot(data, aes(acidbrickRatioTreat, srl)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#4way
ggplot(data, aes(soilType, srl, color = acidbrickRatioTreat)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#interactions with block:
ggplot(data, aes(species, srl, color = acidbrickRatioTreat)) + geom_boxplot() + facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(data, aes(acidbrickRatioTreat, srl)) + geom_boxplot() + facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(data, aes(block, srl, color = soilType)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)

##### b Outliers, zero-inflation, transformations? -----------------------------------------------------
par(mfrow = c(2,2))
dotchart((data$srl), groups = factor(data$species), main = "Cleveland dotplot")
dotchart((data$srl), groups = factor(data$soilType), main = "Cleveland dotplot")
dotchart((data$srl), groups = factor(data$brickRatio), main = "Cleveland dotplot")
dotchart((data$srl), groups = factor(data$acid), main = "Cleveland dotplot")
par(mfrow=c(1,1));
boxplot(data$srl);#identify(rep(1,length(data$srl)),data$srl, labels = c(data$no))
plot(table((data$srl)), type = "h", xlab = "Observed values", ylab = "Frequency")
ggplot(data, aes(srl)) + geom_density()
ggplot(data, aes(log(srl))) + geom_density()


## 2 Model building ################################################################################

#### a models ----------------------------------------------------------------------------------------
#random structure
m1 <- lmer(log(srl) ~ species * acidbrickRatioTreat + (1|block), data, REML = F)
VarCorr(m1)
#3w-model
m2 <- lmer(log(srl) ~ species * soilType * acidbrickRatioTreat +
             (1|block), data, REML = F)
isSingular(m2)
simulateResiduals(m2, plot = T)
#full 2w-model
m3 <- lmer(log(srl) ~ (species + soilType + acidbrickRatioTreat)^2 +
             (1|block), data, REML = F)
isSingular(m3)
simulateResiduals(m3, plot = T)
#2w-model reduced
m4 <- lmer(log(srl) ~ species + soilType + acidbrickRatioTreat +
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
m4 <- lmer(log(srl) ~ species + soilType + acidbrickRatioTreat +
             acidbrickRatioTreat:species + acidbrickRatioTreat:soilType +
             (1|block), data, REML = F)
MuMIn::r.squaredGLMM(m4) #R2m = 0.502, R2c = 0.524
VarCorr(m4)
sjPlot::plot_model(m4, type = "re", show.values = T)
car::Anova(m4, type = 3)

### Effect sizes -----------------------------------------------------------------------------------------
(emm <- emmeans(m4, revpairwise ~ acidbrickRatioTreat | species, type = "response"))
plot(emm, comparison = T)
contrast(emmeans(m4, ~ acidbrickRatioTreat | species, type = "response"), "trt.vs.ctrl", ref = 1)
(emm <- emmeans(m4, revpairwise ~ acidbrickRatioTreat | soilType, type = "response"))
plot(emm, comparison = T)
