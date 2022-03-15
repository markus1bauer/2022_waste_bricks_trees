# Model for experiment mycorrhiza and soil type and stem mass fraction ####
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
library(lme4)
library(lmerTest)
library(DHARMa)
library(emmeans)

### Start ###
rm(list = ls())
setwd(here("data", "processed"))

### Load data ###
(data <- read_csv("data_processed_brickRatio.csv",
                   col_names = TRUE, na = "na", col_types =
                        cols(
                          .default = col_double(),
                          plot = col_factor(),
                          block = col_factor(),
                          replanted = col_factor(),
                          species = col_factor(),
                          mycorrhiza =
                            col_factor(levels = c("Control","Mycorrhiza")),
                          substrate = col_factor(),
                          soilType = col_factor(levels = c("poor","rich")),
                          brickRatio = col_factor(levels = c("5","30")),
                          acid = col_factor(levels = c("Acid")),
                          acidbrickRatioTreat = col_factor()
                        )
                   ) %>%
    select(smf, plot, block, replanted, species, brickRatio, soilType,
           mycorrhiza)
)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ###############################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #######################################################

#### a Graphs ----------------------------------------------------------------
#simple effects:
par(mfrow = c(2,2))
plot(smf ~ species, data)
plot(smf ~ brickRatio, data)
plot(smf ~ soilType, data)
plot(smf ~ mycorrhiza, data)
par(mfrow = c(2,2))
plot(smf ~ block, data)
#2way (brickRatio:species):
ggplot(data, aes(species, smf, color = brickRatio)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
#2way (brickRatio:soilType):
ggplot(data, aes(soilType, smf, color = brickRatio)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
#2way (brickRatio:mycorrhiza):
ggplot(data, aes(mycorrhiza, smf, color = brickRatio)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
#2way (species:soilType):
ggplot(data, aes(species, smf, color = soilType)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
#2way (species:mycorrhiza):
ggplot(data, aes(species, smf, color = mycorrhiza)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
#2way (soilType:mycorrhiza):
ggplot(data, aes(soilType, smf, color = mycorrhiza)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
#3way (brickRatio:species:soilType):
ggplot(data, aes(soilType, smf, color = brickRatio)) + facet_grid(~species) +
  geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:species:mycorrhiza):
ggplot(data, aes(mycorrhiza, smf, color = brickRatio)) + facet_grid(~species) +
  geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (species:soilType:mycorrhiza):
ggplot(data, aes(soilType, smf, color = mycorrhiza)) + facet_grid(~species) +
  geom_boxplot() + geom_quasirandom(dodge.width = .7)
#4way
ggplot(data,aes(soilType, smf, color = brickRatio, shape = mycorrhiza)) +
  facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
# interactions with block:
ggplot(data,aes(brickRatio, smf, color = species)) + geom_boxplot() +
  facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(data,aes(block, smf, color = species)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
ggplot(data,aes(block, smf, color = brickRatio)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
ggplot(data,aes(block, smf, color = soilType)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
ggplot(data,aes(block, smf, color = mycorrhiza)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)

##### b Outliers, zero-inflation, transformations? ---------------------------
par(mfrow = c(2,2))
dotchart((data$smf), groups = factor(data$species), main = "Cleveland dotplot")
dotchart((data$smf), groups = factor(data$brickRatio), main = "Cleveland dotplot")
dotchart((data$smf), groups = factor(data$soilType), main = "Cleveland dotplot")
dotchart((data$smf), groups = factor(data$mycorrhiza), main = "Cleveland dotplot")
dotchart((data$smf), groups = factor(data$block), main = "Cleveland dotplot")
par(mfrow=c(1,1));
boxplot(data$smf);#identify(rep(1, length(data$smf)), data$smf, labels = c(data$no))
plot(table((data$smf)), type = "h", xlab = "Observed values", ylab = "Frequency")
ggplot(data, aes(smf)) + geom_density()
ggplot(data, aes(log(smf))) + geom_density()


## 2 Model building ##########################################################

#### a models ----------------------------------------------------------------
#random structure
m1 <- lmer(smf ~ species * brickRatio + (1|block), data, REML = FALSE)
VarCorr(m1)
#4w-model
m2 <- lmer(smf ~ species * brickRatio * soilType * mycorrhiza +
             (1|block), data, REML = FALSE)
isSingular(m2)
simulateResiduals(m2, plot = TRUE)
#full 3w-model
m3 <- lmer(smf ~ (species + brickRatio + soilType + mycorrhiza)^3 +
             (1|block), data, REML = FALSE)
isSingular(m3)
simulateResiduals(m3, plot = TRUE)
#3w-model reduced
m4 <- lmer(smf ~ (species + brickRatio + soilType + mycorrhiza)^2 +
             species:brickRatio:soilType + species:brickRatio:mycorrhiza +
             (1|block), data, REML = FALSE)
isSingular(m4)
simulateResiduals(m4, plot = TRUE)
#2w-model full
m5 <- lmer(smf ~ (species + brickRatio + soilType + mycorrhiza)^2 +
             (1|block), data, REML = FALSE)
isSingular(m5)
simulateResiduals(m5, plot = TRUE)
#2w-model reduces
m6 <- lmer(smf ~ (species + brickRatio + soilType + mycorrhiza) +
             species:brickRatio + species:soilType + species:mycorrhiza +
             (1|block), data, REML = FALSE)
isSingular(m6)
simulateResiduals(m6, plot = TRUE)
#1w-model full
m7 <- lmer(smf ~ (species + brickRatio + soilType + mycorrhiza) +
             (1|block), data, REML = FALSE)
isSingular(m7)
simulateResiduals(m7, plot = TRUE)

#### b comparison ------------------------------------------------------------
anova(m2,m3,m4,m5,m6,m7)
# --> m7 BUT use m4 because of 3-fold interaction
rm(m1,m2,m3,m5,m6,m7)

#### c model check -----------------------------------------------------------
simulationOutput <- simulateResiduals(m4, plot = TRUE)
par(mfrow=c(2,2))
plotResiduals(main = "species",
              simulationOutput$scaledResiduals, data$species)
plotResiduals(main = "brickRatio",
              simulationOutput$scaledResiduals, data$brickRatio)
plotResiduals(main = "soilType",
              simulationOutput$scaledResiduals, data$soilType)
plotResiduals(main = "mycorrhiza",
              simulationOutput$scaledResiduals, data$mycorrhiza)
plotResiduals(main = "block", simulationOutput$scaledResiduals, data$block)


## 3 Chosen model output #####################################################

### Model output -------------------------------------------------------------
m4 <- lmer(smf ~ (species + brickRatio + soilType + mycorrhiza)^2 +
             species:brickRatio:soilType + species:brickRatio:mycorrhiza +
             (1|block), data, REML = F)
MuMIn::r.squaredGLMM(m4)
#R2m = 0.474, R2c = 0.525
VarCorr(m4)
sjPlot::plot_model(m4, type = "re", show.values = TRUE)
car::Anova(m4, type = 3)

### Effect sizes -------------------------------------------------------------
(emm <- emmeans(m4, revpairwise ~ brickRatio * soilType | species,
                type="response"))
plot(emm, comparison = TRUE)
contrast(emmeans(m4, ~ brickRatio * soilType | species,
                 type = "response"), "trt.vs.ctrl", ref = 1)
(emm <- emmeans(m4, revpairwise ~ brickRatio | mycorrhiza | species,
                type = "response"))
plot(emm, comparison = TRUE)
contrast(emmeans(m4, ~ brickRatio * mycorrhiza | species,
                 type = "response"), "trt.vs.ctrl", ref = 1)
