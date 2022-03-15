# Model for experiment brick ratio and relative growth rate ####
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
data <- read_csv("data_processed_brickRatio.csv",
                 col_names = TRUE, na = "na", col_types =
                        cols(
                          .default = col_double(),
                          plot = col_factor(),
                          block = col_factor(),
                          date1 = col_date(),
                          date2 = col_date(),
                          date3 = col_date(),
                          replanted = col_factor(),
                          species = col_factor(),
                          mycorrhiza =
                            col_factor(levels = c("Control","Mycorrhiza")),
                          substrate = col_factor(),
                          soilType = col_factor(levels = c("poor","rich")),
                          brickRatio = col_factor(levels = c("5","30")),
                          acid = col_factor(),
                          acidbrickRatioTreat = col_factor()
                          )
                 ) %>%
  select(rgr13, plot, block, replanted, species, brickRatio,
         soilType, mycorrhiza)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ###############################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #######################################################

#### a Graphs ----------------------------------------------------------------
#simple effects:
par(mfrow = c(2, 2))
plot(rgr13 ~ species, data)
plot(rgr13 ~ brickRatio, data)
plot(rgr13 ~ soilType, data)
plot(rgr13 ~ mycorrhiza, data)
plot(rgr13 ~ block, data)
#2way (brickRatio:species):
ggplot(data,aes(species, rgr13, color = brickRatio)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
#2way (brickRatio:soilType):
ggplot(data,aes(soilType, rgr13, color = brickRatio)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
#2way (brickRatio:mycorrhiza):
ggplot(data,aes(mycorrhiza, rgr13, color = brickRatio)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
#2way (species:soilType):
ggplot(data,aes(species, rgr13, color = soilType)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
#2way (species:mycorrhiza):
ggplot(data,aes(species, rgr13, color = mycorrhiza)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
#2way (soilType:mycorrhiza):
ggplot(data,aes(soilType, rgr13, color = mycorrhiza)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
#3way (brickRatio:species:soilType):
ggplot(data,aes(soilType, rgr13, color = brickRatio)) + facet_grid(~species) +
  geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:species:mycorrhiza):
ggplot(data,aes(mycorrhiza, rgr13, color = brickRatio)) +
  facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (species:soilType:mycorrhiza):
ggplot(data,aes(soilType, rgr13, color = mycorrhiza)) + facet_grid(~species) +
  geom_boxplot() + geom_quasirandom(dodge.width = .7)
#4way
ggplot(data,aes(soilType, rgr13, color = brickRatio, shape = mycorrhiza)) +
  facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
# interactions with block:
ggplot(data,aes(brickRatio, rgr13, color = species)) + geom_boxplot() +
  facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(data,aes(block, rgr13, color = species)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
ggplot(data,aes(block, rgr13, color = brickRatio)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
ggplot(data,aes(block, rgr13, color = soilType)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
ggplot(data,aes(block, rgr13, color = mycorrhiza)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)

##### b Outliers, zero-inflation, transformations? ---------------------------
par(mfrow = c(2, 2))
dotchart((data$rgr13),
         groups = factor(data$species), main = "Cleveland dotplot")
dotchart((data$rgr13),
         groups = factor(data$brickRatio), main = "Cleveland dotplot")
dotchart((data$rgr13),
         groups = factor(data$soilType), main = "Cleveland dotplot")
dotchart((data$rgr13),
         groups = factor(data$mycorrhiza), main = "Cleveland dotplot")
par(mfrow=c(1, 1))
boxplot(data$rgr13)
par(mfrow = c(2, 2))
plot(table((data$rgr13)), type = "h",
     xlab = "Observed values", ylab = "Frequency")
ggplot(data, aes(rgr13)) + geom_density()
ggplot(data, aes(log(rgr13))) + geom_density()


## 2 Model building ##########################################################

#### a models ----------------------------------------------------------------
#random structure
m1 <- lmer(rgr13 ~ species * brickRatio + (1|block), data, REML = FALSE)
VarCorr(m1)
#4w-model
m2 <- lmer(rgr13 ~ species * brickRatio * soilType * mycorrhiza +
             (1|block), data, REML = FALSE)
isSingular(m2)
simulateResiduals(m2, plot = TRUE)
#full 3w-model
m3 <- lmer(rgr13 ~ (species + brickRatio + soilType + mycorrhiza)^3 +
             (1|block), data, REML = FALSE)
isSingular(m3)
simulateResiduals(m3, plot = TRUE)
#3w-model reduced
m4 <- lmer(rgr13 ~ (species + brickRatio + soilType + mycorrhiza)^2 +
             species:brickRatio:soilType + species:brickRatio:mycorrhiza +
             (1|block), data, REML = FALSE)
isSingular(m4)
simulateResiduals(m4, plot = TRUE)
#2w-model full
m5 <- lmer(rgr13 ~ (species + brickRatio + soilType + mycorrhiza)^2 +
             (1|block), data, REML = FALSE)
isSingular(m5)
simulateResiduals(m5, plot = TRUE)
#2w-model reduces
m6 <- lmer(rgr13 ~ (species + brickRatio + soilType + mycorrhiza) +
             species:brickRatio + species:soilType + species:mycorrhiza +
             (1|block), data, REML = FALSE)
isSingular(m6)
simulateResiduals(m6, plot = TRUE)

#### b comparison -----------------------------------------------------------
anova(m2, m3, m4, m5, m6)
# --> m6 BUT use m4 because of 3-fold interaction
rm(m1, m2, m3, m5, m6)

#### c model check ----------------------------------------------------------
simulationOutput <- simulateResiduals(m4, plot = TRUE)
par(mfrow = c(2, 2));
plotResiduals(main = "species", simulationOutput$scaledResiduals, data$species)
plotResiduals(main = "brickRatio",
              simulationOutput$scaledResiduals, data$brickRatio)
plotResiduals(main = "soilType",
              simulationOutput$scaledResiduals, data$soilType)
plotResiduals(main = "mycorrhiza",
              simulationOutput$scaledResiduals, data$mycorrhiza)
plotResiduals(main = "block", simulationOutput$scaledResiduals, data$block)


## 3 Chosen model output ####################################################

### Model output ------------------------------------------------------------
m4 <- lmer(rgr13 ~ (species + brickRatio + soilType + mycorrhiza)^2 +
             species:brickRatio:soilType + species:brickRatio:mycorrhiza +
             (1|block), data, REML = FALSE)
MuMIn::r.squaredGLMM(m4)
#R2m = 0.286, R2c = 0.337
VarCorr(m4)
sjPlot::plot_model(m4, type = "re", show.values = TRUE)
(table <- car::Anova(m4, type = 3))
tidytable <- broom::tidy(table)

### Effect sizes ------------------------------------------------------------
(emm <- emmeans(m4, revpairwise ~ brickRatio * soilType | species,
                type = "response"))
plot(emm, comparison = TRUE)
contrast(emmeans(m4, ~ brickRatio * soilType | species,
                 type = "response"), "trt.vs.ctrl", ref = 1)
(emm <- emmeans(m4, revpairwise ~ brickRatio * mycorrhiza | species,
                type = "response"))
plot(emm, comparison = TRUE)
contrast(emmeans(m4, ~ brickRatio * mycorrhiza | species,
                 type = "response"), "trt.vs.ctrl", ref = 1)

### Save ###
write.csv(tidytable, here("outputs", "statistics",
                          "table_anova_soilType_mycorrhiza_rgr.csv"))
