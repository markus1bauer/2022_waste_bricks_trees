# Waste bricks for tree substrates
# Model absorptive-transport fine roots ratio ~ soil type * mycorrhiza ####
# Markus Bauer
# 2022-03-15



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
(data <- read_csv2("data_processed_brickRatio.csv",
                   col_names = TRUE, na = "na", col_types =
                        cols(
                          .default = col_double(),
                          plot = col_factor(),
                          block = col_factor(),
                          replanted = col_factor(),
                          species = col_factor(),
                          mycorrhiza =
                            col_factor(levels = c("Control", "Mycorrhiza")),
                          substrate = col_factor(),
                          soilType = col_factor(levels = c("poor", "rich")),
                          brickRatio = col_factor(levels = c("5", "30")),
                          acid = col_factor(levels = c("Acid")),
                          acidbrickRatioTreat = col_factor()
                        )
                   ) %>%
    select(abstransRatio, plot, block, replanted, species, brickRatio,
           soilType, mycorrhiza) %>%
#Exclude 1 outlier
    filter(abstransRatio < 6)
  )



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ##############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration ######################################################

#### a Graphs ---------------------------------------------------------------
#simple effects:
par(mfrow = c(2, 2))
plot(abstransRatio ~ species, data)
plot(abstransRatio ~ brickRatio, data)
plot(abstransRatio ~ soilType, data)
plot(abstransRatio ~ mycorrhiza, data)
plot(abstransRatio ~ block, data)
#2way (brickRatio:species):
ggplot(data, aes(species, abstransRatio, color = brickRatio)) +
  geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (brickRatio:soilType):
ggplot(data, aes(soilType, abstransRatio, color = brickRatio)) +
  geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (brickRatio:mycorrhiza):
ggplot(data, aes(mycorrhiza, abstransRatio, color = brickRatio)) +
  geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (species:soilType):
ggplot(data, aes(species, abstransRatio, color = soilType)) +
  geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (species:mycorrhiza):
ggplot(data, aes(species, abstransRatio, color = mycorrhiza)) +
  geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (soilType:mycorrhiza):
ggplot(data, aes(soilType, abstransRatio, color = mycorrhiza)) +
  geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (species:replanted):
ggplot(data, aes(species, abstransRatio, color = replanted)) +
  geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:species:soilType):
ggplot(data, aes(soilType, abstransRatio, color = brickRatio)) +
  facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:species:mycorrhiza):
ggplot(data, aes(mycorrhiza, abstransRatio, color = brickRatio)) +
  facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (species:soilType:mycorrhiza):
ggplot(data, aes(soilType, abstransRatio, color = mycorrhiza)) +
  facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#4way
ggplot(data,aes(soilType, abstransRatio,
                color = brickRatio, shape = mycorrhiza)) +
  facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#interactions with block:
ggplot(data,aes(brickRatio, abstransRatio, color = species)) + geom_boxplot() +
  facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(data,aes(block, abstransRatio, color = species)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
ggplot(data,aes(block, abstransRatio, color = brickRatio)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
ggplot(data,aes(block, abstransRatio, color = soilType)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
ggplot(data,aes(block, abstransRatio, color = mycorrhiza)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)

##### b Outliers, zero-inflation, transformations? --------------------------
par(mfrow = c(2, 2))
dotchart((data$abstransRatio),
         groups = factor(data$species), main = "Cleveland dotplot")
dotchart((data$abstransRatio),
         groups = factor(data$brickRatio), main = "Cleveland dotplot")
dotchart((data$abstransRatio),
         groups = factor(data$soilType), main = "Cleveland dotplot")
dotchart((data$abstransRatio),
         groups = factor(data$mycorrhiza), main = "Cleveland dotplot")
dotchart((data$abstransRatio),
         groups = factor(data$block), main = "Cleveland dotplot")
par(mfrow = c(1, 1))
boxplot(data$abstransRatio)
identify(rep(1, length(data$abstransRatio)),
         data$abstransRatio, labels = c(data$plot))
plot(table((data$abstransRatio)), type = "h",
     xlab = "Observed values", ylab = "Frequency")
ggplot(data, aes(abstransRatio)) + geom_density()
ggplot(data, aes(sqrt(abstransRatio))) + geom_density()


## 2 Model building #########################################################

#### a models ---------------------------------------------------------------
#random structure
m1 <- lmer(abstransRatio ~ species * brickRatio + (1 | block),
           data, REML = FALSE)
VarCorr(m1)
#4w-model
m2 <- lmer(log(abstransRatio + 1) ~
             species * brickRatio * soilType * mycorrhiza +
             (1 | block), data, REML = FALSE)
isSingular(m2)
simulateResiduals(m2, plot = TRUE)
#full 3w-model
m3 <- lmer(log(abstransRatio + 1) ~
             (species + brickRatio + soilType + mycorrhiza)^3 +
             (1 | block), data, REML = FALSE)
isSingular(m3)
simulateResiduals(m3, plot = TRUE)
#3w-model reduced
m4 <- lmer(log(abstransRatio + 1) ~
             (species + brickRatio + soilType + mycorrhiza)^2 +
             species:brickRatio:soilType + species:brickRatio:mycorrhiza +
             (1 | block), data, REML = FALSE)
isSingular(m4)
simulateResiduals(m4, plot = TRUE)
#2w-model full
m5 <- lmer(log(abstransRatio + 1) ~
             (species + brickRatio + soilType + mycorrhiza)^2 +
             (1 | block), data, REML = FALSE)
isSingular(m5)
simulateResiduals(m5, plot = TRUE)
#2w-model reduced
m6 <- lmer(log(abstransRatio + 1) ~
             (species + brickRatio + soilType + mycorrhiza) +
             species:brickRatio + species:soilType + species:mycorrhiza +
             (1 | block), data, REML = FALSE)
isSingular(m6)
simulateResiduals(m6, plot = TRUE)
#1w-model full
m7 <- lmer(log(abstransRatio + 1) ~
             (species + brickRatio + soilType + mycorrhiza) +
             (1 | block), data, REML = FALSE)
isSingular(m7)
simulateResiduals(m7, plot = TRUE)

#### b comparison -----------------------------------------------------------
anova(m2, m3, m4, m5, m6, m7)
# --> m7 BUT use m4 because of 3-fold interaction
rm(m1, m2, m3, m5, m6, m7)

#### c model check ----------------------------------------------------------
simulationOutput <- simulateResiduals(m4, plot = TRUE)
par(mfrow = c(2, 2))
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
m4 <- lmer(log(abstransRatio + 1) ~
             (species + brickRatio + soilType + mycorrhiza)^2 +
             species:brickRatio:soilType + species:brickRatio:mycorrhiza +
             (1 | block), data, REML = FALSE)
MuMIn::r.squaredGLMM(m4)
#R2m = 0.388, R2c = 0.502
VarCorr(m4)
sjPlot::plot_model(m4, type = "re", show.values = TRUE)
(table <- car::Anova(m4, type = 3))
tidytable <- broom::tidy(table)

### Effect sizes -------------------------------------------------------------
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
write.csv(tidytable,
          here("outputs", "statistics",
               "table_anova_soilType_mycorrhiza_abstransRatio.csv"))
