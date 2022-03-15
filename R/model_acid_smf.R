# Waste bricks for tree substrates
# Model acid-treatment ~ stem mass fraction ####
# Markus Bauer
# 2022-03-15



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
(data <- read_csv2("data_processed_acid.csv",
                   col_names = TRUE, na = "na", col_types =
                        cols(
                          .default = col_double(),
                          plot = col_factor(),
                          block = col_factor(),
                          replanted = col_factor(),
                          species = col_factor(),
                          mycorrhiza = col_factor(),
                          substrate = col_factor(),
                          soilType = col_factor(levels = c("poor", "rich")),
                          brickRatio = col_factor(levels = c("5", "30")),
                          acid = col_factor(levels = c("Control", "Acid")),
                          acidbrickRatioTreat =
                            col_factor(
                              levels = c("Control_30", "Acid_5", "Acid_30")
                              )
                        )
                   ) %>%
    select(smf, plot, block, replanted, species, acid, brickRatio,
           acidbrickRatioTreat, soilType))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ##############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration ######################################################

#### a Graphs ---------------------------------------------------------------
#simple effects:
par(mfrow = c(2, 2))
plot(smf ~ species, data)
plot(smf ~ soilType, data)
plot(smf ~ acidbrickRatioTreat, data)
plot(smf ~ block, data)
#2way (species:soilType):
ggplot(data, aes(species, smf, color = soilType)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
#2way (species:replanted):
ggplot(data, aes(species, smf, color = replanted)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
#3way (acidbrickRatioTreat:soilType):
ggplot(data, aes(acidbrickRatioTreat, smf)) + facet_grid(~soilType) +
  geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:acid:species):
ggplot(data, aes(acidbrickRatioTreat, smf)) + facet_grid(~species) +
  geom_boxplot() + geom_quasirandom(dodge.width = .7)
#4way
ggplot(data, aes(soilType, smf, color = acidbrickRatioTreat)) +
  facet_grid(~ species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#interactions with block:
ggplot(data, aes(species, smf, color = acidbrickRatioTreat)) +
  geom_boxplot() + facet_wrap(~ block) + geom_quasirandom(dodge.width = .7)
ggplot(data, aes(acidbrickRatioTreat, smf)) + geom_boxplot() +
  facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(data, aes(block, smf, color = soilType)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)

##### b Outliers, zero-inflation, transformations? ---------------------------
par(mfrow = c(2, 2))
dotchart((data$smf), groups = factor(data$species), main = "Cleveland dotplot")
dotchart((data$smf),
         groups = factor(data$soilType), main = "Cleveland dotplot")
dotchart((data$smf),
         groups = factor(data$brickRatio), main = "Cleveland dotplot")
dotchart((data$smf), groups = factor(data$acid), main = "Cleveland dotplot")
par(mfrow = c(1, 1))
boxplot(data$smf)
plot(table((data$smf)), type = "h",
     xlab = "Observed values", ylab = "Frequency")
ggplot(data, aes(smf)) + geom_density()
ggplot(data, aes(log(smf))) + geom_density()


## 2 Model building ##########################################################

#### a models ----------------------------------------------------------------
#random structure --> no random factor needed
m1 <- lmer(smf ~ species * acidbrickRatioTreat + (1 | block),
           data, REML = FALSE)
VarCorr(m1)
#3w-model
m2 <- lm(smf ~ species * soilType * acidbrickRatioTreat, data)
simulateResiduals(m2, plot = TRUE)
#full 2w-model
m3 <- lm(smf ~ (species + soilType + acidbrickRatioTreat)^2, data)
simulateResiduals(m3, plot = TRUE)
#2w-model reduced
m4 <- lm(smf ~ species + soilType + acidbrickRatioTreat +
             acidbrickRatioTreat:species + acidbrickRatioTreat:soilType, data)
simulateResiduals(m4, plot = TRUE)

#### b comparison ------------------------------------------------------------
anova(m2, m3, m4) # --> m4
rm(m1, m2, m3)

#### c model check -----------------------------------------------------------
simulationOutput <- simulateResiduals(m4, plot = TRUE)
par(mfrow = c(2, 2))
plotResiduals(main = "species", simulationOutput$scaledResiduals, data$species)
plotResiduals(main = "soilType",
              simulationOutput$scaledResiduals, data$soilType)
plotResiduals(main = "acidbrickRatioTreat",
              simulationOutput$scaledResiduals, data$acidbrickRatioTreat)
plotResiduals(main = "block", simulationOutput$scaledResiduals, data$block)


## 3 Chosen model output #####################################################

### Model output -------------------------------------------------------------
m4 <- lm(smf ~ species + soilType + acidbrickRatioTreat +
           acidbrickRatioTreat:species + acidbrickRatioTreat:soilType, data)
summary(m4)
#r2 = 0.511, r2a = 0.435
(table <- car::Anova(m4, type = 3))
tidytable <- broom::tidy(table)

### Effect sizes -------------------------------------------------------------
(emm <- emmeans(m4, revpairwise ~ acidbrickRatioTreat | species,
                type = "response"))
plot(emm, comparison = TRUE)
contrast(emmeans(m4, ~ acidbrickRatioTreat | species,
                 type = "response"), "trt.vs.ctrl", ref = 1)
(emm <- emmeans(m4, revpairwise ~ acidbrickRatioTreat | soilType,
                type = "response"))
plot(emm, comparison = TRUE)

### Save ###
write.csv(tidytable, here("outputs", "statistics",
                          "table_anova_acid_smf.csv"))
