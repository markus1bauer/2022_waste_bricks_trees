# Waste bricks for tree substrates
# Model acid-treatment ~ specific leaf area####
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
(data <- read_csv("data_processed_acid.csv",
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
    gather("leaf", "sla", sla1, sla2, sla3, factor_key = TRUE) %>%
    select(leaf, sla, plot, block, replanted, species, acidbrickRatioTreat,
           soilType)
)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ##############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration ######################################################

#### a Graphs ---------------------------------------------------------------
#simple effects:
par(mfrow = c(2, 2))
plot(sla ~ species, data)
plot(sla ~ soilType, data)
plot(sla ~ acidbrickRatioTreat, data)
plot(sla ~ block, data)
#2way (species:soilType):
ggplot(data, aes(species, sla, color = soilType)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
#2way (species:replanted):
ggplot(data, aes(species, sla, color = replanted)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
#3way (acidbrickRatioTreat:soilType):
ggplot(data, aes(acidbrickRatioTreat, sla)) + facet_grid(~soilType) +
  geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (acidbrickRatioTreat:species):
ggplot(data, aes(acidbrickRatioTreat, sla)) + facet_grid(~species) +
  geom_boxplot() + geom_quasirandom(dodge.width = .7)
#4way
ggplot(data, aes(soilType, sla, color = acidbrickRatioTreat)) +
  facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#interactions with block:
ggplot(data, aes(species, sla, color = acidbrickRatioTreat)) +
  geom_boxplot() + facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(data, aes(acidbrickRatioTreat, sla)) + geom_boxplot() +
  facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(data, aes(block, sla, color = soilType)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)

##### b Outliers, zero-inflation, transformations? ---------------------------
par(mfrow = c(2, 2))
dotchart((data$sla),
         groups = factor(data$species), main = "Cleveland dotplot")
dotchart((data$sla),
         groups = factor(data$soilType), main = "Cleveland dotplot")
dotchart((data$sla),
         groups = factor(data$brickRatio), main = "Cleveland dotplot")
dotchart((data$sla), groups = factor(data$acid), main = "Cleveland dotplot")
par(mfrow = c(1, 1))
boxplot(data$sla)
plot(table((data$sla)), type = "h",
     xlab = "Observed values", ylab = "Frequency")
ggplot(data, aes(sla)) + geom_density()
ggplot(data, aes(log(sla))) + geom_density()


## 2 Model building ##########################################################

#### a models ----------------------------------------------------------------
#random structure
m1 <- lmer(sla ~ species * acidbrickRatioTreat + (1 | block / plot),
           data, REML = FALSE)
VarCorr(m1)
#3w-model
m2 <- lmer((sla) ~ species * soilType * acidbrickRatioTreat +
             (1 | block / plot), data, REML = FALSE)
isSingular(m2)
simulateResiduals(m2, plot = TRUE)
#full 2w-model
m3 <- lmer((sla) ~ (species + soilType + acidbrickRatioTreat)^2 +
             (1 | block / plot), data, REML = FALSE)
isSingular(m3)
simulateResiduals(m3, plot = TRUE)
#2w-model reduced
m4 <- lmer((sla) ~ species + soilType + acidbrickRatioTreat +
             acidbrickRatioTreat:species + acidbrickRatioTreat:soilType +
             (1 | block / plot), data, REML = FALSE)
isSingular(m4)
simulateResiduals(m4, plot = TRUE)

#### b comparison -----------------------------------------------------------
anova(m2, m3, m4)
# --> m2
rm(m1, m3, m4)

#### c model check ----------------------------------------------------------
simulationOutput <- simulateResiduals(m2, plot = TRUE)
par(mfrow = c(2, 2))
plotResiduals(main = "species", simulationOutput$scaledResiduals, data$species)
plotResiduals(main = "soilType",
              simulationOutput$scaledResiduals, data$soilType)
plotResiduals(main = "acidbrickRatioTreat",
              simulationOutput$scaledResiduals, data$acidbrickRatioTreat)
plotResiduals(main = "block", simulationOutput$scaledResiduals, data$block)


## 3 Chosen model output #####################################################

### Model output -------------------------------------------------------------
m2 <- lmer(sla ~ species * soilType * acidbrickRatioTreat +
             (1 | block / plot), data, REML = FALSE)
MuMIn::r.squaredGLMM(m2)
#r2m = 0.466, r2c = 0.567
VarCorr(m2)
sjPlot::plot_model(m2, type = "re", show.values = TRUE)
(table <- car::Anova(m2, type = 3))
tidytable <- broom::tidy(table)

### Effect sizes --------------------------------------------------------------
(emm <- emmeans(m2, revpairwise ~ acidbrickRatioTreat * soilType | species,
                type = "response"))
plot(emm, comparison = TRUE)
(emm <- emmeans(m2, revpairwise ~ acidbrickRatioTreat | species,
                type = "response"))
plot(emm, comparison = TRUE)
contrast(emmeans(m2, ~ acidbrickRatioTreat * soilType | species,
                 type = "response"), "trt.vs.ctrl", ref = 1)

### Save ###
write.csv(tidytable, here("outputs", "statistics",
                          "table_anova_acid_sla.csv"))
