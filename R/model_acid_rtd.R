# Waste bricks for tree substrates
# Model acid-treatment ~ root tissue density####
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
data <- read_csv("data_processed_acid.csv",
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
                              ),
                          comment = col_factor()
                        )
                  ) %>%
  select(rtd, plot, block, replanted, species, acid, brickRatio,
         acidbrickRatioTreat, soilType)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ##############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration ######################################################

#### a Graphs ---------------------------------------------------------------
#simple effects:
par(mfrow = c(2, 2))
plot(rtd ~ species, data)
plot(rtd ~ soilType, data)
plot(rtd ~ acidbrickRatioTreat, data)
plot(rtd ~ block, data)
#2way (species:soilType):
ggplot(data, aes(species, rtd, color = soilType)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
#2way (species:replanted):
ggplot(data, aes(species, rtd, color = replanted)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)
#3way (acidbrickRatioTreat:soilType):
ggplot(data, aes(acidbrickRatioTreat, rtd)) + facet_grid(~soilType) +
  geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:acid:species):
ggplot(data, aes(acidbrickRatioTreat, rtd)) + facet_grid(~species) +
  geom_boxplot() + geom_quasirandom(dodge.width = .7)
#4way
ggplot(data, aes(soilType, rtd, color = acidbrickRatioTreat)) +
  facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#interactions with block:
ggplot(data, aes(species, rtd, color = acidbrickRatioTreat)) +
  geom_boxplot() + facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(data, aes(acidbrickRatioTreat, rtd)) + geom_boxplot() +
  facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(data, aes(block, rtd, color = soilType)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)

##### b Outliers, zero-inflation, transformations? --------------------------
par(mfrow = c(2, 2))
dotchart((data$rtd),
         groups = factor(data$species), main = "Cleveland dotplot")
dotchart((data$rtd),
         groups = factor(data$soilType), main = "Cleveland dotplot")
dotchart((data$rtd),
         groups = factor(data$brickRatio), main = "Cleveland dotplot")
dotchart((data$rtd), groups = factor(data$acid), main = "Cleveland dotplot")
par(mfrow = c(1, 1))
boxplot(data$rtd)
identify(rep(1, length(data$rtd)), data$rtd, labels = c(data$plot))
plot(table((data$rtd)), type = "h",
     xlab = "Observed values", ylab = "Frequency")
ggplot(data, aes(rtd)) + geom_density()
ggplot(data, aes((1 / rtd))) + geom_density()


## 2 Model building #########################################################

#### a models ---------------------------------------------------------------
#random structure
m1 <- lmer((1 / rtd) ~ species * acidbrickRatioTreat + (1 | block),
           data, REML = FALSE)
VarCorr(m1)
#3w-model
m2 <- lmer((1 / rtd) ~ species * soilType * acidbrickRatioTreat +
             (1 | block), data, REML = FALSE)
isSingular(m2)
simulateResiduals(m2, plot = TRUE)
#full 2w-model
m3 <- lmer((1 / rtd) ~ (species + soilType + acidbrickRatioTreat)^2 +
             (1 | block), data, REML = FALSE)
isSingular(m3)
simulateResiduals(m3, plot = TRUE)
#2w-model reduced
m4 <- lmer((1 / rtd) ~ species + soilType + acidbrickRatioTreat +
             acidbrickRatioTreat:species + acidbrickRatioTreat:soilType +
             (1 | block), data, REML = FALSE)
isSingular(m4)
simulateResiduals(m4, plot = TRUE)

#### b comparison -----------------------------------------------------------
anova(m2, m3, m4)
# --> m3
rm(m1, m2, m4)

#### c model check ----------------------------------------------------------
simulationOutput <- simulateResiduals(m3, plot = TRUE)
par(mfrow = c(2, 2))
plotResiduals(main = "species",
              simulationOutput$scaledResiduals, data$species)
plotResiduals(main = "soilType",
              simulationOutput$scaledResiduals, data$soilType)
plotResiduals(main = "acidbrickRatioTreat",
              simulationOutput$scaledResiduals, data$acidbrickRatioTreat)
plotResiduals(main = "block", simulationOutput$scaledResiduals, data$block)


## 3 Chosen model output ####################################################

### Model output ------------------------------------------------------------
m3 <- lmer((1 / rtd) ~ (species + soilType + acidbrickRatioTreat)^2 +
             (1 | block), data, REML = FALSE)
MuMIn::r.squaredGLMM(m3)
#R2m = 0.124, R2c = 0.210
VarCorr(m3)
sjPlot::plot_model(m3, type = "re", show.values = TRUE)
(table <- car::Anova(m3, type = 3))
tidytable <- broom::tidy(table)

### Effect sizes ------------------------------------------------------------
(emm <- emmeans(m3, revpairwise ~ acidbrickRatioTreat | species,
                type = "response"))
plot(emm, comparison = TRUE)
contrast(emmeans(m3, ~ acidbrickRatioTreat | species,
                 type = "response"), "trt.vs.ctrl", ref = 1)
(emm <- emmeans(m3, revpairwise ~ acidbrickRatioTreat | soilType,
                type = "response"))
plot(emm, comparison = TRUE)

### Save ###
write.csv(tidytable, here("outputs", "statistics",
                          "table_anova_acid_rtd.csv"))
