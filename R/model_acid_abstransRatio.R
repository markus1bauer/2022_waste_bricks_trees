# Waste bricks for tree substrates
# Model acid-treatment ~ absorptive-transport fine roots ratio####
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
    select(abstransRatio, plot, block, replanted, species, acidbrickRatioTreat,
           soilType) %>%
#Exclude 1 outlier
    filter(abstransRatio < 6)
)



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ###############################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #######################################################

#### a Graphs ----------------------------------------------------------------
#simple effects:
par(mfrow = c(2, 2))
plot(abstransRatio ~ species, data)
plot(abstransRatio ~ soilType, data)
plot(abstransRatio ~ acidbrickRatioTreat, data)
plot(abstransRatio ~ block, data)
#2way (soilType:species):
ggplot(data, aes(soilType, abstransRatio)) + facet_grid(~species) +
  geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (replanted:species):
ggplot(data, aes(replanted, abstransRatio)) + facet_grid(~species) +
  geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (acidbrickRatioTreat:soilType):
ggplot(data, aes(acidbrickRatioTreat, abstransRatio)) +
  facet_grid(~soilType) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (acidbrickRatioTreat:species):
ggplot(data, aes(acidbrickRatioTreat, abstransRatio)) + facet_grid(~species) +
  geom_boxplot() + geom_quasirandom(dodge.width = .7)
#4way
ggplot(data, aes(soilType, abstransRatio, color = acidbrickRatioTreat)) +
  facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#interactions with block:
ggplot(data, aes(species, abstransRatio, color = acidbrickRatioTreat)) +
  geom_boxplot() + facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(data, aes(acidbrickRatioTreat, abstransRatio)) + geom_boxplot() +
  facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(data, aes(block, abstransRatio, color = soilType)) + geom_boxplot() +
  geom_quasirandom(dodge.width = .7)

##### b Outliers, zero-inflation, transformations? ---------------------------
par(mfrow = c(2, 2))
dotchart((data$abstransRatio),
         groups = factor(data$species), main = "Cleveland dotplot")
dotchart((data$abstransRatio),
         groups = factor(data$soilType), main = "Cleveland dotplot")
dotchart((data$abstransRatio),
         groups = factor(data$acidbrickRatioTreat), main = "Cleveland dotplot")
dotchart((data$abstransRatio),
         groups = factor(data$block), main = "Cleveland dotplot")
par(mfrow = c(1, 1))
boxplot(data$abstransRatio)
identify(rep(1, length(data$abstransRatio)),
         data$abstransRatio, labels = c(data$plot))
data <- filter(data, abstransRatio < 6)
plot(table((data$abstransRatio)), type = "h",
     xlab = "Observed values", ylab = "Frequency")
ggplot(data, aes(abstransRatio)) + geom_density()
ggplot(data, aes(sqrt(abstransRatio))) + geom_density()


## 2 Model building ##########################################################

#### a models ----------------------------------------------------------------
#random structure --> no random factor needed
m1 <- lmer(log(abstransRatio) ~ species * acidbrickRatioTreat + (1 | block),
           data, REML = FALSE)
VarCorr(m1)
#full-model
m2 <- lmer(log(abstransRatio) ~ species * soilType * acidbrickRatioTreat +
             (1 | block), data, REML = FALSE)
isSingular(m2)
simulateResiduals(m2, plot = TRUE)
#full 2w-model
m3 <- lmer(log(abstransRatio) ~ (species + soilType + acidbrickRatioTreat)^2 +
             (1 | block), data, REML = FALSE)
isSingular(m3)
simulateResiduals(m3, plot = TRUE)
#2w-model reduced
m4 <- lmer(log(abstransRatio) ~ species + soilType + acidbrickRatioTreat +
             acidbrickRatioTreat:species + acidbrickRatioTreat:soilType +
             (1 | block), data, REML = FALSE)
isSingular(m4)
simulateResiduals(m4, plot = TRUE)


#### b comparison ------------------------------------------------------------
anova(m2, m3, m4)
# --> m3
rm(m1, m2, m4)

#### c model check -----------------------------------------------------------
simulationOutput <- simulateResiduals(m3, plot = TRUE)
par(mfrow = c(2, 2))
plotResiduals(main = "species",
              simulationOutput$scaledResiduals, data$species)
plotResiduals(main = "soilType",
              simulationOutput$scaledResiduals, data$soilType)
plotResiduals(main = "acidbrickRatioTreat",
              simulationOutput$scaledResiduals, data$acidbrickRatioTreat)
plotResiduals(main = "block", simulationOutput$scaledResiduals, data$block)


## 3 Chosen model output #####################################################

### Model output -------------------------------------------------------------
m3 <- lmer(log(abstransRatio) ~ (species + soilType + acidbrickRatioTreat)^2 +
             (1 | block), data, REML = FALSE)
MuMIn::r.squaredGLMM(m3)
# R2m = 0.421, R2c = 0.512
VarCorr(m3)
sjPlot::plot_model(m3, type = "re", show.values = TRUE)
(table <- car::Anova(m3, type = 3))
tidytable <- broom::tidy(table)

### Effect sizes -------------------------------------------------------------
(emm <- emmeans(m3, revpairwise ~ acidbrickRatioTreat | species,
                type = "response"))
plot(emm, comparison = TRUE)
contrast(emmeans(m3, ~ acidbrickRatioTreat | species,
                 type = "response"), "trt.vs.ctrl", ref = 1)
(emm <- emmeans(m3, revpairwise ~ soilType | species,
                type = "response"))
plot(emm, comparison = TRUE)

### Save ###
write.csv(tidytable, here("outputs", "statistics",
                          "table_anova_acid_abstransRatio.csv"))
