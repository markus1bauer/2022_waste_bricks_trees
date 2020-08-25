# Model for experiment acid and absorptive vs. transportive fine-root ratio ####



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(tidyverse)
library(ggbeeswarm)
library(lme4)
library(lmerTest)
library(DHARMa)
library(emmeans)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_trees/data/processed")

### Load data ###
(edata <- read_table2("data_processed_acid.txt", col_names = T, na = "na", col_types = 
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
))
edata <- select(edata, abstransRatio, plot, block, replanted, species, acidbrickRatioTreat, soilType)
#Exclude 1 outlier
edata <- filter(edata, abstransRatio < 6)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #####################################################################################

#### a Graphs ---------------------------------------------------------------------------------------------
#simple effects:
par(mfrow = c(2,2))
plot(abstransRatio ~ species, edata)
plot(abstransRatio ~ soilType, edata)
plot(abstransRatio ~ acidbrickRatioTreat, edata)
plot(abstransRatio ~ block, edata)
#2way (soilType:species):
ggplot(edata, aes(soilType, abstransRatio)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (replanted:species):
ggplot(edata, aes(replanted, abstransRatio)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (acidbrickRatioTreat:soilType):
ggplot(edata, aes(acidbrickRatioTreat, abstransRatio)) + facet_grid(~soilType) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (acidbrickRatioTreat:species):
ggplot(edata, aes(acidbrickRatioTreat, abstransRatio)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#4way
ggplot(edata, aes(soilType, abstransRatio, color = acidbrickRatioTreat)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#interactions with block:
ggplot(edata, aes(species, abstransRatio, color = acidbrickRatioTreat)) + geom_boxplot() + facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(edata, aes(acidbrickRatioTreat, abstransRatio)) + geom_boxplot() + facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(edata, aes(block, abstransRatio, color = soilType)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)

##### b Outliers, zero-inflation, transformations? -----------------------------------------------------
par(mfrow = c(2,2))
dotchart((edata$abstransRatio), groups = factor(edata$species), main = "Cleveland dotplot")
dotchart((edata$abstransRatio), groups = factor(edata$soilType), main = "Cleveland dotplot")
dotchart((edata$abstransRatio), groups = factor(edata$acidbrickRatioTreat), main = "Cleveland dotplot")
dotchart((edata$abstransRatio), groups = factor(edata$block), main = "Cleveland dotplot")
par(mfrow=c(1,1));
boxplot(edata$abstransRatio);
identify(rep(1, length(edata$abstransRatio)), edata$abstransRatio, labels = c(edata$plot))
edata <- filter(edata, abstransRatio < 6)
plot(table((edata$abstransRatio)), type = "h", xlab = "Observed values", ylab = "Frequency")
ggplot(edata, aes(abstransRatio)) + geom_density()
ggplot(edata, aes(sqrt(abstransRatio))) + geom_density()


## 2 Model building ################################################################################

#### a models ----------------------------------------------------------------------------------------
#random structure --> no random factor needed
m1 <- lmer(log(abstransRatio) ~ species * acidbrickRatioTreat + (1|block), edata, REML = F)
VarCorr(m1)
#full-model
m2 <- lmer(log(abstransRatio) ~ species * soilType * acidbrickRatioTreat +
             (1|block), edata, REML = F)
isSingular(m2)
simulateResiduals(m2, plot = T)
#full 2w-model
m3 <- lmer(log(abstransRatio) ~ (species + soilType + acidbrickRatioTreat)^2 +
             (1|block), edata, REML = F)
isSingular(m3)
simulateResiduals(m3, plot = T)
#2w-model reduced
m4 <- lmer(log(abstransRatio) ~ species + soilType + acidbrickRatioTreat +
             acidbrickRatioTreat:species + acidbrickRatioTreat:soilType +
             (1|block), edata, REML = F)
isSingular(m4)
simulateResiduals(m4, plot = T)

#### b comparison -----------------------------------------------------------------------------------------
anova(m2,m3,m4) # --> m3
rm(m1,m2,m4)

#### c model check -----------------------------------------------------------------------------------------
simulationOutput <- simulateResiduals(m3, plot = T)
par(mfrow=c(2,2));
plotResiduals(main = "species", simulationOutput$scaledResiduals, edata$species)
plotResiduals(main = "soilType", simulationOutput$scaledResiduals,edata$soilType)
plotResiduals(main = "acidbrickRatioTreat", simulationOutput$scaledResiduals, edata$acidbrickRatioTreat)
plotResiduals(main = "block", simulationOutput$scaledResiduals, edata$block)


## 3 Chosen model output ################################################################################

### Model output ---------------------------------------------------------------------------------------------
m3 <- lmer(log(abstransRatio) ~ (species + soilType + acidbrickRatioTreat)^2 +
             (1|block), edata, REML = F)
MuMIn::r.squaredGLMM(m3) #R2m = 0.421, R2c = 0.512
VarCorr(m3)
sjPlot::plot_model(m3, type = "re", show.values = T)
car::Anova(m3, type = 3)

### Effect sizes -----------------------------------------------------------------------------------------
(emm <- emmeans(m3, revpairwise ~ acidbrickRatioTreat | species, type = "response"))
plot(emm, comparison = T)
contrast(emmeans(m3, ~ acidbrickRatioTreat | species, type = "response"), "trt.vs.ctrl", ref = 1)
(emm <- emmeans(m3, revpairwise ~ soilType | species, type = "response"))
plot(emm, comparison = T)
