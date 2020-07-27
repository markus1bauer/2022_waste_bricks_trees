# Model for experiment brick ratio and specific leaf area ####



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#library(installr);updateR(browse_news=F, install_R=T, copy_packages = T,copy_Rprofile.site = T,keep_old_packages = T, update_packages = T)

### Packages ###
library(tidyverse)
library(ggplot2)
library(ggbeeswarm)
library(car); #Anova(); vif(): variance inflation factors --> checking for dependence (Collinearity) (below 3 is ok)
library(nlme); #use for vif()
library(lme4)
library(lmerTest)
library(DHARMa)
#library(vcd)
library(sjPlot) #plot random effects
library(MuMIn)
library(emmeans)
library(ggeffects)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_trees/data/processed")

### Load data ###
(edata <- read_table2("data_processed_brickRatio.txt", col_names = T, na = "na", col_types = 
                        cols(
                          .default = col_double(),
                          plot = col_factor(),
                          block = col_factor(),
                          replanted = col_factor(),
                          date1 = col_date(),
                          date2 = col_date(),
                          date3 = col_date(),
                          species = col_factor(),
                          mycorrhiza = col_factor(levels = c("Control","Mycorrhiza")),
                          substrate = col_factor(),
                          soilType = col_factor(),
                          brickRatio = col_factor(levels = c("5","30")),
                          acid = col_factor(levels = c("Acid_5","Acid_30")),
                          leaf = col_factor()
                        )        
))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #####################################################################################

#### a Graphs ---------------------------------------------------------------------------------------------
#simple effects:
par(mfrow = c(2,2))
plot(sla ~ species, edata)
plot(sla ~ brickRatio, edata)
plot(sla ~ soilType, edata)
plot(sla ~ mycorrhiza, edata)
plot(sla ~ block, edata)
plot(sla ~ plot, edata)
#2way (brickRatio:species):
ggplot(edata,aes(species, sla, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (brickRatio:soilType):
ggplot(edata,aes(soilType, sla, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (brickRatio:mycorrhiza):
ggplot(edata,aes(mycorrhiza, sla, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (species:soilType):
ggplot(edata,aes(species, sla, color = soilType)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (species:mycorrhiza):
ggplot(edata,aes(species, sla, color = mycorrhiza)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (soilType:mycorrhiza):
ggplot(edata,aes(soilType, sla, color = mycorrhiza)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:species:soilType):
ggplot(edata,aes(soilType, sla, color = brickRatio)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:species:mycorrhiza):
ggplot(edata,aes(mycorrhiza, sla, color = brickRatio)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (species:soilType:mycorrhiza):
ggplot(edata,aes(soilType, sla, color = mycorrhiza)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#4way
ggplot(edata,aes(soilType, sla, color = brickRatio, shape = mycorrhiza)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
# interactions with block:
ggplot(edata,aes(brickRatio, sla, color = species)) + geom_boxplot() + facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(block, sla, color = species)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(block, sla, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(block, sla, color = soilType)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(block, sla, color = mycorrhiza)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)

##### b Outliers, zero-inflation, transformations? -----------------------------------------------------
par(mfrow = c(2,2))
dotchart((edata$sla), groups = factor(edata$species), main = "Cleveland dotplot")
dotchart((edata$sla), groups = factor(edata$brickRatio), main = "Cleveland dotplot")
dotchart((edata$sla), groups = factor(edata$soilType), main = "Cleveland dotplot")
dotchart((edata$sla), groups = factor(edata$mycorrhiza), main = "Cleveland dotplot")
par(mfrow=c(1,1));
boxplot(edata$sla, ylim = c(100,400));#identify(rep(1,length(edata$biomass)),edata$biomass, labels = c(edata$no))
par(mfrow = c(2,2));
plot(table((edata$sla)),type = "h", xlab = "Observed values", ylab = "Frequency")
plot(table(log(edata$sla)), type = "h", xlab = "Observed values", ylab = "Frequency");
ggplot(edata, aes(sla)) + geom_density()


## 2 Model building ################################################################################

#### a models ----------------------------------------------------------------------------------------
#random structure
m1 <- lmer(sla ~ species * brickRatio + (1|block/plot), edata, REML = F)
VarCorr(m1)
#4w-model
m2 <- lmer(log(sla) ~ species * brickRatio * soilType * mycorrhiza +
             (1|block/plot), edata, REML = F)
isSingular(m2)
simulationOutput <- simulateResiduals(m2, plot = T)
#full 3w-model
m3 <- lmer(log(sla) ~ (species + brickRatio + soilType + mycorrhiza)^3 +
             (1|block/plot), edata, REML = F)
isSingular(m3)
simulationOutput <- simulateResiduals(m3, plot = T)
#3w-model species:mycorrhiza:soilType
m4 <- lmer(log(sla) ~ (species + brickRatio + soilType + mycorrhiza)^2 +
             species:mycorrhiza:soilType + species:brickRatio:soilType +
             (1|block/plot), edata, REML = F)
isSingular(m4)
simulationOutput <- simulateResiduals(m4, plot = T)
#3w-model species:brickRatio:soilType
m5 <- lmer(log(sla) ~ (species + brickRatio + soilType + mycorrhiza)^2 +
             species:brickRatio:soilType + 
             (1|block/plot), edata, REML = F)
isSingular(m5)
simulationOutput <- simulateResiduals(m5, plot = T)
#2w-model
m6 <- lmer(log(sla) ~ (species + brickRatio + soilType + mycorrhiza)^2 +
             (1|block/plot), edata, REML = F)
isSingular(m6)
simulationOutput <- simulateResiduals(m6, plot = T)
#2w-model
m7 <- lmer(log(sla) ~ (species + brickRatio + soilType + mycorrhiza) +
             species:brickRatio + species:soilType +
             (1|block/plot), edata, REML = F)
isSingular(m7)
simulationOutput <- simulateResiduals(m7, plot = T);

#### b comparison -----------------------------------------------------------------------------------------
AIC(m2,m3,m4,m5,m6,m7) # --> m4
(re.effects <- plot_model(m4, type = "re", show.values = TRUE))
rm(m1,m2,m3,m5,m6,m7)

#### c model check -----------------------------------------------------------------------------------------
simulationOutput <- simulateResiduals(m4, plot = F)
par(mfrow=c(2,2));
plotResiduals(main = "species", simulationOutput$scaledResiduals, edata$species)
plotResiduals(main = "brickRatio", simulationOutput$scaledResiduals, edata$brickRatio)
plotResiduals(main = "soilType", simulationOutput$scaledResiduals, edata$soilType)
plotResiduals(main = "mycorrhiza", simulationOutput$scaledResiduals,edata$mycorrhiza)
plotResiduals(main = "block", simulationOutput$scaledResiduals, edata$block)
plotResiduals(main = "plot", simulationOutput$scaledResiduals, edata$plot)


## 3 Chosen model output ################################################################################

### Model output ---------------------------------------------------------------------------------------------
m4 <- lmer(log(sla) ~ (species + brickRatio + soilType + mycorrhiza)^2 +
             species:mycorrhiza:soilType + species:brickRatio:soilType +
             (1|block/plot), edata, REML = F)
VarCorr(m4)
r.squaredGLMM(m4)
Anova(m4, type = 3)

### Effect sizes -----------------------------------------------------------------------------------------
(emm <- emmeans(m4, revpairwise ~ brickRatio * mycorrhiza, type = "response"))
plot(emm, comparison = T)
(emm <- emmeans(m4, revpairwise ~ soilType * mycorrhiza | species, type="response"))
plot(emm, comparison = T)
(emm <- emmeans(m4, revpairwise ~ soilType * brickRatio | species, type="response"))
plot(emm, comparison = T)