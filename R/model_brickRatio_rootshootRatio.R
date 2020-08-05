# Model for experiment brick ratio and root mass fraction ####



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
                          acid = col_factor(levels = c("Acid")),
                          acidbrickRatioTreat = col_factor()
                        )        
))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #####################################################################################

#### a Graphs ---------------------------------------------------------------------------------------------
#simple effects:
par(mfrow = c(2,2))
plot(rootshootRatio ~ species, edata)
plot(rootshootRatio ~ brickRatio, edata)
plot(rootshootRatio ~ soilType, edata)
plot(rootshootRatio ~ mycorrhiza, edata)
par(mfrow = c(2,2))
plot(rootshootRatio ~ block, edata)
#2way (brickRatio:species):
ggplot(edata, aes(species, rootshootRatio, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (brickRatio:soilType):
ggplot(edata, aes(soilType, rootshootRatio, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (brickRatio:mycorrhiza):
ggplot(edata, aes(mycorrhiza, rootshootRatio, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (species:soilType):
ggplot(edata, aes(species, rootshootRatio, color = soilType)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (species:mycorrhiza):
ggplot(edata, aes(species, rootshootRatio, color = mycorrhiza)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (soilType:mycorrhiza):
ggplot(edata, aes(soilType, rootshootRatio, color = mycorrhiza)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:species:soilType):
ggplot(edata, aes(soilType, rootshootRatio, color = brickRatio)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:species:mycorrhiza):
ggplot(edata, aes(mycorrhiza, rootshootRatio, color = brickRatio)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (species:soilType:mycorrhiza):
ggplot(edata, aes(soilType, rootshootRatio, color = mycorrhiza)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#4way
ggplot(edata,aes(soilType, rootshootRatio, color = brickRatio, shape = mycorrhiza)) + facet_grid(~species) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
# interactions with block:
ggplot(edata,aes(brickRatio, rootshootRatio, color = species)) + geom_boxplot() + facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(block, rootshootRatio, color = species)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(block, rootshootRatio, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(block, rootshootRatio, color = soilType)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(block, rootshootRatio, color = mycorrhiza)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)

##### b Outliers, zero-inflation, transformations? -----------------------------------------------------
par(mfrow = c(2,2))
dotchart((edata$rootshootRatio), groups = factor(edata$species), main = "Cleveland dotplot")
dotchart((edata$rootshootRatio), groups = factor(edata$brickRatio), main = "Cleveland dotplot")
dotchart((edata$rootshootRatio), groups = factor(edata$soilType), main = "Cleveland dotplot")
dotchart((edata$rootshootRatio), groups = factor(edata$mycorrhiza), main = "Cleveland dotplot")
dotchart((edata$rootshootRatio), groups = factor(edata$block), main = "Cleveland dotplot")
par(mfrow=c(1,1));
boxplot(edata$rootshootRatio, ylim = c(0,45));#identify(rep(1,length(edata$rootshootRatio)),edata$rootshootRatio, labels = c(edata$no))
par(mfrow = c(2,2));
plot(table((edata$rootshootRatio)),type = "h", xlab = "Observed values", ylab = "Frequency")
plot(table(log(edata$rootshootRatio)), type = "h", xlab = "Observed values", ylab = "Frequency");
ggplot(edata, aes(rootshootRatio)) + geom_density()


## 2 Model building ################################################################################

#### a models ----------------------------------------------------------------------------------------
#random structure
m1 <- lmer((rootshootRatio) ~ species * brickRatio + (1|block), edata, REML = F)
VarCorr(m1)
#4w-model
m2 <- lmer(rootshootRatio ~ species * brickRatio * soilType * mycorrhiza +
             (1|block), edata, REML = F)
isSingular(m2)
simulationOutput <- simulateResiduals(m2, plot = T)
#full 3w-model
m3 <- lmer(rootshootRatio ~ (species + brickRatio + soilType + mycorrhiza)^3 +
             (1|block), edata, REML = F)
isSingular(m3)
simulationOutput <- simulateResiduals(m3, plot = T)
#3w-model brick:water:mix
m4 <- lmer(log(rootshootRatio) ~ (brickRatio + acid + f.watering + seedmix) +
             brickRatio:acid + brickRatio:f.watering + brickRatio:seedmix + 
             f.watering:seedmix +
             brickRatio:f.watering:seedmix + 
             (1|block), edata, REML = F)
isSingular(m4)
simulationOutput <- simulateResiduals(m4, plot = T)
#3w-model brick:acid:mix
m5 <- lmer(log(rootshootRatio) ~ (brickRatio + acid + f.watering + seedmix) +  
             brickRatio:acid + brickRatio:f.watering + brickRatio:seedmix + 
             f.watering:seedmix + acid:seedmix + 
             brickRatio:acid:seedmix + 
             (1|block), edata, REML = F)
isSingular(m5)
simulationOutput <- simulateResiduals(m5, plot = T)
#2w-model
m6 <- lmer(log(rootshootRatio) ~ (brickRatio + acid + f.watering + seedmix) + 
             brickRatio:acid + brickRatio:f.watering + brickRatio:seedmix + 
             f.watering:seedmix + 
             (1|block), edata, REML = F)
isSingular(m6)
simulationOutput <- simulateResiduals(m6, plot = T);

#### b comparison -----------------------------------------------------------------------------------------
anova(m2,m3,m4,m5,m6) # --> m5
(re.effects <- plot_model(m5, type = "re", show.values = TRUE))
rm(m1,m2,m3,m4,m6)

#### c model check -----------------------------------------------------------------------------------------
simulationOutput <- simulateResiduals(m5, plot = F)
par(mfrow=c(2,2));
plotResiduals(main = "species", simulationOutput$scaledResiduals, edata$brickRatio)
plotResiduals(main = "brickRatio", simulationOutput$scaledResiduals, edata$acid)
plotResiduals(main = "soilType", simulationOutput$scaledResiduals, edata$f.watering)
plotResiduals(main = "mycorrhiza", simulationOutput$scaledResiduals,edata$seedmix)
plotResiduals(main = "block", simulationOutput$scaledResiduals, edata$block)


## 3 Chosen model output ################################################################################

### Model output ---------------------------------------------------------------------------------------------
m5 <- lmer(log(rootshootRatio) ~ (brickRatio + acid + f.watering + seedmix) +  
             brickRatio:acid + brickRatio:f.watering + brickRatio:seedmix + 
             f.watering:seedmix + acid:seedmix + 
             brickRatio:acid:seedmix + 
             (1|block), edata, REML = F)
VarCorr(m5)
r.squaredGLMM(m5)
Anova(m5, type = 3)

### Effect sizes -----------------------------------------------------------------------------------------
(emm <- emmeans(m5, revpairwise ~ seedmix | f.watering, type = "response"))
plot(emm, comparison = T)
contrast(emmeans(m5, ~ seedmix * f.watering, type = "response"), "trt.vs.ctrl", ref = 1)
(emm <- emmeans(m5, revpairwise ~ brickRatio * acid | seedmix, type="response"))
plot(emm, comparison = T)
(emm <- emmeans(m5, revpairwise ~ brickRatio | f.watering, type = "response"))
plot(emm, comparison = T)