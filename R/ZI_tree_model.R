#####################################################################################################################################
# Datenvorbereitung
# Statistik
## 1. acid
## 2. brickRatio and mycorrhiza
## 3. species and soil
# Plotten
###################################################################################################################################



###############################################################################################################################
# Datenvorbereitung ############################################################################################################
###############################################################################################################################
library(tibble);library(dplyr);setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse")

edata <- as_tibble(read.table("tree_site.txt",header=T, na.strings="na", dec ="."))
edata <- filter(edata, no < 101);
edata$finishdate <- rep("20.09.2019",100)
edata$height <- edata$height3 - edata$height1;edata$diameter <- edata$diameter3 - edata$diameter1
par(mfrow=c(2,2));range(edata$height);plot(table(edata$height));edata[which(edata$height < 0.5 | edata$height > 50),c(3,4,24)]
range(edata$diameter);plot(table(edata$diameter));edata[which(edata$diameter < 0.4 | edata$diameter > 10),c(3,4,25)]
edata$dateDiff <- as.numeric(as.Date(as.character(edata$finishdate),format="%d.%m.%Y") - as.Date(as.character(edata$plantdate),format="%d.%m.%Y"))
edata <- filter(edata, height > 0)
#RGR:
edata$rgr <- (log(edata$diameter3 * edata$height3) - log(edata$diameter1 * edata$height1))/edata$dateDiff
edata$brickRatio <- as.factor(edata$brickRatio); edata$block <- factor(edata$block)
edataFULL <- edata



###############################################################################################################################
# Statistik ############################################################################################################
###############################################################################################################################
library(emmeans);library(vcd);library(vegan);library(multcomp);library(lmerTest);library(car);library(pastecs);library(MuMIn);library(nlme);library(car);library(DHARMa)


## 1. acid ##########################################################################################################

edata <- filter(edataFULL, brickRatio==30 & mycorrhiza=="no_mycorrhiza")
####Overview
par(mfrow=c(2,2));plot(edata$rgr ~ edata$acid);plot(edata$rgr ~ edata$block);interaction.plot(edata$acid, edata$soil, edata$rgr);interaction.plot(edata$acid, edata$species, edata$rgr);
par(mfrow=c(2,2));dotchart(edata$rgr, groups=factor(edata$soil), main="Cleveland dotplot");dotchart(edata$rgr, groups=factor(edata$species), main="Cleveland dotplot");dotchart(edata$rgr, groups=factor(edata$acid), main="Cleveland dotplot");dotchart(edata$rgr, groups=factor(edata$block), main="Cleveland dotplot");
plot(table(edata$rgr));a <- edata[order(edata$rgr),];plot(a$rgr);rm(a)
by(edata$rgr,list(edata$acid,edata$soil),stat.desc);
####Model
m1a <- lmer(rgr ~ (species + soil + acid)^3 + (1|block), edata);Anova(m1a,type=3);isSingular(m1a)
r.squaredGLMM(m1a)
m1b <- lm(rgr ~ (species + soil + acid)^3, edata);Anova(m1b,type=3)
m1c <- lm((rgr) ~ (species + soil + acid)^2, edata);Anova(m1c,type=3)
m1d <- lm((rgr) ~ species + soil + acid + species:acid + soil:acid, edata);Anova(m1d,type=3)
m1e <- lm(rgr ~ species + soil + acid + soil:acid, edata);Anova(m1e,type=3)
m1f <- lm(rgr ~ species + soil + acid + species:acid, edata);Anova(m1f,type=3)
m1g <- lm((rgr) ~ species + soil + acid, edata);Anova(m1d,type=2)
m1h <- lm(rgr ~ species + soil, edata);Anova(m1h,type=2)
anova(m1a,m1b,m1c,m1d,m1e,m1f,m1g,m1h) #--> m1d
Anova(m1d, type=3)
summary(m1d);r.squaredGLMM(m1a) # 0.463; 0.415 0.415
####Model validation --> m1d
simulationOutput <- simulateResiduals(fittedModel = m1d, plot=T)
par(mfrow=c(2,2));plotResiduals(edata$soil, simulationOutput$scaledResiduals);plotResiduals(edata$acid, simulationOutput$scaledResiduals);plotResiduals(edata$species, simulationOutput$scaledResiduals);plotResiduals(edata$block, simulationOutput$scaledResiduals)
testResiduals(simulationOutput)
####Plotten
par(mfrow=c(2,2));interaction.plot(edata$acid, edata$soil, edata$rgr);interaction.plot(edata$acid, edata$species, edata$rgr)
interaction.plot(edata$soil, edata$species, edata$rgr)
####Contrasts
emmeans(m1d, pairwise ~ acid * soil)
emmeans(m1d, pairwise ~ acid * species)

## 2. brickRatio and mycorrhiza ##########################################################################################################

edata <- filter(edataFULL, acid=="acid")
####Overview
par(mfrow=c(2,2));plot(edata$rgr ~ edata$brickRatio);plot(edata$rgr ~ edata$species);plot(edata$rgr ~ edata$mycorrhiza);plot(edata$rgr ~ edata$block);
par(mfrow=c(2,2));interaction.plot(edata$brickRatio, edata$soil, edata$rgr);interaction.plot(edata$brickRatio, edata$species, edata$rgr);interaction.plot(edata$brickRatio, edata$mycorrhiza, edata$rgr)
par(mfrow=c(2,2));interaction.plot(edata$mycorrhiza, edata$soil, edata$rgr);interaction.plot(edata$mycorrhiza, edata$species, edata$rgr)
par(mfrow=c(2,2));dotchart(edata$rgr, groups=factor(edata$soil), main="Cleveland dotplot");dotchart(edata$rgr, groups=factor(edata$brickRatio), main="Cleveland dotplot");dotchart(edata$rgr, groups=factor(edata$species), main="Cleveland dotplot");dotchart(edata$rgr, groups=factor(edata$mycorrhiza), main="Cleveland dotplot");
dotchart(edata$rgr, groups=factor(edata$block), main="Cleveland dotplot");
plot(table(edata$rgr));a <- edata[order(edata$rgr),];plot(a$rgr);rm(a)
by(edata$rgr,list(edata$mycorrhiza,edata$soil),stat.desc);
####Model
m2a <- lmer(rgr ~ (species + soil + brickRatio + mycorrhiza)^3 + (1|block), edata);Anova(m2a, type=3);isSingular(m2a)
r.squaredGLMM(m2a) #--> .408 .414
m2b <- lm((rgr) ~ (species + soil + brickRatio + mycorrhiza)^3, edata);Anova(m2b, type=3)
m2c <- lm((rgr) ~ (species + soil + brickRatio + mycorrhiza)^2, edata);Anova(m2c,type=3)
m2d <- lm((rgr) ~ species + soil + brickRatio + mycorrhiza + species:soil + species:mycorrhiza + soil:brickRatio + soil:mycorrhiza + brickRatio:mycorrhiza, edata);Anova(m2d,type=3)
m2e <- lm((rgr) ~ species + soil + brickRatio + mycorrhiza + species:soil + soil:brickRatio + soil:mycorrhiza + brickRatio:mycorrhiza, edata);Anova(m2e,type=3)
m2f <- lm((rgr) ~ species + soil + brickRatio + mycorrhiza + species:soil + soil:brickRatio + brickRatio:mycorrhiza, edata);Anova(m2f,type=3)
m2g <- lm(sqrt(rgr) ~ species + soil + brickRatio + mycorrhiza + soil:brickRatio + brickRatio:mycorrhiza, edata);Anova(m2g,type=3)
m2h <- lm(sqrt(rgr) ~ species + soil + brickRatio + mycorrhiza + brickRatio:mycorrhiza, edata);Anova(m2h,type=3)
m2i <- lm(sqrt(rgr) ~ species + soil + brickRatio + mycorrhiza, edata);Anova(m2i,type=2)
anova(m2a,m2b,m2c,m2d,m2e,m2f,m2g,m2h,m2i) #--> m2h ABER m2g wg. besserer Modellkritik
Anova(m2g, type=3)
summary(m2g) # 0.409
####Model validation
simulationOutput <- simulateResiduals(fittedModel = m2g, plot=T)
par(mfrow=c(2,2));plotResiduals(edata$soil, simulationOutput$scaledResiduals);plotResiduals(edata$brickRatio, simulationOutput$scaledResiduals);plotResiduals(edata$species, simulationOutput$scaledResiduals);plotResiduals(edata$mycorrhiza, simulationOutput$scaledResiduals);
plotResiduals(edata$block, simulationOutput$scaledResiduals);
testResiduals(simulationOutput)
####Plotten
par(mfrow=c(2,2));interaction.plot(edata$brickRatio, edata$mycorrhiza, edata$rgr);plot(edata$rgr ~ edata$species);
par(mfrow=c(2,2));interaction.plot(edata$brickRatio, edata$soil, edata$rgr)
####Contrasts
emmeans(m2g, pairwise ~ brickRatio * soil)
emmeans(m2g, pairwise ~ brickRatio * species)
emmeans(m2g, pairwise ~ brickRatio * mycorrhiza)
emmeans(m2g, pairwise ~ soil * mycorrhiza)


## 3. species and soil ##########################################################################################################

edata <- edataFULL
####Overview
par(mfrow=c(2,2));plot(edata$rgr ~ edata$soil);plot(edata$rgr ~ edata$species);plot(edata$rgr ~ edata$block);interaction.plot(edata$species, edata$soil, edata$rgr)
par(mfrow=c(2,2));dotchart(edata$rgr, groups=factor(edata$soil), main="Cleveland dotplot");dotchart(edata$rgr, groups=factor(edata$species), main="Cleveland dotplot");dotchart(edata$rgr, groups=factor(edata$block), main="Cleveland dotplot");
plot(table(edata$rgr));a <- edata[order(edata$rgr),];plot(a$rgr);rm(a)
edata[which(edata$rgr > 0.005),c(3,4,28)]
#edata <- filter(edata, rgr < 0.0051)
by(edata$rgr,list(edata$species,edata$soil),stat.desc);
####Model
m3a <- lmer(rgr ~ (species + soil)^2 + (1|block), edata);Anova(m3a, type=3);isSingular(m3a)
m3b <- lmer(rgr ~ (species + soil) + (1|block), edata);Anova(m3b, type=2);isSingular(m3b)
r.squaredGLMM(m3a) #--> .247 .277
Anova(m3a, type=3)
####Model validation
simulationOutput <- simulateResiduals(fittedModel = m3a, plot=T)
par(mfrow=c(2,2));plotResiduals(edata$soil, simulationOutput$scaledResiduals);plotResiduals(edata$brickRatio, simulationOutput$scaledResiduals);plotResiduals(edata$species, simulationOutput$scaledResiduals);plotResiduals(edata$mycorrhiza, simulationOutput$scaledResiduals);
plotResiduals(edata$block, simulationOutput$scaledResiduals);
testResiduals(simulationOutput)
####Plotten
par(mfrow=c(2,2));plot(edata$rgr ~ edata$species);plot(edata$rgr ~ edata$soil)
interaction.plot(edata$species, edata$soil, edata$rgr);
####Contrasts
emmeans(m3a, pairwise ~ species * soil)



###############################################################################################################################
# Plotten ############################################################################################################
###############################################################################################################################
library(ggplot2);library(plyr)

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {library(plyr);length2 <- function (x, na.rm=FALSE) {if (na.rm) sum(!is.na(x)) else length(x)};datac <- ddply(data, groupvars, .drop=.drop, .fun = function(xx, col) {c(N    = length2(xx[[col]], na.rm=na.rm),mean = mean   (xx[[col]], na.rm=na.rm),sd   = sd(xx[[col]], na.rm=na.rm) ) },measurevar );datac <- rename(datac, c("mean" = measurevar));datac$se <- datac$sd / sqrt(datac$N); ciMult <- qt(conf.interval/2 + .5, datac$N-1);datac$ci <- datac$se * ciMult;return(datac)}


## 1. acid #############################################################################################
edata <- filter(edataFULL, brickRatio==30 & mycorrhiza=="no_mycorrhiza")

###acid -----------------------------------------------------------------------------------------------------------------------
Graph <- ggplot(edata,aes(acid, rgr)) +
  geom_boxplot() +
  scale_y_continuous(limits=c(0,.006), breaks=seq(0, 100, .002)) +
  scale_x_discrete(labels=c("Acid treatment","No acid"))+
  annotate("text", x = 2.2, y = .006, label = "n.s.",size=2) +
  labs(x="",y="Relative growth rate (RGR)") +
  theme(text  = element_text(size=7), axis.line.y = element_line() , axis.line.x = element_blank(),axis.ticks.x = element_blank(),legend.key = element_rect(fill="white"), legend.position = "right", legend.direction = "vertical",panel.background=element_rect(fill="white"));Graph
ggsave("tree_acid_rgr_(800dpi_4.5x4cm).tiff",dpi=800,width=4.5,height=4, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/Ergebnisse/tree")
###acid:soil -----------------------------------------------------------------------------------------------------------------------
pdata <- summarySE(edata, measurevar="rgr", groupvars=c("soil","acid"))
pd <- position_dodge(0.45)
Graph <- ggplot(pdata,aes(soil, rgr, shape=acid))+
  geom_point(size=2, position=pd) +
  geom_errorbar(position=pd, aes(ymin=rgr-se, ymax=rgr+se), width=.0,size=.25) +
  scale_y_continuous(limits=c(0,.0043), breaks=seq(0, 100, .001)) +
  scale_x_discrete(breaks=c("Y","Z"),labels=c("High fertility","Low fertility"))+
  scale_shape_manual(values=c(0,1),breaks=c("no_acid","acid"), labels=c("No acid", "Acid treatment"))+
  annotate("text", x = 2.05, y = .0043, label = expression(paste(italic("p"),"= 0.091")),size=2) +
  labs(x="",y="Relative growth rate (RGR)",shape = "") +
  guides(shape = guide_legend(reverse=T))+
  theme(text  = element_text(size=7), axis.line.y = element_line() , axis.line.x = element_blank(),axis.ticks.x = element_blank(),legend.key = element_rect(fill="white"), legend.position = "bottom", legend.direction = "horizontal",panel.background=element_rect(fill="white"));Graph
ggsave("tree_acid_soil_rgr_(800dpi_8x6cm).tiff",dpi=800,width=8,height=6, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/Ergebnisse/tree")
###species:acid -----------------------------------------------------------------------------------------------------------------------
pdata <- summarySE(edata, measurevar="rgr", groupvars=c("species","acid"))
pd <- position_dodge(0.45)
Graph <- ggplot(pdata,aes(species, rgr, shape=acid))+
  geom_point(size=2, position=pd) +
  geom_errorbar(position=pd, aes(ymin=rgr-se, ymax=rgr+se), width=.0,size=.25) +
  scale_y_continuous(limits=c(0,.0043), breaks=seq(0, 100, .001)) +
  scale_x_discrete(breaks=c("Acer","Tilia"),labels=c(expression(paste(italic("Acer platanoides"))),expression(paste(italic("Tilia cordata")))))+
  scale_shape_manual(values=c(0,1),breaks=c("no_acid","acid"), labels=c("No acid", "Acid treatment"))+
  annotate("text", x = 2.05, y = .0043, label = expression(paste(italic("p"),"= 0.071")),size=2) +
  labs(x="",y="Relative growth rate (RGR)",shape = "") +
  guides(shape = guide_legend(reverse=T))+
  theme(text  = element_text(size=7), axis.line.y = element_line() , axis.line.x = element_blank(),axis.ticks.x = element_blank(),legend.key = element_rect(fill="white"), legend.position = "bottom", legend.direction = "horizontal",panel.background=element_rect(fill="white"));Graph
ggsave("tree_acid_species_rgr_(800dpi_8x6cm).tiff",dpi=800,width=8,height=6, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/Ergebnisse/tree")


## 2. brickRatio and mycorrhiza #############################################################################################
edata <- filter(edataFULL, acid=="acid")

###brickRatio:mycorrhiza -----------------------------------------------------------------------------------------------------------------------
pdata <- summarySE(edata, measurevar="rgr", groupvars=c("brickRatio","mycorrhiza"))
pd <- position_dodge(0.6)
Graph <- ggplot(pdata,aes(mycorrhiza, rgr, shape=brickRatio))+
  geom_point(size=2, position=pd) +
  geom_errorbar(position=pd, aes(ymin=rgr-se, ymax=rgr+se), width=.0,size=.25) +
  scale_y_continuous(limits=c(0,.0043), breaks=seq(0, 100, .001)) +
  scale_x_discrete(breaks=c("mycorrhiza","no_mycorrhiza"), labels=c("Mycorrhiza", "No mycorrhiza"))+
  scale_shape_manual(values=c(1,0),breaks=c("5","30"),labels=c("5% bricks","30% bricks"))+
  annotate("text", x = 2.05, y = .0043, label = expression(paste(italic("p"),"= 0.041")),size=2) +
  annotate("text", x = c(0.89,1.11,1.89,2.11), y = rep(0.0039,4),label=c("a","b","b","b"),size=2)+
  labs(x="",y="Relative growth rate (RGR)",shape = "") +
  #guides(shape = guide_legend(reverse=T))+
  theme(legend.position = "right", text  = element_text(size=7), axis.line.y = element_line() , axis.line.x = element_blank(),axis.ticks.x = element_blank(),legend.key = element_rect(fill="white"), panel.background=element_rect(fill="white"));Graph
ggsave("tree_brickRatio_mycorrhiza_rgr_(800dpi_7x4cm).tiff",dpi=800,width=7,height=4, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/Ergebnisse/tree")
###brickRatio:soil -----------------------------------------------------------------------------------------------------------------------
pdata <- summarySE(edata, measurevar="rgr", groupvars=c("brickRatio","soil"))
pd <- position_dodge(0.6)
Graph <- ggplot(pdata,aes(soil, rgr, shape=brickRatio))+
  geom_point(size=2, position=pd) +
  geom_errorbar(position=pd, aes(ymin=rgr-se, ymax=rgr+se), width=.0,size=.25) +
  scale_y_continuous(limits=c(0,.0043), breaks=seq(0, 100, .001)) +
  scale_x_discrete(breaks=c("Y","Z"),labels=c("High fertility","Low fertility")) +
  scale_shape_manual(values=c(1,0), labels=c("5% bricks","30% bricks"))+
  annotate("text", x = 2.05, y = .0043, label = expression(paste(italic("p")," = 0.103")),size=2) +
  labs(x="",y="Relative growth rate (RGR)",shape = "") +
  #guides(shape = guide_legend(reverse=T))+
  theme(legend.position = "right", text  = element_text(size=7), axis.line.y = element_line() , axis.line.x = element_blank(),axis.ticks.x = element_blank(),legend.key = element_rect(fill="white"), panel.background=element_rect(fill="white"));Graph
ggsave("tree_brickRatio_soil_rgr_(800dpi_7x4cm).tiff",dpi=800,width=7,height=4, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/Ergebnisse/tree")
###species:brickRatio -----------------------------------------------------------------------------------------------------------------------
pdata <- summarySE(edata, measurevar="rgr", groupvars=c("species","brickRatio"))
pd <- position_dodge(0.6)
Graph <- ggplot(pdata,aes(species, rgr, shape=brickRatio))+
  geom_point(size=2, position=pd) +
  geom_errorbar(position=pd, aes(ymin=rgr-se, ymax=rgr+se), width=.0,size=.25) +
  scale_y_continuous(limits=c(0,.0043), breaks=seq(0, 100, .001)) +
  scale_shape_manual(values=c(1,0), breaks=c("5","30"),labels=c("5% bricks","30% bricks"))+
  scale_x_discrete(breaks=c("Acer","Tilia"),labels=c(expression(paste(italic("Acer platanoides"))),expression(paste(italic("Tilia cordata"))))) +
  annotate("text", x = 1.55, y = .0043, label = expression(paste("Bricks *  Sp. ***  Bricks x Sp. n.s.")),size=2) +
  labs(x="",y="Relative growth rate (RGR)",shape = "") +
  #guides(shape = guide_legend(reverse=T))+
  theme(legend.position = "right", text  = element_text(size=7), axis.line.y = element_line() , axis.line.x = element_blank(),axis.ticks.x = element_blank(),legend.key = element_rect(fill="white"), panel.background=element_rect(fill="white"));Graph
ggsave("tree_brickRatio_species_rgr_(800dpi_7x4cm).tiff",dpi=800,width=7,height=4, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/Ergebnisse/tree")
###soil:mycorrhiza -----------------------------------------------------------------------------------------------------------------------
pdata <- summarySE(edata, measurevar="rgr", groupvars=c("soil","mycorrhiza"))
pd <- position_dodge(0.45)
Graph <- ggplot(pdata,aes(soil, rgr, shape=mycorrhiza))+
  geom_point(size=2, position=pd) +
  geom_errorbar(position=pd, aes(ymin=rgr-se, ymax=rgr+se), width=.0,size=.25) +
  scale_y_continuous(limits=c(0,.004), breaks=seq(0, 100, .001)) +
  scale_x_discrete(breaks=c("Y","Z"),labels=c("High fertility","Low fertility"))+
  scale_shape_manual(values=c(0,1),breaks=c("mycorrhiza","no_mycorrhiza"), labels=c("Mycorrhiza", "No mycorrhiza"))+
  annotate("text", x = 1.4, y = .004, label = expression(paste("Myc. ", italic("p"), " = 0.004    ", "Fertility ", italic("p")," = 0.028    ", "Myc. x Fertility n.s.")),size=2) +
  labs(x="",y="Relative growth rate (RGR)",shape = "") +
  #guides(shape = guide_legend(reverse=T))+
  theme(text  = element_text(size=7), axis.line.y = element_line() , axis.line.x = element_blank(),axis.ticks.x = element_blank(),legend.key = element_rect(fill="white"), legend.position = "bottom", legend.direction = "horizontal",panel.background=element_rect(fill="white"));Graph
ggsave("tree_mycorrhiza_soil_rgr_(800dpi_8x6cm).tiff",dpi=800,width=8,height=6, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/Ergebnisse/tree")


## 3. species and soil #############################################################################################
edata <- edataFULL

###species -----------------------------------------------------------------------------------------------------------------------
Graph <- ggplot(edata,aes(species, rgr)) +
  geom_boxplot() +
  scale_y_continuous(limits=c(0,.007), breaks=seq(0, 100, .002)) +
  scale_x_discrete(labels=c(expression(paste(italic("Acer platanoides"))),expression(paste(italic("Tilia cordata"))))) +
  annotate("text", x = 2.1, y = .007, label = expression(paste(italic("p"), " < 0.003")),size=2) +
  labs(x="",y="Relative growth rate (RGR)") +
  theme(text  = element_text(size=7), axis.line.y = element_line() , axis.line.x = element_blank(),axis.ticks.x = element_blank(),legend.key = element_rect(fill="white"), legend.position = "right", legend.direction = "vertical",panel.background=element_rect(fill="white"));Graph
ggsave("tree_species_rgr_(800dpi_4.5x4cm).tiff",dpi=800,width=4.5,height=4, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/Ergebnisse/tree")
###soil -----------------------------------------------------------------------------------------------------------------------
Graph <- ggplot(edata,aes(soil, rgr)) +
  geom_boxplot() +
  scale_y_continuous(limits=c(0,.007), breaks=seq(0, 100, .002)) +
  scale_x_discrete(breaks=c("Y","Z"),labels=c("High fertility","Low fertility"))+
  annotate("text", x = 2.1, y = .007, label = expression(paste(italic("p"), " = 0.026")),size=2) +
  labs(x="",y="Relative growth rate (RGR)") +
  theme(text  = element_text(size=7), axis.line.y = element_line() , axis.line.x = element_blank(),axis.ticks.x = element_blank(),legend.key = element_rect(fill="white"), legend.position = "right", legend.direction = "vertical",panel.background=element_rect(fill="white"));Graph
ggsave("tree_soil_rgr_(800dpi_4.5x4cm).tiff",dpi=800,width=4.5,height=4, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/Ergebnisse/tree")
###species:soil -----------------------------------------------------------------------------------------------------------------------
pdata <- summarySE(edata, measurevar="rgr", groupvars=c("soil","species"))
pd <- position_dodge(0.45)
Graph <- ggplot(pdata,aes(species, rgr, shape=soil))+
  geom_point(size=2, position=pd) +
  geom_errorbar(position=pd, aes(ymin=rgr-se, ymax=rgr+se), width=.0,size=.25) +
  scale_y_continuous(limits=c(0,.004), breaks=seq(0, 100, .001)) +
  scale_x_discrete(labels=c(expression(paste(italic("Acer platanoides"))),expression(paste(italic("Tilia cordata"))))) +
  scale_shape_manual(values=c(0,1),breaks=c("Y","Z"),labels=c("High fertility","Low fertility"))+
  annotate("text", x = 1.4, y = .004, label = expression(paste("Species ", italic("p"), " = 0.003    ", "Fertility ", italic("p")," = 0.026    ", "Species x Fertility n.s.")),size=2) +
  labs(x="",y="Relative growth rate (RGR)",shape = "") +
  #guides(shape = guide_legend(reverse=T))+
  theme(text  = element_text(size=7), axis.line.y = element_line() , axis.line.x = element_blank(),axis.ticks.x = element_blank(),legend.key = element_rect(fill="white"), legend.position = "bottom", legend.direction = "horizontal",panel.background=element_rect(fill="white"));Graph
ggsave("tree_species_soil_rgr_(800dpi_8x6cm).tiff",dpi=800,width=8,height=6, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/Ergebnisse/tree")
