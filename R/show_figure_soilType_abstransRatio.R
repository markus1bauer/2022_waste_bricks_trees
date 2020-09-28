# Show Figure absorptive vs. transportive fine-root ratio ~ soilType:species:brickRatio ####



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(tidyverse)
library(ggbeeswarm)
library(lme4)
library(emmeans)
library(ggeffects)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_trees/data/processed")

### Load data ###
(edata <- read_table2("data_processed_brickRatio.txt", col_names = T, na = "na", col_types = 
                        cols(
                          .default = col_double(),
                          plot = col_factor(),
                          block = col_factor(),
                          replanted = col_factor(),
                          species = col_factor(),
                          mycorrhiza = col_factor(levels = c("Control","Mycorrhiza")),
                          substrate = col_factor(),
                          soilType = col_factor(levels = c("poor","rich")),
                          brickRatio = col_factor(levels = c("5","30")),
                          acid = col_factor(),
                          acidbrickRatioTreat = col_factor()
                        )        
))
edata <- select(edata, abstransRatio, plot, block, species, brickRatio, soilType, mycorrhiza, conf.high, conf.low)
#Exclude 1 outlier
edata <- filter(edata, abstransRatio < 6)

#### Chosen model ###
m4 <- lmer(log(abstransRatio+1) ~ (species + brickRatio + soilType + mycorrhiza)^2 +
             species:brickRatio:soilType + species:brickRatio:mycorrhiza +
             (1|block), edata, REML = F)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plotten ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
themeMB <- function(){
  theme(
    panel.background = element_rect(fill = "white"),
    text  = element_text(size=10, color = "black"),
    axis.line.y = element_line(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.key = element_rect(fill = "white"),
    legend.position = "right",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}

### acid:soilType ###
pdata <- ggemmeans(m4, terms = c("soilType","brickRatio", "species"), type = "fe")
pdata <- rename(pdata, abstransRatio = predicted, soilType = x, brickRatio = group, species = facet)
pdata$abstransRatio <- pdata$abstransRatio-1
pdata$conf.low <- pdata$conf.low-1
pdata$conf.high <- pdata$conf.high-1
meandata <- filter(pdata, soilType == "poor" & brickRatio == "5")
pd <- position_dodge(.6)
ggplot(pdata, aes(soilType, abstransRatio, shape = brickRatio, ymin = conf.low, ymax = conf.high))+
  geom_quasirandom(data = edata, aes(soilType, abstransRatio), 
                   color = "grey70", dodge.width = .6, size = 0.7)+
  geom_hline(aes(yintercept = abstransRatio), meandata, 
             color = "grey70", size = .25) +
  geom_hline(aes(yintercept = conf.low), meandata, 
             color = "grey70", linetype = "dashed", size = .25) +
  geom_hline(aes(yintercept = conf.high), meandata, 
             color = "grey70", linetype = "dashed", size = .25) +
  geom_errorbar(position = pd, width = 0.0, size = 0.4) +
  geom_point(position = pd, size = 2.5) +
  facet_grid(~ species) +
  annotate("text", label = "n.s.", x = 2.2, y = 2.2) +
  scale_y_continuous(limits = c(-0.02,2.2), breaks = seq(-100,100,0.5)) +
  scale_shape_manual(values = c(1,16)) +
  labs(x = "Soil fertility", y = expression(Absorptive*":"*transport~fine~roots~"["*g~g^-1*"]"), shape = "Brick ratio [%]", color = "") +
  themeMB()
#ggsave("figure_soilType_abstransRatio_(800dpi_12x6cm).tiff",
#       dpi = 800, width = 12, height = 6, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_trees/outputs/figures")
