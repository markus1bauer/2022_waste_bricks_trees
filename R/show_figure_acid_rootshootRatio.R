# Show Figure root-shoot ratio ~ acid:brickRatio:species ####



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
edata <- select(edata, rootshootRatio, plot, block, replanted, species, acidbrickRatioTreat, soilType, conf.low, conf.high)
edata$acidbrickRatioTreat <- dplyr::recode(edata$acidbrickRatioTreat,
                                           "Control_30" = "Control 30% bricks", "Acid_5" = "Acid 5% bricks", "Acid_30" = "Acid 30% bricks")

#### Chosen model ###
m4 <- lm(log(rootshootRatio) ~ species + soilType + acidbrickRatioTreat +
           acidbrickRatioTreat:species + acidbrickRatioTreat:soilType, edata)



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

### interaction: acid x brickRatio x species ###
pdata <- ggemmeans(m4, terms = c("acidbrickRatioTreat", "species"), type = "fe")
pdata <- rename(pdata, rootshootRatio = predicted, acidbrickRatioTreat = x, species = group)
meandata <- filter(pdata, acidbrickRatioTreat == "Control 30% bricks")
pd <- position_dodge(.6)
ggplot(pdata, aes(acidbrickRatioTreat, rootshootRatio, shape = acidbrickRatioTreat, ymin = conf.low, ymax = conf.high))+
  geom_quasirandom(data = edata, aes(acidbrickRatioTreat, rootshootRatio), 
                   color = "grey70", dodge.width = .6, size = 0.7)+
  geom_hline(aes(yintercept = rootshootRatio), meandata, color = "grey70") +
  geom_errorbar(position = pd, width = 0.0, size = 0.4) +
  geom_point(position = pd, size = 2.5) +
  facet_grid(~ species) +
  annotate("text", label = "n.s.", x = 3.2, y = 2.06) +
  scale_y_continuous(limits = c(0.5,2.06), breaks = seq(-100,100,0.2)) +
  scale_shape_manual(values = c(1,16,16)) +
  labs(x = "", y = expression("Root-to-shoot ratio ["*g~g^-1*"]"), shape = "", color = "") +
  guides(x = guide_axis(angle = 45), shape = F)+
  themeMB()
ggsave("figure_acid_rootshootRatio_(800dpi_8x7cm).tiff",
      dpi = 800, width = 8, height = 7, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_trees/outputs/figures")
