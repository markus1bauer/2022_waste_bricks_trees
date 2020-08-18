# Show Figure absorptive vs. transportive fine-root ratio  ~ mycorrhiza:species:brickRatio ####



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(tidyverse)
library(ggplot2)
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
                          date1 = col_date(),
                          date2 = col_date(),
                          date3 = col_date(),
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
edata <- select(edata, abstransRatio, plot, block, replanted, species, brickRatio, soilType, mycorrhiza, conf.low, conf.high)

#### Chosen model ###
m4 <- lmer(abstransRatio ~ (species + brickRatio + soilType + mycorrhiza)^2 +
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
pdata <- ggemmeans(m4, terms = c("mycorrhiza","brickRatio", "species"), type = "fe")
pdata <- rename(pdata, abstransRatio = predicted, mycorrhiza = x, brickRatio = group, species = facet)
meandata <- filter(pdata, mycorrhiza == "Control" & brickRatio == "5")
pd <- position_dodge(.6)
ggplot(pdata, aes(mycorrhiza, abstransRatio, shape = brickRatio, ymin = conf.low, ymax = conf.high))+
  geom_quasirandom(data = edata, aes(mycorrhiza, abstransRatio), 
                   color = "grey70", dodge.width = .6, size = 0.7)+
  geom_hline(aes(yintercept = abstransRatio), meandata, color = "grey70") +
  geom_errorbar(position = pd, width = 0.0, size = 0.4) +
  geom_point(position = pd, size = 2.5) +
  facet_grid(~ species) +
  annotate("text", label = "n.s.", x = 2.2, y = 0.5) +
  scale_y_continuous(limits = c(0.15,0.5), breaks = seq(-100,100,0.05)) +
  scale_shape_manual(values = c(1,16)) +
  labs(x = "", y = expression(Absorptive*":"*transport~fine~roots~"["*g~g^-1*"]"), shape = "Brick ratio [%]", color = "") +
  guides(x = guide_axis(angle = 0))+
  themeMB()
ggsave("figure_mycorrhiza_abstransRatio_(800dpi_12x6cm).tiff",
       dpi = 800, width = 12, height = 6, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_trees/outputs/figures")
#visreg(m5, "seedmix", by = "f.watering", ylab = expression(paste(Delta,"biomass [g g"^"-1"*"]")), xlab = "", data = edata,
#       type = "contrast", partial = T, rug = F, gg = T, overlay = F, band = T, points = list(cex = 0.5, pch = 16), line = list(col = "black"), whitespace = .2) +
#  themeMB()