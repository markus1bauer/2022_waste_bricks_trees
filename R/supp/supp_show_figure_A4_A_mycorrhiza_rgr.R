# Show Figure relative growth rate ~ mycorrhiza:species:brickRatio ####
# Markus Bauer
# Citation: Markus Bauer, Martin Krause, Valentin Heizinger & Johannes Kollmann  (2021) ...
# DOI: ...



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2021_waste_bricks_trees/data/processed")

### Load data ###
data <- read_csv2("data_processed_brickRatio.csv", col_names = T, na = "na", col_types = 
                    cols(
                      .default = col_double(),
                      plot = col_factor(),
                      block = col_factor(),
                      replanted = col_factor(),
                      species = col_factor(),
                      mycorrhiza = col_factor(levels = c("Control", "Mycorrhiza")),
                      substrate = col_factor(),
                      soilType = col_factor(levels = c("poor", "rich")),
                      brickRatio = col_factor(levels = c("5", "30")),
                      acid = col_factor(),
                      acidbrickRatioTreat = col_factor()
                    )        
)
(data <- select(data, rgr13, plot, block, species, brickRatio, soilType, mycorrhiza, conf.low, conf.high))

#### Chosen model ###
m4 <- lmer(rgr13 ~ (species + brickRatio + soilType + mycorrhiza)^2 +
             species:brickRatio:soilType + species:brickRatio:mycorrhiza +
             (1|block), data, REML = F)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
themeMB <- function(){
  theme(
    panel.background = element_rect(fill = "white"),
    text  = element_text(size = 8, color = "black"),
    strip.text = element_text(size = 10),
    axis.text.y = element_text(angle = 90, hjust = .5),
    axis.line.y = element_line(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.key = element_rect(fill = "white"),
    legend.position = "right",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}

### brickRatio:mycorrhiza ###
pdata <- ggemmeans(m4, terms = c("mycorrhiza", "brickRatio", "species"), type = "fe")
pdata <- rename(pdata, rgr13 = predicted, mycorrhiza = x, brickRatio = group, species = facet)
meandata <- filter(pdata, mycorrhiza == "Control" & brickRatio == "5")
pd <- position_dodge(.6)
(rgr13 <- ggplot(pdata, aes(mycorrhiza, rgr13, shape = brickRatio, ymin = conf.low, ymax = conf.high)) +
    geom_quasirandom(data = data, aes(mycorrhiza, rgr13), 
                     color = "grey70", dodge.width = .6, size = .7) +
    geom_hline(aes(yintercept = rgr13), meandata, 
               color = "grey70", size = .25) +
    geom_hline(aes(yintercept = conf.low), meandata, 
               color = "grey70", linetype = "dashed", size = .25) +
    geom_hline(aes(yintercept = conf.high), meandata, 
               color = "grey70", linetype = "dashed", size = .25) +
    geom_errorbar(position = pd, width = .0, size = .4) +
    geom_point(position = pd, size = 2.5) +
    facet_grid(~ species) +
    annotate("text", label = "n.s.", x = 2.2, y = 0.0028) +
    scale_y_continuous(limits = c(0, 0.003), breaks = seq(-100, 100, 0.001)) +
    scale_shape_manual(values = c(1,16)) +
    labs(x = "Mycorrhiza", y = expression(paste("Relative growth rate")), shape = "Brick ratio [%]", color = "") +
    themeMB() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          legend.direction = "horizontal",
          legend.position = c(0.3, 0.03))
)

#ggsave("figure_mycorrhiza_rgr_(800dpi_12x7cm).tiff",
#      dpi = 800, width = 12, height = 7, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2021_waste_bricks_trees/outputs/figures/supp")
