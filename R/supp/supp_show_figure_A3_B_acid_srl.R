# Show Figure relative growth rate ~ acid:brickRatio:species ####
# Markus Bauer
# Citation: Markus Bauer, Martin Krause, Valentin Heizinger & Johannes Kollmann  (2021) ...
# DOI: ...



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
rm(list = c("data", "meandata", "pd", "pdata", "m4"))
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2021_waste_bricks_trees/data/processed")

### Load data ###
(data <- read_csv2("data_processed_acid.csv", col_names = T, na = "na", col_types = 
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
data <- select(data, srl, plot, block, species, acidbrickRatioTreat, soilType, conf.low, conf.high)
data$acidbrickRatioTreat <- dplyr::recode(data$acidbrickRatioTreat,
                                          "Control_30" = "Control 30% bricks", "Acid_5" = "Acid 5% bricks", "Acid_30" = "Acid 30% bricks")

#### Chosen model ###
m4 <- lm(log(srl) ~ species + soilType + acidbrickRatioTreat +
           acidbrickRatioTreat:species + acidbrickRatioTreat:soilType, data)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
themeMB <- function(){
  theme(
    panel.background = element_rect(fill = "white"),
    text  = element_text(size = 8, color = "black"),
    strip.text = element_text(size = 10),
    axis.line.y = element_line(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(angle = 90, hjust = 0.5),
    legend.key = element_rect(fill = "white"),
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}

### interaction: acid x brickRatio x species ###
pdata <- ggemmeans(m4, terms = c("acidbrickRatioTreat", "species"), type = "fe")
pdata <- rename(pdata, srl = predicted, acidbrickRatioTreat = x, species = group)
meandata <- filter(pdata, acidbrickRatioTreat == "Control 30% bricks")
pd <- position_dodge(.6)
(srl <- ggplot(pdata, aes(acidbrickRatioTreat, srl, shape = acidbrickRatioTreat, ymin = conf.low, ymax = conf.high))+
    geom_quasirandom(data = data, aes(acidbrickRatioTreat, srl), 
                     color = "grey70", dodge.width = .6, size = .7)+
    geom_hline(aes(yintercept = srl), meandata, 
               color = "grey70", size = .25) +
    geom_hline(aes(yintercept = conf.low), meandata, 
               color = "grey70", linetype = "dashed", size = .25) +
    geom_hline(aes(yintercept = conf.high), meandata, 
               color = "grey70", linetype = "dashed", size = .25) +
    geom_errorbar(position = pd, width = 0.0, size = 0.4) +
    geom_point(position = pd, size = 2.5) +
    facet_grid(~ species) +
    annotate("text", label = "n.s.", x = 3.2, y = 150) +
    scale_y_continuous(limits = c(0, 150), breaks = seq(-100, 150, 25)) +
    scale_shape_manual(values = c(1, 16, 15)) +
    labs(x = "", y = expression(Specific~root~length[1-3]~"["*m~g^-1*"]"), shape = "", color = "") +
    themeMB() +
    theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")
)

#ggsave("figure_A3_B_acid_srl_(800dpi_8x7.5cm).tiff",
#      dpi = 800, width = 8, height = 7.5, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2021_waste_bricks_trees/outputs/figures/supp")
