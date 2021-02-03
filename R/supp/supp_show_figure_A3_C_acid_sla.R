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
data <- gather(data, "leaf", "sla", sla1, sla2, sla3, factor_key = T)
data <- select(data, sla, plot, block, species, acidbrickRatioTreat, soilType, conf.low, conf.high)
data$acidbrickRatioTreat <- dplyr::recode(data$acidbrickRatioTreat,
                                          "Control_30" = "Control 30% bricks", "Acid_5" = "Acid 5% bricks", "Acid_30" = "Acid 30% bricks")

#### Chosen model ###
m2 <- lmer((sla) ~ species * soilType * acidbrickRatioTreat + 
             (1|block/plot), data, REML= F)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
themeMB <- function(){
  theme(
    panel.background = element_rect(fill = "white"),
    text  = element_text(size = 8, color = "black"),
    strip.text = element_text(size = 11),
    axis.line.y = element_line(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(angle = 90, hjust = 0.5),
    legend.key = element_rect(fill = "white"),
    legend.position = "right",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}

### interaction: acid x brickRatio x species ###
pdata <- ggemmeans(m2, terms = c("acidbrickRatioTreat", "species"), type = "fe")
pdata <- rename(pdata, sla = predicted, acidbrickRatioTreat = x, species = group)
meandata <- filter(pdata, acidbrickRatioTreat == "Control 30% bricks")
pd <- position_dodge(.6)
ann_text1 <- data.frame(acidbrickRatioTreat = "Acid 30% bricks", 
                        sla = 265,
                        species = factor("Acer", levels = c("Acer","Tilia")),
                        conf.low = 265,
                        conf.high = 265)
ann_text2 <- data.frame(acidbrickRatioTreat = "Control 30% bricks", 
                        sla = 265,
                        species = factor("Tilia", levels = c("Acer","Tilia")),
                        conf.low = 265,
                        conf.high = 265)
ann_text3 <- data.frame(acidbrickRatioTreat = "Acid 5% bricks", 
                        sla = 265,
                        species = factor("Tilia", levels = c("Acer","Tilia")),
                        conf.low = 265,
                        conf.high = 265)
ann_text4 <- data.frame(acidbrickRatioTreat = "Acid 30% bricks", 
                        sla = 265,
                        species = factor("Tilia", levels = c("Acer","Tilia")),
                        conf.low = 265,
                        conf.high = 265)
(sla <- ggplot(pdata, aes(acidbrickRatioTreat, sla, shape = acidbrickRatioTreat, ymin = conf.low, ymax = conf.high))+
    geom_quasirandom(data = data, aes(acidbrickRatioTreat, sla), 
                     color = "grey70", dodge.width = .6, size = 0.7)+
    geom_hline(aes(yintercept = sla), meandata, 
               color = "grey70", size = .25) +
    geom_hline(aes(yintercept = conf.low), meandata, 
               color = "grey70", linetype = "dashed", size = .25) +
    geom_hline(aes(yintercept = conf.high), meandata, 
               color = "grey70", linetype = "dashed", size = .25) +
    geom_errorbar(position = pd, width = 0.0, size = 0.4) +
    geom_point(position = pd, size = 2.5) +
    facet_grid(~ species) +
    geom_text(data = ann_text1, label = "n.s.") +
    geom_text(data = ann_text2, label = "ab") +
    geom_text(data = ann_text3, label = "a") +
    geom_text(data = ann_text4, label = "b") +
    scale_y_continuous(limits = c(120, 270), breaks = seq(-100, 270, 50)) +
    scale_shape_manual(values = c(1, 16, 15)) +
    labs(x = "", y = expression(Specific~leaf~area~"["*cm^2~g^-1*"]"), shape = "", color = "") +
    themeMB() +
    theme(strip.text = element_blank(), 
          strip.background = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          legend.position = "none")
)

#ggsave("figure_A3_C_acid_sla_(800dpi_8x7.5cm).tiff",
#      dpi = 800, width = 8, height = 7.5, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2021_waste_bricks_trees/outputs/figures/supp")
