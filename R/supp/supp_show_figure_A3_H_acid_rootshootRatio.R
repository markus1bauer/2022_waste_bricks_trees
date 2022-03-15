# Show Figure relative growth rate ~ acid:brickRatio:species ####
# Markus Bauer
# Citation: Markus Bauer, Martin Krause, Valentin Heizinger & Johannes Kollmann  (2021) ...
# DOI: ...



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ##############################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(here)
library(tidyverse)
library(ggbeeswarm)
library(lme4)
library(emmeans)
library(ggeffects)

### Start ###
rm(list = c("data", "meandata", "pd", "pdata", "m4"))
setwd(here("data", "processed"))

### Load data ###
(data <- read_csv("data_processed_acid.csv",
                   col_names = TRUE, na = "na", col_types =
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
                       acidbrickRatioTreat =
                         col_factor(
                           levels = c("Control_30","Acid_5","Acid_30")
                           )
                     )
                  ) %>%
    select(rootshootRatio, plot, block, species, acidbrickRatioTreat, soilType,
           conf.low, conf.high)
  )
  
data$acidbrickRatioTreat <- dplyr::recode(data$acidbrickRatioTreat,
                                          "Control_30" = "Control 30% bricks",
                                          "Acid_5" = "Acid 5% bricks",
                                          "Acid_30" = "Acid 30% bricks")

#### Chosen model ###
m4 <- lm(log(rootshootRatio) ~ species + soilType + acidbrickRatioTreat +
           acidbrickRatioTreat:species + acidbrickRatioTreat:soilType, data)



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plotten ##################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


themeMB <- function(){
  theme(
    panel.background = element_rect(fill = "white"),
    text  = element_text(size = 8, color = "black"),
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
pdata <- ggemmeans(m4, terms = c("acidbrickRatioTreat", "species"),
                   type = "fe")
pdata <- rename(pdata, rootshootRatio = predicted,
                acidbrickRatioTreat = x, species = group)
meandata <- filter(pdata, acidbrickRatioTreat == "Control 30% bricks")
pd <- position_dodge(.6)

### plot ###
(rootshootRatio <- ggplot(pdata,
                          aes(acidbrickRatioTreat, rootshootRatio,
                              shape = acidbrickRatioTreat,
                              ymin = conf.low, ymax = conf.high))+
    geom_quasirandom(data = data, aes(acidbrickRatioTreat, rootshootRatio), 
                     color = "grey70", dodge.width = .6, size = .7)+
    geom_hline(aes(yintercept = rootshootRatio), meandata, 
               color = "grey70", size = .25) +
    geom_hline(aes(yintercept = conf.low), meandata, 
               color = "grey70", linetype = "dashed", size = .25) +
    geom_hline(aes(yintercept = conf.high), meandata, 
               color = "grey70", linetype = "dashed", size = .25) +
    geom_errorbar(position = pd, width = 0.0, size = 0.4) +
    geom_point(position = pd, size = 2.5) +
    facet_grid(~ species) +
    annotate("text", label = "n.s.", x = 3.2, y = 2.06) +
    scale_y_continuous(limits = c(0.5, 2.06), breaks = seq(-100, 100, 0.5)) +
    scale_shape_manual(values = c(1, 16, 15)) +
    labs(x = "",
         y = expression("Root-to-shoot ratio ["*g~g^-1*"]"),
         shape = "", color = "") +
    themeMB() +
    theme(strip.text = element_blank(), 
          strip.background = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          legend.position = "none")
  )

ggsave("figure_A3_H_acid_rootshootRatio_800dpi_8x7cm.tiff",
       dpi = 800, width = 8, height = 7, units = "cm",
       path = here("outputs", "figures", "supp"))
