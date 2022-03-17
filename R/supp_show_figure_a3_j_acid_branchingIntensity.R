# Waste bricks for tree substrates
# Show Figure A3 J ####
# Markus Bauer
# 2022-03-15



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
rm(list = c("data", "meandata", "pd", "pdata", "m3", "ann_text1", "ann_text2",
            "ann_text3", "ann_text4"))
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
                       soilType = col_factor(levels = c("poor", "rich")),
                       brickRatio = col_factor(levels = c("5", "30")),
                       acid = col_factor(levels = c("Control", "Acid")),
                       acidbrickRatioTreat =
                         col_factor(
                           levels = c("Control_30", "Acid_5", "Acid_30"
                                      )
                           )
                     )
                   ) %>%
    select(branchingIntensity, plot, block, species, acidbrickRatioTreat,
           soilType, conf.low, conf.high)
  )
  
data$acidbrickRatioTreat <- dplyr::recode(data$acidbrickRatioTreat,
                                          "Control_30" = "Control 30% bricks",
                                          "Acid_5" = "Acid 5% bricks",
                                          "Acid_30" = "Acid 30% bricks")

#### Chosen model ###
m3 <- lmer(log(branchingIntensity) ~
             (species + soilType + acidbrickRatioTreat)^2 +
             (1 | block), data, REML = FALSE)



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot #####################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


themeMB <- function() {
  theme(
    panel.background = element_rect(fill = "white"),
    text  = element_text(size = 8, color = "black"),
    axis.title.y = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(angle = 90, hjust = 0.5),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.line.y = element_line(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.key = element_rect(fill = "white"),
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}

### brickRatio:mycorrhiza ###
pdata <- ggemmeans(m3, terms = c("acidbrickRatioTreat", "species"),
                   type = "fe")
pdata <- rename(pdata, branchingIntensity = predicted,
                acidbrickRatioTreat = x, species = group)
meandata <- filter(pdata, acidbrickRatioTreat == "Control 30% bricks")
pd <- position_dodge(.6)

### plot ###
(branchingIntensity <- ggplot(pdata,
                              aes(acidbrickRatioTreat, branchingIntensity,
                                  shape = acidbrickRatioTreat,
                                  ymin = conf.low, ymax = conf.high)) +
    geom_quasirandom(data = data, aes(acidbrickRatioTreat, branchingIntensity),
                     color = "grey70", dodge.width = .6, size = 0.7) +
    geom_hline(aes(yintercept = branchingIntensity), meandata,
               color = "grey70", size = .25) +
    geom_hline(aes(yintercept = conf.low), meandata,
               color = "grey70", linetype = "dashed", size = .25) +
    geom_hline(aes(yintercept = conf.high), meandata,
               color = "grey70", linetype = "dashed", size = .25) +
    geom_errorbar(position = pd, width = 0.0, size = 0.4) +
    geom_point(position = pd, size = 2.5) +
    facet_grid(~ species) +
    annotate("text", label = "n.s.", x = 3.2, y = 12) +
    scale_y_continuous(limits = c(0, 12), breaks = seq(-100, 150, 2)) +
    scale_shape_manual(values = c(1, 16, 15)) +
    labs(x = "",
         y = expression(Branching~intensity~"["*tips~cm^-1*"]"),
         shape = "", color = "") +
    themeMB() +
    theme(strip.text = element_blank(),
          strip.background = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          legend.text = element_text(size = 10),
          legend.position = "bottom")
)
