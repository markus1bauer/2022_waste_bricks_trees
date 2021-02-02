# Show Figure specific root length  ~ soilType:species:brickRatio ####
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
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_trees/data/processed")

### Load data ###
data <- read_csv2("data_processed_brickRatio.csv", col_names = T, na = "na", col_types = 
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
)
(data <- select(data, branchingIntensity, plot, block, species, brickRatio, soilType, mycorrhiza, conf.low, conf.high))

#### Chosen model ###
m4 <- lmer(log(branchingIntensity) ~ (species + brickRatio + soilType + mycorrhiza)^2 +
             species:brickRatio:soilType + species:brickRatio:mycorrhiza +
             (1|block), data, REML = F)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plotten ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
themeMB <- function(){
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
    legend.position = "right",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}

### acid:soilType ###
pdata <- ggemmeans(m4, terms = c("soilType","brickRatio", "species"), type = "fe")
pdata <- rename(pdata, branchingIntensity = predicted, soilType = x, brickRatio = group, species = facet)
meandata <- filter(pdata, soilType == "poor" & brickRatio == "5")
pd <- position_dodge(.6)
(branchingIntensity <- ggplot(pdata, aes(soilType, branchingIntensity, shape = brickRatio, ymin = conf.low, ymax = conf.high)) +
  geom_quasirandom(data = data, aes(soilType, branchingIntensity), 
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
  annotate("text", label = "n.s.", x = 2.2, y = 12) +
  scale_y_continuous(limits = c(0, 12), breaks = seq(-100, 150, 2)) +
  scale_shape_manual(values = c(1, 16)) +
  labs(x = "Soil fertility", y = expression(Branching~intensity~"["*tips~cm^-1*"]"), shape = "Brick ratio [%]", color = "") +
  themeMB() +
  theme(strip.text = element_blank(), 
        strip.background = element_blank(),
        legend.position = "none")
)

#ggsave("figure_1_J_branchingIntensity_(800dpi_12x7cm).tiff",
 #     dpi = 800, width = 12, height = 7, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_trees/outputs/figures")
