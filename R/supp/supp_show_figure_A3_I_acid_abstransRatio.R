# Waste bricks for tree substrates
# Show Figure A3 I ####
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
rm(list = c("data", "meandata", "pd", "pdata", "m4"))
setwd(here("data", "processed"))

### Load data ###
(data <- read_csv("data_processed_acid.csv", col_names = T, na = "na", col_types = 
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
                           levels = c("Control_30", "Acid_5", "Acid_30")
                           )
                     )
                   ) %>%
    select(abstransRatio, plot, block, species, acidbrickRatioTreat, soilType,
           conf.low, conf.high) %>%
#Remove 1 outlier
    filter(abstransRatio < 6)
  )
  
data$acidbrickRatioTreat <- dplyr::recode(data$acidbrickRatioTreat,
                                          "Control_30" = "Control 30% bricks",
                                          "Acid_5" = "Acid 5% bricks",
                                          "Acid_30" = "Acid 30% bricks")


#### Chosen model ###
m3 <- lmer(log(abstransRatio) ~ (species + soilType + acidbrickRatioTreat)^2 +
             (1 | block), data, REML = FALSE)



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot #####################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


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
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}

### interaction: acid x brickRatio x species ###
pdata <- ggemmeans(m3, terms = c("acidbrickRatioTreat", "species"),
                   type = "fe")
pdata <- rename(pdata, abstransRatio = predicted,
                acidbrickRatioTreat = x, species = group)
meandata <- filter(pdata, acidbrickRatioTreat == "Control 30% bricks")
pd <- position_dodge(.6)

### plot ###
ann_text1 <- data.frame(acidbrickRatioTreat = "Acid 30% bricks", 
                        abstransRatio = 2.2,
                        species = factor("Acer", levels = c("Acer","Tilia")),
                        conf.low = 2.2,
                        conf.high = 2.2)
ann_text2 <- data.frame(acidbrickRatioTreat = "Control 30% bricks", 
                        abstransRatio = 2.2,
                        species = factor("Tilia", levels = c("Acer","Tilia")),
                        conf.low = 2.2,
                        conf.high = 2.2)
ann_text3 <- data.frame(acidbrickRatioTreat = "Acid 5% bricks", 
                        abstransRatio = 2.2,
                        species = factor("Tilia", levels = c("Acer","Tilia")),
                        conf.low = 2.2,
                        conf.high = 2.2)
ann_text4 <- data.frame(acidbrickRatioTreat = "Acid 30% bricks", 
                        abstransRatio = 2.2,
                        species = factor("Tilia", levels = c("Acer","Tilia")),
                        conf.low = 2.2,
                        conf.high = 2.2)
(abstransRatio <- ggplot(pdata,
                         aes(acidbrickRatioTreat, abstransRatio,
                             shape = acidbrickRatioTreat,
                             ymin = conf.low, ymax = conf.high)) +
    geom_quasirandom(data = data, aes(acidbrickRatioTreat, abstransRatio), 
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
    geom_text(data = ann_text1, label = "n.s.") +
    geom_text(data = ann_text2, label = "a") +
    geom_text(data = ann_text3, label = "ab") +
    geom_text(data = ann_text4, label = "b") +
    scale_y_continuous(limits = c(-0.02, 2.2), breaks = seq(-100, 100, 0.5)) +
    scale_shape_manual(values = c(1, 16, 15)) +
    labs(x = "",
         y = expression(Absorptive*":"*transport~roots~"["*g~g^-1*"]"),
         shape = "") +
    themeMB() +
    theme(strip.text = element_blank(), 
          strip.background = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          legend.text = element_text(size = 10),
          legend.position = "bottom")
  )

ggsave("figure_A3_I_acid_abstransRatio_800dpi_8x8cm.tiff",
       dpi = 800, width = 8, height = 8, units = "cm",
       path = here("outputs", "figures", "supp"))
