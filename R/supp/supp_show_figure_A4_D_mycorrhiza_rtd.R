# Waste bricks for tree substrates
# Show Figure A4 D ####
# Markus Bauer
# 2022-03-15



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
setwd(here("data", "processed"))

### Load data ###
data <- read_csv2("data_processed_brickRatio.csv",
                  col_names = TRUE, na = "na", col_types =
                    cols(
                      .default = col_double(),
                      plot = col_factor(),
                      block = col_factor(),
                      replanted = col_factor(),
                      species = col_factor(),
                      mycorrhiza =
                        col_factor(levels = c("Control", "Mycorrhiza")),
                      substrate = col_factor(),
                      soilType = col_factor(levels = c("poor", "rich")),
                      brickRatio = col_factor(levels = c("5", "30")),
                      acid = col_factor(),
                      acidbrickRatioTreat = col_factor()
                    )
                  ) %>%
  select(rtd, plot, block, species, brickRatio, soilType, mycorrhiza,
         conf.low, conf.high) %>%
#Exclude 2 outlier
  filter(rtd < 1000 & rtd > -80)

#### Chosen model ###
m4 <- lmer(log(rtd) ~ (species + brickRatio + soilType + mycorrhiza)^2 +
             species:brickRatio:soilType + species:brickRatio:mycorrhiza +
             (1 | block), data, REML = FALSE)



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot #####################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


themeMB <- function(){
  theme(
    panel.background = element_rect(fill = "white"),
    text  = element_text(size = 8, color = "black"),
    axis.text.y = element_text(angle = 90, hjust = 0.5),
    strip.text = element_text(size = 11),
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
pdata <- ggemmeans(m4, terms = c("mycorrhiza", "brickRatio", "species"),
                   type = "fe")
pdata <- rename(pdata, rtd = predicted, mycorrhiza = x, brickRatio = group,
                species = facet)
meandata <- filter(pdata, mycorrhiza == "Control" & brickRatio == "5")
pd <- position_dodge(.6)

### plot ###
(rtd <- ggplot(pdata, aes(mycorrhiza, rtd, shape = brickRatio,
                          ymin = conf.low, ymax = conf.high)) +
    geom_quasirandom(data = data, aes(mycorrhiza, rtd), 
                     color = "grey70", dodge.width = .6, size = .7) +
    geom_hline(aes(yintercept = rtd), meandata, 
               color = "grey70", size = .25) +
    geom_hline(aes(yintercept = conf.low), meandata, 
               color = "grey70", linetype = "dashed", size = .25) +
    geom_hline(aes(yintercept = conf.high), meandata, 
               color = "grey70", linetype = "dashed", size = .25) +
    geom_errorbar(position = pd, width = .0, size = .4) +
    geom_point(position = pd, size = 2.5) +
    facet_grid(~ species) +
    annotate("text", label = "n.s.", x = 2.2, y = 9.5) +
    scale_y_continuous(limits = c(1.5, 9.5), breaks = seq(-100, 150, 1)) +
    scale_shape_manual(values = c(1, 16)) +
    labs(x = "Mycorrhiza",
         y = expression(Root~tissue~density[1-3]~"["*g~cm^-3*"]"),
         shape = "Brick ratio [%]", color = "") +
    themeMB() +
    theme(strip.text = element_blank(), 
          strip.background = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          legend.position = "none")
)

ggsave("figure_A4_D_mycorrhiza_rtd_800dpi_12x6cm.tiff",
       dpi = 800, width = 12, height = 6, units = "cm",
       path = here("outputs", "figures", "supp"))
