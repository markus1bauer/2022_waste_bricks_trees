# Waste bricks for tree substrates
# Show Figure A1 ####
# Markus Bauer
# 2022-03-15



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ##############################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(here)
library(tidyverse)

### Start ###
rm(list = ls())
setwd(here("data", "processed"))

### Load data ###
edata <- read_csv("supp_data_processed_brickRatio.csv",
                     col_names = TRUE, na = "na", col_types =
                       cols(
                         .default = col_double(),
                         substrate = col_factor(),
                         substrateAbb = col_factor()
                       )      
)



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plotten ##################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


themeMB <- function() {
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "grey80"),
    text  = element_text(size = 10, color = "black"),
    axis.line.y = element_line(),
    axis.line.x = element_line(),
    axis.text.x = element_text(angle = 270),
    axis.ticks.x = element_line(),
    legend.key = element_rect(fill = "white"),
    legend.position = "right",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}


ggplot(edata, aes(x = grainSize, y = grainSizeCum,
                  color = substrateAbb, linetype = substrateAbb)) +
  geom_line(size = .8) +
  scale_color_manual(
    values = c(
      "brown3", "blue", "coral", "cyan", "black", "black"
      )
    ) +
  scale_linetype_manual(
    values = c(
      "solid", "solid", "solid", "solid", "dotted", "dotted"
      )
    ) +
  scale_x_log10(breaks = c(0.002, 0.063, 0.2, 0.63, 2, 4, 8, 16, 25, 31.5)) +
  labs(x = "Grain size [mm]", y = "Cumulative ratio [wt%]",
       linetype = "", color = "") +
  themeMB()

ggsave("supp_figure_a1_800dpi_16x10cm.tiff",
      dpi = 800, width = 16, height = 10, units = "cm",
      path = here("outputs", "figures"))
