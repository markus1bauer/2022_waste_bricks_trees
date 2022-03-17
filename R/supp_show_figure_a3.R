# Waste bricks for tree substrates
# Show Figure A3 ####
# Markus Bauer
# 2022-03-15



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ##############################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(here)
library(patchwork)

### Start ###
rm(list = c("data", "meandata", "pd", "pdata", "m3"))
setwd(here("data", "processed"))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ####################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


(rgr13 | srl) /
  (sla | rtd) /
  (lmf | rmf) /
  (smf | rootshootRatio) /
  (abstransRatio | branchingIntensity) +
  guide_area() +
  plot_layout(
    guides = "collect",
    heights = c(7, 7, 7, 7, 7, 1)
    ) +
  plot_annotation(
    tag_levels = "A", tag_prefix = "(", tag_suffix = ")") +
  theme(plot.tag = element_text(size = 10), legend.position = "bottom")

ggsave("supp_figure_a3_300dpi_17x22.5cm.tiff",
       dpi = 300, width = 17, height = 22.5, units = "cm",
       path = here("outputs", "figures"))
