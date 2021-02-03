# Show Figure of all traits ~ soilType:species:brickRatio ####
# Markus Bauer
# Citation: Markus Bauer, Martin Krause, Valentin Heizinger & Johannes Kollmann  (2021) ...
# DOI: ...



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(patchwork)

### Start ###
rm(list = c("data", "meandata", "pd", "pdata", "m4"))
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_trees/data/processed")



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(rgr13 | srl) / 
  (sla | rtd) / 
  (lmf | rmf) / 
  (smf | rootshootRatio) / 
  (abstransRatio | branchingIntensity) +
  plot_annotation(tag_levels = "A", tag_prefix = "(", tag_suffix = ")") +
  theme(plot.tag = element_text(size = 10))
ggsave("figure_1_(300dpi_17x22.5cm).tiff",
      dpi = 300, width = 17, height = 22.5, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_trees/outputs/figures")
