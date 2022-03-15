# Waste bricks for tree substrates
# Show Figure 1 ####
# Markus Bauer
# 2022-03-15



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ##############################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(here)
library(patchwork)

### Start ###
rm(list = c("data", "meandata", "pd", "pdata", "m4"))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot #####################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


(rgr13 | srl) / 
  (sla | rtd) / 
  (lmf | rmf) / 
  (smf | rootshootRatio) / 
  (abstransRatio | branchingIntensity) +
  plot_annotation(tag_levels = "A", tag_prefix = "(", tag_suffix = ")") +
  theme(plot.tag = element_text(size = 10))
ggsave("figure_1_(300dpi_17x22.5cm).tiff",
      dpi = 300, width = 17, height = 22.5, units = "cm",
      path = here("outputs", "figures"))
