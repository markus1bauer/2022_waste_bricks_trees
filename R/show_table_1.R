# Show Figure specific leaf area  ~ soilType:species:brickRatio ####
# Markus Bauer
# Citation: Markus Bauer, Martin Krause, Valentin Heizinger & Johannes Kollmann  (2021) ...
# DOI: ...



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ##############################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(here)
library(tidyverse)
library(gt)
library(webshot)
library(flextable)
library(officer)

### Start ###
rm(list = ls())
setwd(here("outputs", "tables"))

### Load data ###
data <- c()
data$substrate <- c(1:6)
data$soilFertility <- c(rep("Rich", 3), rep("Poor", 3))
data$brickRatio <- c(5, 30, 30, 5, 30, 30)
data$acid <- c("Yes", "Yes", "No", "Yes", "Yes", "No")
data$soilTexture <- c("Sl2", "Sl2", "Su3", "Su2", "Su2", "Su2")
data$fineSand <- c(21, 18, 18, 22, 22, 19)
data$silt <- c(23, 24, 26, 16, 13, 17)
data$pH <- c(7.6, 7.7, 7.9, 7.7, 8.1, 8.1)
data$C <- c(2.7, 2.3, 2.3, 0.7, 0.7, 0.7)
data$P <- c(34, 57, 20, 32, 25, 12)
data$K <- c(8, 8, 12, 7, 7, 11)
data$Mg <- c(19, 54, 23, 19, 30, 26)
data <- as_tibble(data)



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot with gt #############################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


(table1 <- data %>%
    gt() %>%
    opt_table_lines(
      extent = "none"
      ) %>%
   #general settings
    tab_options(
      table.font.style = "Arial",
      table.font.size = px(12),
      table.font.color = "black",
      data_row.padding = px(4),
      table.align = "left",
      column_labels.border.top.style = "solid",
      table_body.border.bottom.style = "solid",
      table_body.border.top.style = "solid",
      column_labels.border.top.color = "black",
      table_body.border.bottom.color = "black",
      table_body.border.top.color = "black",
      column_labels.border.top.width = px(2),
      table_body.border.bottom.width = px(2),
      table_body.border.top.width = px(1)
      ) %>%
   #general alignments
    tab_style(
      style = cell_text(
        v_align = "top",
        align = "center"
        ),
      locations = cells_column_labels(
        columns = TRUE
        )
    ) %>%
   #general alignments
    tab_style(
      style = cell_text(
        align = "center",
        v_align = "middle"
      ),
      locations = cells_body(
        columns = TRUE
      )
    ) %>%
   #general alignments
   tab_style(
     style = cell_text(
       align = "left"
     ),
     locations = cells_column_labels(
       columns = "substrate"
     )
   ) %>%
   #general alignments
   tab_style(
      style = cell_text(
        align = "left"
        ),
      locations = cells_body(
          columns = "substrate"
          )
      ) %>%
   #heading
   tab_header(
     title = "Table 1"
   ) %>%
   tab_style(
     style = cell_text(
       align = "left"
     ),
     locations = cells_title()
   ) %>%
   #spanners
   tab_spanner(
     label = "Treatments",
     columns = vars(
       brickRatio, acid, soilFertility
     )
   ) %>%
   tab_spanner(
     label = "Grain size distribution",
     columns = vars(
       soilTexture, fineSand, silt
     )
   ) %>%
   tab_spanner(
     label = "Chemical analysis",
     columns = vars(
       pH, C, P, K, Mg
     )
   ) %>%
   #styles of spanners
   tab_style(
     style = cell_text(
       weight = "bold"
     ),
     locations = cells_column_spanners(
       spanners = TRUE
     )
   ) %>%
   tab_style(
     style = cell_borders(
       sides = c("bottom"),
       color = "black",
       style = "solid",
       weight = px(1)
     ),
     locations = cells_column_spanners(
       spanners = TRUE
     )
   ) %>%
   #column labels
   cols_label(
      substrate = md("**Substrate**"), 
      soilFertility = md("**Fertility**"), 
      brickRatio = md("**Brick ratio**<br/>[vol%]"), 
      acid = md("**Acid**"), 
      soilTexture = md("**Texture**"), 
      fineSand = md("**Fine sand**<br/>[wt%]"), 
      silt = md("**Silt**<br/>[wt%]"), 
      pH = md("**pH<br/>**"), 
      C = md("**C organic**<br/>[wt%]"),
      P = md("**P<sub>2</sub>O<sub>5</sub>**<br/>[mg 100 g<sup>-1</sup>]"), 
      K = md("**K<sub>2</sub>O**<br/>[mg 100 g<sup>-1</sup>]"),
      Mg = md("**Mg<sup>2+</sup>**<br/>[mg 100 g<sup>-1</sup>]")
      ) %>%
   #footnote
   tab_source_note(
     source_note = "Bauer, Heizinger, Kollmann (20xx): Ecological application of waste bricks: No negative effects of brick-augmented substrates on urban tree growth. - Journal. DOI: xxx"
   )
)

### Save ###
gtsave(table1, "table_1.html")
gtsave(table1, "table_1.png")



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Plot with flextable ######################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


(table1flextable <- data %>%
    flextable %>%
    align(
      align = "center",
      part = "all"
    ) %>%
    valign(
      valign = "top",
      part = "header"
      ) %>%
    hline_top(
      border = fp_border(color = "black", width = 2),
      part = "header"
    ) %>%
    hline_top(
      border = fp_border(color = "black", width = 1),
      part = "body"
    ) %>%
    hline_bottom(
      border = fp_border(color = "black", width = 2),
      part = "body"
    ) %>%
    fontsize(
      size = 10,
      part = "all"
    ) %>%
    bold(
      bold = T,
      part = "header"
    ) %>%
    fit_to_width(
      max_width = 22.5
    ) %>%
    set_header_labels(
      substrate = "Substrate", 
      soilFertility = "Fertility", 
      brickRatio = "Brick ratio [vol%]", 
      acid = "Acid", 
      soilTexture = "Texture", 
      fineSand = "Fine sand [wt%]", 
      silt = "Silt [wt%]", 
      pH = "pH", 
      C = "C organic [wt%]",
      P = "P2O5 [mg 100 g-1]", 
      K = "K2O [mg 100 g-1]",
      Mg = "Mg2+ [mg 100 g-1]"
    )
)

### Save ###
path <- here("outputs", "tables", "table_1.docx")
save_as_docx(table1flextable, path = path)
