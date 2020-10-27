rm(list = ls())

### REQUEST:
# 1. For the last chapter of the next GTA report, please calculate the following percentages:
#    - Percentage of worldwide measures announced or implemented in 2020 based on official sources.
#    - Percentage of worldwide measures announced or implemented ever based on official sources.
#    - Percentage of G20 measures announced or implemented in 2020 based on official sources.
#    - Percentage of G20 measures announced or implemented ever based on official sources.
#    - Percentage of G20 subsidy measures announced or implemented in 2020 based on official sources.
#    - Percentage of G20 subsidy measures announced or implemented ever based on official sources.

library(gtalibrary)
library(tidyverse)
library(openxlsx)

gta_setwd()
gta26.path = "0 report production/GTA 26/"
data.path = "data/Worldwide and G20 intervention statistics/"
out.path = "tables & figures/11 - Worldwide and G20 intervention statistics/"
source(paste0(gta26.path, "help files/GTA 26 cutoff and definitions.R"))

gta_colour_palette()

# Load data
load(paste0(gta26.path, data.path, "worldwide and g20 intervention statistics.Rdata"))

### Save Excel files of the data
write.xlsx(table1, file = paste0(gta26.path, out.path, "Table 1.xlsx"))


### No figures to plot! ###
