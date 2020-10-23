rm(list = ls())

### REQUEST:
# For the purpose of this chapter define “Economic support measures” as
# •	Any inward subsidy (MAST chapter L plus price stabilisation policies if not included in chapter L)
# •	Export incentives and support in foreign markets (MAST chapter M excluding any export limits or restrictions but including financial assistance in foreign markets)
# •	All localisation measures (all four of them)
# •	All government procurement measures (all four of them)
# Only extract information for harmful economic support measures implemented this year.
#
# 1. Please produce a table where each G20 member is a row. The columns of the table refer to the number of economic support measures in the four categories above
# (please add one table column for each category). Then add a final column with the % of economic support measures that are time-limited (that is, that have phase
# out dates).
#
# 2. Please produce a table where each G20 member is a row. The table should have the following five columns:
# a.	% of harmful economic support measures that affect agricultural sectors.
# b.	% of harmful support measures that affect manufacturing sectors.
# c.	% of harmful support measures that affect service sectors.
# d.	% of harmful support measures that are classified as “horizontal”.
# e.	% of harmful support measures that are classified as firm-specific.

library(gtalibrary)
library(tidyverse)
library(openxlsx)

gta_setwd()
gta26.path = "0 report production/GTA 26/"
data.path = "data/G20 economic support measures/"
out.path = "tables & figures/8 - G20 economic support measures/"
source(paste0(gta26.path, "help files/GTA 26 cutoff and definitions.R"))

# Load data
load(paste0(gta26.path, data.path, "g20 economic support measures.Rdata"))

### Save Excel files of the data
write.xlsx(table1, file = paste0(gta26.path, out.path, "Table 1.xlsx"))
write.xlsx(table2, file = paste0(gta26.path, out.path, "Table 2.xlsx"))


### No charts to plot ###
