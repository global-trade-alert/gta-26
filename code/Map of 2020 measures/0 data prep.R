rm(list = ls())

### REQUEST:
# 1. Please produce a map that shows for each country the total number of measures of any type recorded this year in the GTA database. Please use the following
# ranges for the map: 0 measures, 1-9 measures, 10-50 measures, 50-100 measures, more than 100 measures. Please add at the bottom of the map the text
# “Source: Global Trade Alert. Map relates only to policy intervention announced or implemented from 1 January 2020 to 21 October 2020.”

library(gtalibrary)
library(tidyverse)
library(openxlsx)
library(splitstackshape)

gta_setwd()
gta26.path = "0 report production/GTA 26/"
data.path = "data/Map of 2020 measures/"
source(paste0(gta26.path, "help files/GTA 26 cutoff and definitions.R"))


### Table 1
# Get data
# gta_data_slicer(implementation.period = c(as.Date("2020-01-01"), as.Date(cutoff.date)),
#                 keep.implementation.na = F)

gta_data_slicer()
master.sliced = subset(master.sliced, (date.announced>=as.Date("2020-01-01") & date.announced<=as.Date("2020-12-31")) | (date.implemented>=as.Date("2020-01-01") & date.implemented<=as.Date("2020-12-31")))


# Aggregate per implementing country
table1 <- select(aggregate(intervention.id ~ implementing.jurisdiction, master.sliced, function(x){length(unique(x))}), implementing.jurisdiction, "nr.of.interventions" = intervention.id)

# Add UN codes
table1 <- merge(select(country.names, name, un_code), table1, by.x = "name", by.y = "implementing.jurisdiction", all.x = T)
table1[is.na(table1)] <- 0

# Add grouping variable
table1$group <- "1"
table1$group[table1$nr.of.interventions > 0 & table1$nr.of.interventions <= 9] <- "2"
table1$group[table1$nr.of.interventions > 9 & table1$nr.of.interventions <= 50] <- "3"
table1$group[table1$nr.of.interventions > 50 & table1$nr.of.interventions <= 100] <- "4"
table1$group[table1$nr.of.interventions > 100] <- "5"


### Save data
save(table1, file = paste0(gta26.path, data.path, "map of 2020 measures.Rdata"))
