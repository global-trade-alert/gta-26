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
library(splitstackshape)

gta_setwd()
gta26.path = "0 report production/GTA 26/"
data.path = "data/Worldwide and G20 intervention statistics/"
source(paste0(gta26.path, "help files/GTA 26 cutoff and definitions.R"))


### Table 1
# Get data
gta_data_slicer()
gta.intervention <- read_delim(file = "data/database replica/gta_intervention.csv", delim = ",")
gta.measure <- read_delim(file = "data/database replica/gta_measure.csv", delim = ",")

gta.all <- merge(gta.intervention, gta.measure, by.x = "measure_id", by.y = "id", all.x = T); rm(gta.intervention, gta.measure)
gta.all <- merge(master.sliced, gta.all, by.x = "intervention.id", by.y = "id", all.x = T); rm(master.sliced)

# Aggregate data for requested columns
table1 <- data.frame("measure" = c("Percentage of worldwide measures announced or implemented in 2020 based on official sources",
                                   "Percentage of worldwide measures announced or implemented ever based on official sources",
                                   "Percentage of G20 measures announced or implemented in 2020 based on official sources",
                                   "Percentage of G20 measures announced or implemented ever based on official sources",
                                   "Percentage of G20 subsidy measures announced or implemented in 2020 based on official sources",
                                   "Percentage of G20 subsidy measures announced or implemented ever based on official sources"),
                     "percentage" = c(length(unique(subset(gta.all, is_source_official == 1 & ((date.announced >= as.Date("2020-01-01") & date.announced <= as.Date(cutoff.date)) | (date.implemented >= as.Date("2020-01-01") & date.implemented <= as.Date(cutoff.date))))$intervention.id)) / length(unique(subset(gta.all, (date.announced >= as.Date("2020-01-01") & date.announced <= as.Date(cutoff.date)) | (date.implemented >= as.Date("2020-01-01") & date.implemented <= as.Date(cutoff.date)))$intervention.id)),
                                      length(unique(subset(gta.all, is_source_official == 1)$intervention.id)) / length(unique(gta.all$intervention.id)),
                                      length(unique(subset(gta.all, i.atleastone.G20 == 1 & is_source_official == 1 & ((date.announced >= as.Date("2020-01-01") & date.announced <= as.Date(cutoff.date)) | (date.implemented >= as.Date("2020-01-01") & date.implemented <= as.Date(cutoff.date))))$intervention.id)) / length(unique(subset(gta.all, i.atleastone.G20 == 1 & (date.announced >= as.Date("2020-01-01") & date.announced <= as.Date(cutoff.date)) | (date.implemented >= as.Date("2020-01-01") & date.implemented <= as.Date(cutoff.date)))$intervention.id)),
                                      length(unique(subset(gta.all, i.atleastone.G20 == 1 & is_source_official == 1)$intervention.id)) / length(unique(subset(gta.all, i.atleastone.G20 == 1)$intervention.id)),
                                      length(unique(subset(gta.all, mast.chapter == "L" & i.atleastone.G20 == 1 & is_source_official == 1 & ((date.announced >= as.Date("2020-01-01") & date.announced <= as.Date(cutoff.date)) | (date.implemented >= as.Date("2020-01-01") & date.implemented <= as.Date(cutoff.date))))$intervention.id)) / length(unique(subset(gta.all, mast.chapter == "L" & i.atleastone.G20 == 1 & (date.announced >= as.Date("2020-01-01") & date.announced <= as.Date(cutoff.date)) | (date.implemented >= as.Date("2020-01-01") & date.implemented <= as.Date(cutoff.date)))$intervention.id)),
                                      length(unique(subset(gta.all, mast.chapter == "L" & i.atleastone.G20 == 1 & is_source_official == 1)$intervention.id)) / length(unique(subset(gta.all, mast.chapter == "L" & i.atleastone.G20 == 1)$intervention.id)))
                     )

### Save data
save(table1, file = paste0(gta26.path, data.path, "worldwide and g20 intervention statistics.Rdata"))
