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
library(splitstackshape)

gta_setwd()
gta26.path = "0 report production/GTA 26/"
data.path = "data/G20 economic support measures/"
source(paste0(gta26.path, "help files/GTA 26 cutoff and definitions.R"))


### Table 1
# Get data
gta_data_slicer(gta.evaluation = c("Red", "Amber"),
                implementation.period = c(as.Date("2020-01-01"), as.Date(cutoff.date)),
                keep.implementation.na = F,
                implementing.country = country.names$un_code[country.names$is.g20],
                keep.implementer = T,
                intervention.types = unlist(economic.support.measures),
                keep.type = T)

# Aggregate for every country
table1 <- select(aggregate(intervention.id ~ implementing.jurisdiction, subset(master.sliced, intervention.type %in% unlist(economic.support.measures["Inward subsidy"])), function(x){length(unique(x))}), implementing.jurisdiction, "Inward subsidies" = intervention.id)
table1 <- merge(table1, select(aggregate(intervention.id ~ implementing.jurisdiction, subset(master.sliced, intervention.type %in% unlist(economic.support.measures["Export incentive and support in foreign markets"])), function(x){length(unique(x))}), implementing.jurisdiction, "Export incentives and support in foreign markets" = intervention.id),
                by = "implementing.jurisdiction", all = T)
table1 <- merge(table1, select(aggregate(intervention.id ~ implementing.jurisdiction, subset(master.sliced, intervention.type %in% unlist(economic.support.measures["Localisation measure"])), function(x){length(unique(x))}), implementing.jurisdiction, "Localisation measure" = intervention.id),
                by = "implementing.jurisdiction", all = T)
table1 <- merge(table1, select(aggregate(intervention.id ~ implementing.jurisdiction, subset(master.sliced, intervention.type %in% unlist(economic.support.measures["Government procurement measure"])), function(x){length(unique(x))}), implementing.jurisdiction, "Government procurement measures" = intervention.id),
                by = "implementing.jurisdiction", all = T)

# Add column with time-limited measures
table1 <- merge(table1, select(aggregate(intervention.id ~ implementing.jurisdiction, subset(master.sliced, !is.na(date.removed)), function(x){length(unique(x))}), implementing.jurisdiction, "Percentage of time-limited measures" = intervention.id),
                by = "implementing.jurisdiction", all = T)
table1[is.na(table1)] <- 0
table1$`Percentage of time-limited measures` <- table1$`Percentage of time-limited measures` / rowSums(table1[,2:5])

# Aggregate for every country - removing firm-specific transactions
table1.nofirm <- select(aggregate(intervention.id ~ implementing.jurisdiction, subset(master.sliced, eligible.firms!="firm-specific" & intervention.type %in% unlist(economic.support.measures["Inward subsidy"])), function(x){length(unique(x))}), implementing.jurisdiction, "Inward subsidies" = intervention.id)
table1.nofirm <- merge(table1.nofirm, select(aggregate(intervention.id ~ implementing.jurisdiction, subset(master.sliced, eligible.firms!="firm-specific" & intervention.type %in% unlist(economic.support.measures["Export incentive and support in foreign markets"])), function(x){length(unique(x))}), implementing.jurisdiction, "Export incentives and support in foreign markets" = intervention.id),
                       by = "implementing.jurisdiction", all = T)
table1.nofirm <- merge(table1.nofirm, select(aggregate(intervention.id ~ implementing.jurisdiction, subset(master.sliced, eligible.firms!="firm-specific" & intervention.type %in% unlist(economic.support.measures["Localisation measure"])), function(x){length(unique(x))}), implementing.jurisdiction, "Localisation measure" = intervention.id),
                       by = "implementing.jurisdiction", all = T)
table1.nofirm <- merge(table1.nofirm, select(aggregate(intervention.id ~ implementing.jurisdiction, subset(master.sliced, eligible.firms!="firm-specific" & intervention.type %in% unlist(economic.support.measures["Government procurement measure"])), function(x){length(unique(x))}), implementing.jurisdiction, "Government procurement measures" = intervention.id),
                       by = "implementing.jurisdiction", all = T)

# Add column with time-limited measures
table1.nofirm <- merge(table1.nofirm, select(aggregate(intervention.id ~ implementing.jurisdiction, subset(master.sliced, eligible.firms!="firm-specific" & !is.na(date.removed)), function(x){length(unique(x))}), implementing.jurisdiction, "Percentage of time-limited measures" = intervention.id),
                       by = "implementing.jurisdiction", all = T)
table1.nofirm[is.na(table1.nofirm)] <- 0
table1.nofirm$`Percentage of time-limited measures` <- table1.nofirm$`Percentage of time-limited measures` / rowSums(table1.nofirm[,2:5])



### Table 2

# Split the affected products and sectors
master.sliced <- cSplit(master.sliced, splitCols = "affected.product", sep = ", ", direction = "long")
master.sliced <- cSplit(master.sliced, splitCols = "affected.sector", sep = ", ", direction = "long")

# Load raw intervention data for horizontal column
load("data/database replica/database replica - parts - base.Rdata")
gta_intervention <- subset(gta_intervention, select = c("intervention_id", "is_horizontal_measure"))
master.sliced <- merge(master.sliced, gta_intervention, by.x = "intervention.id", by.y = "intervention_id", all.x = T)

# Aggregate for every country
table2 <- select(aggregate(intervention.id ~ implementing.jurisdiction, subset(master.sliced, affected.product %in% hs.codes$hs.code[hs.codes$is.agriculture]), function(x){length(unique(x))}), implementing.jurisdiction, "Percentage of harmful economic support measures that affect agricultural sectors" = intervention.id)
table2 <- merge(table2, select(aggregate(intervention.id ~ implementing.jurisdiction, subset(master.sliced, affected.product %in% hs.codes$hs.code[hs.codes$is.manufactured]), function(x){length(unique(x))}), implementing.jurisdiction, "Percentage of harmful support measures that affect manufacturing sectors" = intervention.id),
                by = "implementing.jurisdiction", all.x = T)
table2 <- merge(table2, select(aggregate(intervention.id ~ implementing.jurisdiction, subset(master.sliced, affected.sector >= 500), function(x){length(unique(x))}), implementing.jurisdiction, "Percentage of harmful support measures that affect service sectors" = intervention.id),
                by = "implementing.jurisdiction", all.x = T)
table2 <- merge(table2, select(aggregate(intervention.id ~ implementing.jurisdiction, subset(master.sliced, is_horizontal_measure == 1), function(x){length(unique(x))}), implementing.jurisdiction, "Percentage of harmful support measures that are classified as 'horizontal'" = intervention.id),
                by = "implementing.jurisdiction", all.x = T)
table2 <- merge(table2, select(aggregate(intervention.id ~ implementing.jurisdiction, subset(master.sliced, eligible.firms == "firm-specific"), function(x){length(unique(x))}), implementing.jurisdiction, "Percentage of harmful support measures that are classified as firm-specific" = intervention.id),
                by = "implementing.jurisdiction", all.x = T)

# Convert to percentages
table2 <- merge(table2, select(aggregate(intervention.id ~ implementing.jurisdiction, master.sliced, function(x){length(unique(x))}), implementing.jurisdiction, "total.nr.of.interventions" = intervention.id),
                by = "implementing.jurisdiction", all.x = T)
table2[is.na(table2)] <- 0

table2$`Percentage of harmful economic support measures that affect agricultural sectors` <- table2$`Percentage of harmful economic support measures that affect agricultural sectors` / table2$total.nr.of.interventions
table2$`Percentage of harmful support measures that affect manufacturing sectors` <- table2$`Percentage of harmful support measures that affect manufacturing sectors` / table2$total.nr.of.interventions
table2$`Percentage of harmful support measures that affect service sectors` <- table2$`Percentage of harmful support measures that affect service sectors` / table2$total.nr.of.interventions
table2$`Percentage of harmful support measures that are classified as 'horizontal'` <- table2$`Percentage of harmful support measures that are classified as 'horizontal'` / table2$total.nr.of.interventions
table2$`Percentage of harmful support measures that are classified as firm-specific` <- table2$`Percentage of harmful support measures that are classified as firm-specific` / table2$total.nr.of.interventions
table2$total.nr.of.interventions <- NULL

### Save data
save(table1, table2, file = paste0(gta26.path, data.path, "g20 economic support measures.Rdata"))
