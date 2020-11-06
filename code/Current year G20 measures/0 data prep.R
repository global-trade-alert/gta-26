rm(list = ls())

### REQUEST:
# 1. Please produce a line chart for the years 2009 to 2020 with the following variable on the primary vertical axis: Total number of G20 commercial policy
# interventions worldwide documented by 21 October. On the second axis: please plot the percentage of G20 commercial policy interventions that are harmful.
# For the primary axis title use the text highlighted in bold. For the secondary axis title please use “Percentage of G20 measures that harm trading partners.”
#
# 2. For each G20 country please add up the total number of harmful measures implemented this year, the total number of liberalising measures implemented this
# year, and the total number of contingent protection investigations started this year but which have not resulted in duties imposed. Rank the G20 nations by
# these totals. Please produce a 2-D bar chart with G20 country as a separate entry where red is used to indicate the number of harmful interventions, green is
# used to indicate the liberalising interventions, and yellow to indicate the contingent protection investigations started this year. Please indicate in the chart
# the number of measures in each category for each G20 member. So for each G20 member, the number associated with each colour segment in the bar will be reported.
#
# 3. For each G20 member, focusing only on the harmful interventions implemented this year, please produce a 2-D 100% stacked bar which indicates the % of measures
# that have already lapsed (by 21 October), the % of measures due to lapse during the remainder of this year, the % of measures due to lapse in 2021, the % of
# measures due to lapse after 2021, and the % of measures without phase out dates.

library(gtalibrary)
library(tidyverse)
library(openxlsx)
library(splitstackshape)

gta_setwd()
gta26.path = "0 report production/GTA 26/"
data.path = "data/Current year G20 measures/"
source(paste0(gta26.path, "help files/GTA 26 cutoff and definitions.R"))


### Table 1
# Get data
gta_data_slicer(implementing.country = country.names$un_code[country.names$is.g20],
                keep.implementer = T,
                lag.adjustment = format(as.Date(cutoff.date), "%m-%d"))


# Aggregate per year and add percentage of harmful interventions
table1 <- merge(select(aggregate(intervention.id ~ year(date.published), master.sliced, function(x){length(unique(x))}), "year.published" = "year(date.published)", "total.nr.of.int" = intervention.id),
                select(aggregate(intervention.id ~ year(date.published), subset(master.sliced, gta.evaluation %in% c("Red", "Amber")), function(x){length(unique(x))}), "year.published" = "year(date.published)", "nr.of.harmful.interventions" = intervention.id),
                by = "year.published", all.x = T)

table1$harmful.perc <- table1$nr.of.harmful.interventions / table1$total.nr.of.int
table1$nr.of.harmful.interventions <- NULL


### Table 2
# Get data
gta_data_slicer(implementing.country = country.names$un_code[country.names$is.g20],
                keep.implementer = T)

# Convert gta.evaluation into harmful and liberalising
master.sliced$gta.evaluation <- ifelse(master.sliced$gta.evaluation %in% c("Red", "Amber"), "harmful", "liberalising")

# Aggregate the harmful and liberalising interventions for every country
table2 <- merge(select(aggregate(intervention.id ~ implementing.jurisdiction, subset(master.sliced, gta.evaluation == "harmful" & !is.na(date.implemented) & date.implemented >= as.Date("2020-01-01") & date.implemented <= as.Date(cutoff.date)), function(x){length(unique(x))}), implementing.jurisdiction, "nr.of.harmful.int" = intervention.id),
                select(aggregate(intervention.id ~ implementing.jurisdiction, subset(master.sliced, gta.evaluation == "liberalising" & !is.na(date.implemented) & date.implemented >= as.Date("2020-01-01") & date.implemented <= as.Date(cutoff.date)), function(x){length(unique(x))}), implementing.jurisdiction, "nr.of.liberalising.int" = intervention.id),
                by = "implementing.jurisdiction", all.x = T)

# Aggregate chapter D investigations that have not resulted in duties imposed
# investigation.status.history <- read_delim("data/database replica/gta_investigation_status_history.csv", delim = ",")
# investigation.status <- bar <- read_delim("data/database replica/gta_investigation_status.csv", delim = ",")
# 
# investigation.status.history <- merge(investigation.status.history, investigation.status, by.x = "status_id", by.y = "id", all.x = T)
# data2 <- merge(subset(master.sliced, mast.chapter == "D" & !is.na(date.implemented) & date.implemented >= as.Date("2020-01-01") & date.implemented <= as.Date(cutoff.date)), investigation.status.history, by.x = "intervention.id", by.y = "intervention_id", all.x = T)
# data2 <- select(aggregate(intervention.id ~ implementing.jurisdiction, subset(data2, !label %in% c("preliminary duty", "definitive duty", "extended duty")), function(x){length(unique(x))}), implementing.jurisdiction, "nr.of.cont.protection.int.RE" = intervention.id)
# table2 <- merge(table2, data2, by = "implementing.jurisdiction", all.x = T)

data2 <- select(aggregate(intervention.id ~ implementing.jurisdiction, subset(master.sliced, mast.chapter == "D" & date.announced >= as.Date("2020-01-01") & date.announced <= as.Date(cutoff.date) & is.na(date.implemented)), function(x){length(unique(x))}), implementing.jurisdiction, "nr.of.cont.protection.int" = intervention.id)
table2 <- merge(table2, data2, by = "implementing.jurisdiction", all.x = T)

# Merge the chapter D numbers with the rest
# table2 <- merge(table2, data2, by = "implementing.jurisdiction", all.x = T); rm(data2, investigation.status, investigation.status.history)
table2[is.na(table2)] <- 0

# Add column of total interventions per country to rank the countries in the bar chart
table2$total.nr.of.interventions <- table2$nr.of.harmful.int + table2$nr.of.liberalising.int + table2$nr.of.cont.protection.int


### Table 3
# Get data
gta_data_slicer(gta.evaluation = c("Red", "Amber"),
                implementation.period = c(as.Date("2020-01-01"), as.Date(cutoff.date)),
                keep.implementation.na = F,
                implementing.country = country.names$un_code[country.names$is.g20],
                keep.implementer = T)

# Aggregate total number of interventions per country and number of already lapsed interventions
table3 <- merge(select(aggregate(intervention.id ~ implementing.jurisdiction, master.sliced, function(x){length(unique(x))}), implementing.jurisdiction, "total.nr.of.interventions" = intervention.id),
                select(aggregate(intervention.id ~ implementing.jurisdiction, subset(master.sliced, !is.na(date.removed) & date.removed <= as.Date(cutoff.date)), function(x){length(unique(x))}), implementing.jurisdiction, "perc.of.lapsed.interventions" = intervention.id),
                by = "implementing.jurisdiction", all.x = T)

# Add number of interventions due to lapse during the rest of the year
table3 <- merge(table3, select(aggregate(intervention.id ~ implementing.jurisdiction, subset(master.sliced, !is.na(date.removed) & date.removed > as.Date(cutoff.date) & date.removed <= as.Date("2020-12-31")), function(x){length(unique(x))}), implementing.jurisdiction, "perc.of.interventions.to.lapse.rest.of.year" = intervention.id),
                by = "implementing.jurisdiction", all.x = T)

# Add number of interventions due to lapse during 2021
table3 <- merge(table3, select(aggregate(intervention.id ~ implementing.jurisdiction, subset(master.sliced, !is.na(date.removed) & date.removed >= as.Date("2021-01-01") & date.removed <= as.Date("2021-12-31")), function(x){length(unique(x))}), implementing.jurisdiction, "perc.of.interventions.to.lapse.2021" = intervention.id),
                by = "implementing.jurisdiction", all.x = T)

# Add number of interventions due to lapse after 2021
table3 <- merge(table3, select(aggregate(intervention.id ~ implementing.jurisdiction, subset(master.sliced, !is.na(date.removed) & date.removed >= as.Date("2022-01-01")), function(x){length(unique(x))}), implementing.jurisdiction, "perc.of.interventions.to.lapse.after.2021" = intervention.id),
                by = "implementing.jurisdiction", all.x = T)

# Add number of interventions without removal date
table3 <- merge(table3, select(aggregate(intervention.id ~ implementing.jurisdiction, subset(master.sliced, is.na(date.removed)), function(x){length(unique(x))}), implementing.jurisdiction, "perc.of.interventions.without.phaseout.date" = intervention.id),
                by = "implementing.jurisdiction", all.x = T)

# Convert everything to percentages and remove the column with total interventions
table3[is.na(table3)] <- 0
table3$perc.of.lapsed.interventions <- table3$perc.of.lapsed.interventions / table3$total.nr.of.interventions
table3$perc.of.interventions.to.lapse.rest.of.year <- table3$perc.of.interventions.to.lapse.rest.of.year / table3$total.nr.of.interventions
table3$perc.of.interventions.to.lapse.2021 <- table3$perc.of.interventions.to.lapse.2021 / table3$total.nr.of.interventions
table3$perc.of.interventions.to.lapse.after.2021 <- table3$perc.of.interventions.to.lapse.after.2021 / table3$total.nr.of.interventions
table3$perc.of.interventions.without.phaseout.date <- table3$perc.of.interventions.without.phaseout.date / table3$total.nr.of.interventions

table3$total.nr.of.interventions <- NULL

### Save data
save(table1, table2, table3, file = paste0(gta26.path, data.path, "current year g20 measures.Rdata"))
