rm(list = ls())

### REQUEST:
# 1. Please produce a heat map where for each G20 country the number of hits to every other G20 members commercial interests are shown. This heat map should be
# based only on harmful policy interventions implemented this year.
#
# 2. Please repeat 1 but for liberalising policy interventions.
#
# 3. Please repeat 1 (again for harmful policy interventions) but the countries affected in this heat map are the following groups: the Least Developed Countries,
# Sub-Saharan Africa, African Union, Latin American and Caribbean nations, EBRD countries of operation, ASEAN, East Asia and Pacific, and Eurasian Economic Union.
# As the number of countries in each of group of nations differs, then please produce the heat map based on the total number of hits to a group divided by the total
# number of members of each group. Please take account of the fact that a G20 member’s policy intervention can hurt more than one member of a group—therefore, could
# each harmed country towards the total number of hits by a G20 country on a given group’s commercial interests.
#
# 4. Please repeat 3 but for liberalising policy interventions.

library(gtalibrary)
library(tidyverse)
library(openxlsx)
library(splitstackshape)

gta_setwd()
gta26.path = "0 report production/GTA 26/"
data.path = "data/Heat maps/"
source(paste0(gta26.path, "help files/GTA 26 cutoff and definitions.R"))


### Tables 1 & 2
# Get data
gta_data_slicer(implementation.period = c(as.Date("2020-01-01"), as.Date(cutoff.date)),
                keep.implementation.na = F,
                implementing.country = country.names$un_code[country.names$is.g20],
                keep.implementer = T,
                affected.country = country.names$un_code[country.names$is.g20],
                keep.affected = T,
                submission.period = c("1999-01-01", cutoff.date))

# Convert GTA evaluation into two categories
master.sliced$gta.evaluation <- ifelse(master.sliced$gta.evaluation %in% c("Red", "Amber"), "harmful", "liberalising")

# Keep only G20 countries in affected jurisdictions
master.sliced <- subset(master.sliced, affected.jurisdiction %in% country.names$name[country.names$is.g20])

# Prepare a table with every combination
table1 <- data.frame("implementing.jurisdiction" = country.names$name[country.names$is.g20],
                     "affected.jurisdiction" = paste(country.names$name[country.names$is.g20], collapse = ", "))
table1 <- cSplit(table1, splitCols = "affected.jurisdiction", sep = ", ", direction = "long")

table1$gta.evaluation <- "harmful, liberalising"
table1 <- cSplit(table1, splitCols = "gta.evaluation", sep = ", ", direction = "long")
table1=subset(table1, implementing.jurisdiction!=affected.jurisdiction)

# Aggregate for every combination
table1 <- merge(table1, select(aggregate(intervention.id ~ implementing.jurisdiction + affected.jurisdiction + gta.evaluation, master.sliced, function(x){length(unique(x))}), implementing.jurisdiction, affected.jurisdiction, gta.evaluation, "nr.of.interventions" = intervention.id),
                by = c("implementing.jurisdiction", "affected.jurisdiction", "gta.evaluation"), all.x = T)

table1[is.na(table1)] <- 0

### Tables 3 & 4
# Get data
gta_data_slicer(implementation.period = c(as.Date("2020-01-01"), as.Date(cutoff.date)),
                keep.implementation.na = F,
                implementing.country = country.names$un_code[country.names$is.g20],
                keep.implementer = T)

# Convert GTA evaluation into two categories
master.sliced$gta.evaluation <- ifelse(master.sliced$gta.evaluation %in% c("Red", "Amber"), "harmful", "liberalising")

# Aggregate the data for every implementer
table2 <- data.frame()
for (a.group in names(se.country.groups)){
  temp <- select(aggregate(intervention.id ~ implementing.jurisdiction + affected.jurisdiction + gta.evaluation, subset(master.sliced, affected.jurisdiction %in% unlist(se.country.groups[a.group])), function(x){length(unique(x))}), implementing.jurisdiction, gta.evaluation, "nr.of.interventions.per.member" = intervention.id)
  
  # Second aggregation step to address this point: "Please take account of the fact that a G20 member’s policy intervention can hurt more than one member of a
  # group—therefore, could each harmed country towards the total number of hits by a G20 country on a given group’s commercial interests."
  temp <- aggregate(nr.of.interventions.per.member ~ implementing.jurisdiction + gta.evaluation, temp, sum)
  temp$nr.of.interventions.per.member=temp$nr.of.interventions.per.member/length(unique(unlist(se.country.groups[a.group])))
  temp$affected.jurisdiction <- a.group
  table2 <- rbind(table2, temp)
}; rm(temp)

# Prepare a table with every combination
temp <- data.frame("implementing.jurisdiction" = country.names$name[country.names$is.g20],
                   "affected.jurisdiction" = paste(names(se.country.groups), collapse = ", "))
temp <- cSplit(temp, splitCols = "affected.jurisdiction", sep = ", ", direction = "long")
temp$gta.evaluation <- "harmful, liberalising"
temp <- cSplit(temp, splitCols = "gta.evaluation", sep = ", ", direction = "long")

# Merge the prepared table and the aggregated results
table2 <- merge(temp, table2, by = c("implementing.jurisdiction", "affected.jurisdiction", "gta.evaluation"), all.x = T); rm(temp)
table2[is.na(table2)] <- 0


### Save data
save(table1, table2, file = paste0(gta26.path, data.path, "heat maps.Rdata"))
