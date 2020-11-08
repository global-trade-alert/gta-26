rm(list = ls())

### REQUEST:
# 1. Please produce a line chart for the years 2009 to 2020 with the following variable on the primary vertical axis: Total number of commercial policy interventions
# worldwide documented by 21 October. On the second axis: please plot the average number of hours between policy intervention announcements (calculated as the ratio
# of 7080 divided by the total number of commercial policy interventions documented by 21 October). For the primary axis title use the text highlighted in bold. For
# the secondary axis title please use “Average number of hours between each commercial policy intervention.” Please add a note at the bottom stating
# “Source: Global Trade Alert.” ### SE 06.11.: Use announcement date
# SE 07.11.: Please update figure 1 in chapter 4 using the new cutoff date of 30 October.
#
# 2. Map of number of times each country was hit by red and amber measures implemented during 2020.
#
# 3. Map of number of times each country was hit by green measures implemented during 2020. If you can format this map and the map directly above into the same
# graphic (one vertically above the other), then great.
#
# 4. Map of share of nation’s exports that faced red and amber measures implemented during 2020.
#
# 5. Map of share of nation’s exports that faced green measures implemented during 2020. If you can format this map and the map directly above into the same
# graphic (one vertically above the other), then great.

library(gtalibrary)
library(tidyverse)
library(openxlsx)
library(splitstackshape)

gta_setwd()
gta26.path = "0 report production/GTA 26/"
data.path = "data/Harmful intervention statistics & maps/"
source(paste0(gta26.path, "help files/GTA 26 cutoff and definitions.R"))


### Table 1
# Get the data
gta_data_slicer(lag.adjustment = format(as.Date(cutoff.date), "%m-%d"))

# Add a column with submission year
master.sliced$year.announced <- year(master.sliced$date.announced)

# Aggregate per year
table1 <- select(aggregate(intervention.id ~ year.announced, master.sliced, function(x){length(unique(x))}), year.announced, "nr.of.interventions" = intervention.id)

# Add column for average hours between policy interventions
table1$avg.hours.between.int <- 7320 / table1$nr.of.interventions

# Drop years before 2009
table1 <- subset(table1, year.announced >= 2009)


### Tables 2 & 3
# Get data
gta_data_slicer(implementation.period = c(as.Date("2020-01-01"), as.Date(cutoff.date)),
                keep.implementation.na = F)

# Separate GTA evaluation into harmful and liberalising
master.sliced$gta.evaluation <- ifelse(master.sliced$gta.evaluation %in% c("Red", "Amber"), "harmful", "liberalising")

# Aggregate per affected country and evaluation and add country names for better readability
table2 <- merge(aggregate(intervention.id ~ a.un + gta.evaluation, master.sliced, function(x){length(unique(x))}), select(country.names, name, un_code), by.x = "a.un", by.y = "un_code", all.x = T)

# Rearrange columns
table2 <- select(table2, "affected.name" = name, "affected.un" = a.un, gta.evaluation, "nr.of.interventions" = intervention.id)


### Tables 4 & 5
# Get data
table3 <- data.frame()
for (evaluation in names(list("harmful" = c("Red", "Amber"), "liberalising" = "Green"))){
  gta_trade_coverage(gta.evaluation = unlist(list("harmful" = c("Red", "Amber"), "liberalising" = "Green")[evaluation]),
                     coverage.period = c(2020,2020),
                     implementation.period = c(as.Date("2020-01-01"), as.Date(cutoff.date)),
                     group.exporters = F)
  
  trade.coverage.estimates$evaluation <- evaluation
  table3 <- rbind(table3, trade.coverage.estimates)
}

# Select and rename necessary columns and add UN codes
table3 <- merge(select(table3, "exporter" = "Exporting country", "trade.coverage" = "Trade coverage estimate for 2020", evaluation), select(country.names, name, un_code), by.x = "exporter", by.y = "name", all.x = T)

### Save data
save(table1, table2, table3, file = paste0(gta26.path, data.path, "harmful intervention statistics & maps.Rdata"))
