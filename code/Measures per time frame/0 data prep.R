rm(list = ls())

### REQUEST:
# 1. Please prepare a stacked bar chart for each year 2009 with the number of measures recorded in the GTA database worldwide by 21 October 2009, from
# 22 October 2009 to 21 October 2010, from 22 October 2010 to 21 October 2015, from 22 October 2015 to 21 October 2020. On the same stacked bar chart
# please add another column for the measures implemented by the G20, again for the same time periods. Please add a note at the bottom stating
# “Source: Global Trade Alert.”
#
# 2. Please prepare a stacked column along the following lines. This chart only relates to measures implemented during 2009. Define the set of transparent
# policy instruments as all import tariff changes, all trade defence changes, all import quotas, all export quotas, all export taxes, all export licensing
# requirements, all import bans, all import quotas, and all export bans and all export quotas. Define subsidies to import competing firms as all measures
# in chapter L of the MAST classification. Define export incentives as all the measures in Chapter M except any export restriction or other export limit.
# Define other commercial policies as any policy intervention not in these three above categories. Please plot from 2009 to 2020 the 100% stacked columns
# showing the information reported in GTA database of these 4 classes of policy intervention that have been documented up to 31 December of each year
# (21 October for the year 2020). I have identified in bold text the legend labels to be used. Please add a note at the bottom stating “Source: Global Trade Alert.”

library(gtalibrary)
library(tidyverse)
library(openxlsx)
library(splitstackshape)

gta_setwd()
gta26.path = "0 report production/GTA 26/"
data.path = "data/Measures per time frame/"
source(paste0(gta26.path, "help files/GTA 26 cutoff and definitions.R"))


### Table 1
# Load data
gta_data_slicer(keep.implementation.na = F)

# Create an auxiliary variable to keep track of the submission time frames
master.sliced$period <- ifelse(master.sliced$date.published <= as.Date("2009-10-21"), 1, 0)
master.sliced$period[master.sliced$date.published > as.Date("2009-10-21") & master.sliced$date.published <= as.Date("2010-10-21")] <- 2
master.sliced$period[master.sliced$date.published > as.Date("2010-10-21") & master.sliced$date.published <= as.Date("2015-10-21")] <- 3
master.sliced$period[master.sliced$date.published > as.Date("2015-10-21") & master.sliced$date.published <= as.Date("2020-10-21")] <- 4

master.sliced <- subset(master.sliced, period != 0)

# Assemble into worldwide and G20
table1 <- data.frame("period" = c(1:4))
table1 <- merge(table1, select(aggregate(intervention.id ~ period, master.sliced, function(x){length(unique(x))}), period, "worldwide" = intervention.id))
table1 <- merge(table1, select(aggregate(intervention.id ~ period, subset(master.sliced, i.atleastone.G20 == 1), function(x){length(unique(x))}), period, "G20" = intervention.id))

# Make periods more readable
table1$period <- c("before 21 Oct 2009", "22 Oct 2009 to 21 Oct 2010", "22 Oct 2010 to 21 Oct 2015", "22 Oct 2015 to 21 Oct 2020")


### Table 2
# Load data
gta_data_slicer(keep.implementation.na = F,
                implementation.period = c(as.Date("2009-01-01"), as.Date("2009-12-31"))) ## Did SE really mean only implementation in 2009?

# Disregard submissions after 2020-10-21
master.sliced <- subset(master.sliced, date.published <= as.Date("2020-10-21"))

# Add column for submission year
master.sliced$year.submitted <- as.integer(str_extract(master.sliced$date.published, "\\d{4}"))

# Add column for SE intervention categories
master.sliced$SE.int.type <- ifelse(master.sliced$intervention.type %in% transparent.policy.instruments, "transparent policy instruments", NA)
master.sliced$SE.int.type[master.sliced$intervention.type %in% subsidies.to.imp.competing.firms] <- "subsidies to import competing firms"
master.sliced$SE.int.type[master.sliced$intervention.type %in% export.incentives] <- "export incentives"
master.sliced$SE.int.type[master.sliced$intervention.type %in% other.interventions] <- "other commercial policies"

any(is.na(master.sliced$SE.int.type)) ## Should be FALSE

# Aggregate per SE intervention type and submission year and make sure to include every category every year, even if there are 0 occurences
table2 <- data.frame("SE.int.type" = unique(master.sliced$SE.int.type),
                     "year.submitted" = "2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020")
table2 <- cSplit(table2, splitCols = "year.submitted", sep = ", ", direction = "long")
table2$year.submitted <- as.integer(table2$year.submitted)
table2 <- merge(table2, aggregate(intervention.id ~ SE.int.type + year.submitted, master.sliced, function(x){length(unique(x))}), by = c("SE.int.type", "year.submitted"), all.x = T)
table2[is.na(table2)] <- 0

# Convert to percentage
table2 <- merge(table2, select(aggregate(intervention.id ~ year.submitted, table2, sum), year.submitted, "total.per.year" = intervention.id), by = "year.submitted", all.x = T)
table2$percentage.per.year <- table2$intervention.id / table2$total.per.year

# Drop unnecessary columns
table2 <- select(table2, year.submitted, SE.int.type, percentage.per.year)

### Save data
save(table1, table2, file = paste0(gta26.path, data.path, "measures per time frame.Rdata"))