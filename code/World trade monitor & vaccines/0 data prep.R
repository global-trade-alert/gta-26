rm(list = ls())

### REQUEST:
# 1. Download the World Trade Monitor’s (https://www.cpb.nl/en/worldtrademonitor) latest monthly data (the series is available through to July 2020) and for the
# following nations/groups: World trade, USA (exports), Japan (exports), Euro zone (exports). Setting the value of each nation’s index at 100 at the maximum value
# available from January 2019 through to July 2020, please plot the month by month evolution over time from the start of 2019. Please use following title for Y axis
# “Volume of trade: For each series the index was set at 100 for the month with the maximum recorded volume since January 2019”.  For the X axis title please use “Month”.
#
# 2. Repeat the above for the above charts but for World trade, China (exports), Emerging Asia (excluding China) (exports), Eastern Europe/CIS region (exports),
# and Africa and Middle East (exports). For these two graphs please add a note at the bottom stating “Source: World Trade Monitor.”
#
# 3. Please make a R chart out the Excel file I will attach to the email with this file. The excel file relates to Chinese exports of masks to the United States.
#
# 4. Please confirm the 6 digit UN COMTRADE code for human (not animal) vaccines is 300220. For the years 2015 until the latest year COMTRADE is available, please
# identify the countries that are net importers of human vaccines. Please produce a map identifying those net importers and colour code the countries by the total
# imports per capita of population. For this map please add a note at the bottom stating “Source: UN COMTRADE database used to extract data on HS code 300200.”

library(gtalibrary)
library(tidyverse)
library(openxlsx)
library(splitstackshape)

gta_setwd()
gta26.path = "0 report production/GTA 26/"
data.path = "data/World trade monitor & vaccines/"
source(paste0(gta26.path, "help files/GTA 26 cutoff and definitions.R"))


### Table 1
# Load WTM data
wtm.data <- read.xlsx(paste0(gta26.path, "help files/cpb-data-wtm.xlsx"), sheet = 1, startRow = 4)

# Select data
table1 <- select(wtm.data, "region" = "25.September.2020.14:11:17", "2019m01", "2019m02", "2019m03", "2019m04", "2019m05", "2019m06", "2019m07", "2019m08",
                 "2019m09", "2019m10", "2019m11", "2019m12", "2020m01", "2020m02", "2020m03", "2020m04", "2020m05", "2020m06", "2020m07")[c(2,15:26),]
table1 <- pivot_longer(table1, cols = names(table1)[-1], names_to = "period", values_to = "value")
table1$region <- trimws(table1$region, "both")

# Keep only relevant rows
table1 <- subset(table1, region %in% c("World trade", "United States", "Japan", "Euro Area"))

# Set maximum value to index 100
table1 <- merge(table1, select(aggregate(value ~ region, table1, max), region, "max" = value), by = "region", all.x = T)
table1$index <- (table1$value / table1$max) * 100
table1 <- select(table1, -max)


### Table 2
# Select data
table2 <- select(wtm.data, "region" = "25.September.2020.14:11:17", "2019m01", "2019m02", "2019m03", "2019m04", "2019m05", "2019m06", "2019m07", "2019m08",
                 "2019m09", "2019m10", "2019m11", "2019m12", "2020m01", "2020m02", "2020m03", "2020m04", "2020m05", "2020m06", "2020m07")[c(2,15:26),]
table2 <- pivot_longer(table2, cols = names(table2)[-1], names_to = "period", values_to = "value")
table2$region <- trimws(table2$region, "both")

# Keep only relevant rows
table2 <- subset(table2, region %in% c("World trade", "China", "Emerging Asia (excl China)", "Eastern Europe / CIS", "Africa and Middle East"))

# Set maximum value to index 100
table2 <- merge(table2, select(aggregate(value ~ region, table2, max), region, "max" = value), by = "region", all.x = T)
table2$index <- (table2$value / table2$max) * 100
table2 <- select(table2, -max)


### Table 3
# Load SE data
table3 <- read.xlsx(paste0(gta26.path, "help files/Mask Imports USA.xlsx"), sheet = 3, detectDates = T)
names(table3) <- c("date", "China", "ROW")

# Fill dates back in (not detected by openxlsx)
table3$date <- c("Jan 18", "Feb 18", "Mar 18", "Apr 18", "May 18", "Jun 18", "Jul 18", "Aug 18", "Sep 18", "Oct 18", "Nov 18", "Dec 18",
                 "Jan 19", "Feb 19", "Mar 19", "Apr 19", "May 19", "Jun 19", "Jul 19", "Aug 19", "Sep 19", "Oct 19", "Nov 19", "Dec 19",
                 "Jan 20", "Feb 20", "Mar 20", "Apr 20", "May 20", "Jun 20", "Jul 20", "Aug 20")

table3 <- pivot_longer(table3, cols = c("China", "ROW"), names_to = "exporter", values_to = "trade.value")

### Table 4
# Get data (JF: use GTA data instead of COMTRADE)
data4 <- data.frame()
for (year in c(2015:2019)){
  gta_trade_value_bilateral(hs.codes = 300220,
                            keep.hs = T,
                            trade.data = year)
  
  trade.base.bilateral$year <- year
  data4 <- rbind(data4, trade.base.bilateral)
}; rm(trade.base.bilateral, parameter.choice.trade.base)

# Aggregate exports and imports for each country
table4 <- data.frame("country" = unique(c(data4$i.un, data4$a.un)),
                     "year" = "2015, 2016, 2017, 2018, 2019")
table4 <- cSplit(table4, splitCols = "year", sep = ", ", direction = "long")

# Add imports
table4 <- merge(table4, select(aggregate(trade.value ~ i.un + year, data4, sum), "country" = i.un, year, "import" = trade.value), by = c("country", "year"), all.x = T)

# Add exports
table4 <- merge(table4, select(aggregate(trade.value ~ a.un + year, data4, sum), "country" = a.un, year, "export" = trade.value), by = c("country", "year"), all.x = T)

# Replace NA with zero and add net imports
table4[is.na(table4)] <- 0
table4$net.import <- table4$import - table4$export

# Load WB population data
wb.population <- read.xlsx(paste0(gta26.path, "help files/WB population data.xlsx"), sheet = 1)
wb.population <- select(wb.population, -Series.Name, -Series.Code)
names(wb.population) <- c("name", "iso_code", as.character(2015:2020))
wb.population[wb.population == ".."] <- NA

# Convert to UN codes
wb.population <- merge(wb.population, select(country.names, un_code, iso_code), by = "iso_code", all.x = T)

# Pivot to join with prepared trade data
wb.population <- pivot_longer(wb.population, cols = c(as.character(2015:2020)), names_to = "year", values_to = "population")
wb.population$year <- as.numeric(wb.population$year)
wb.population$population <- as.numeric(wb.population$population)

# Merge trade and population data
table4 <- merge(table4, select(wb.population, name, un_code, population, year), by.x = c("country", "year"), by.y = c("un_code", "year"), all.x = T)

# Aggregate over all years as per SE: "Please use options B (Net importers when all the years are combined) and D (Total imports per capita summed up over the entire timeframe)"
table4 <- merge(table4, select(aggregate(net.import ~ country, table4, sum), country, "total.net.import" = net.import), by = "country", all.x = T)
table4 <- subset(table4, total.net.import > 0 & country != 568) ### ISO-3166-1 code 568 does not exist

# Sum net import per capita per country
table4$net.import.per.capita <- table4$net.import / table4$population
table4 <- aggregate(net.import.per.capita ~ country, table4, sum)

### Save data
save(table1, table2, table3, table4, file = paste0(gta26.path, data.path, "world trade monitor & vaccines.Rdata"))
