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
#
# Added 07.11.: I have been thinking the results that you've tabulated and produced charts for. What is striking this year is that the global averages are potentially
# misleading as there is a huge diversity in outcomes across nations. I am going to figure out how to explain that in the report and the following charts
# would help in this regard.
#
# 6. For the data in the figure 2 & 3 excel file, please produce two kernel distributions across nations of the number of times their commercial interests have been
# (a) hit or (b) benefited from foreign reforms. If possible, please put both kernels on the same chart.
# 
# 7. Please repeat the same request immediately above for the harmful and liberalising export exposure statistics in the attached file called figure 4 & 5. 
# 
# 8. Next, using the data in figure 4 & 5 please plot the % of exports facing harmful measures (on the Y axis) against the % of exports facing beneficial
# measures (on the X axis). Please make the size of the dot associated with each nation proportional to the logarithm of total goods exports of that nation in
# the last year for which we have trade data before the pandemic (either 2018 or 2019). 
# 
# 9. Please produce another version of the same plot (mentioned directly above) where the size of the dot associated with each nation is the logarithm of the per
# capita GDP.
# 
# 10. Please produce another version of the same plot (mentioned two paragraphs above) where the dot is red for a G20 member.
# 
# 11. Please produce another version of the same plot (mentioned three paragraphs above) where the dot is red for a Least Developed country. 

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


### Figure 6
#### Data already prepared (table2)


### Figure 7
#### Data already prepared (table3)


### Figure 8
# Get additional data needed
gta_trade_value_bilateral(cpc.sectors = cpc.names$cpc[cpc.names$cpc.digit.level == 3 & cpc.names$cpc < 500],
                          keep.cpc = T,
                          trade.data = 2019)

# Make sure that every country has values for both GTA evaluations and merge with GDP data
temp <- data.frame("exporter" = unique(table3$exporter),
                   "evaluation" = "harmful, liberalising")
temp <- cSplit(temp, "evaluation", ", ", "long")

temp <- merge(temp, select(country.names, "exporter" = name, un_code), by = "exporter", all.x = T)

table8 <- merge(temp, select(table3, exporter, evaluation, trade.coverage), by = c("exporter", "evaluation"), all.x = T); rm(temp)

table8 <- merge(table8, select(aggregate(trade.value ~ a.un, trade.base.bilateral, sum), "un_code" = a.un, "log.trade.value" = trade.value), by = "un_code", all.x = T)

# Convert trade value to base 10 logarithm
table8$log.trade.value <- log(table8$log.trade.value, 10)

# Replace NAs in trade coverage with zero
table8$trade.coverage[is.na(table8$trade.coverage)] <- 0


### Figure 9
# Load per capita GDP data (from Worldbank)
wb.gdp.per.capita <- read.xlsx(xlsxFile = paste0(gta26.path, "help files/WB GDP per capita.xlsx"), sheet = 1)

# Get the latest data for every country
names(wb.gdp.per.capita) <- c("series.name", "series.code", "country.name", "country.code", as.character(2000:2019))
wb.gdp.per.capita <- pivot_longer(wb.gdp.per.capita, cols = as.character(2000:2019), names_to = "year", values_to = "gdp.per.capita")
wb.gdp.per.capita <- subset(wb.gdp.per.capita, gdp.per.capita != "..")
wb.gdp.per.capita$year <- as.numeric(wb.gdp.per.capita$year)

wb.gdp.per.capita <- merge(wb.gdp.per.capita, select(aggregate(year ~ country.code, wb.gdp.per.capita, max), country.code, "latest.year" = year), by = "country.code", all.x = T)
wb.gdp.per.capita <- subset(wb.gdp.per.capita, year == latest.year)

# Add ISO-3166-1 codes
wb.gdp.per.capita <- merge(wb.gdp.per.capita, select(country.names, un_code, iso_code), by.x = "country.code", by.y = "iso_code", all.x = T)

# Remove countries that are not in the GTA library
wb.gdp.per.capita <- subset(wb.gdp.per.capita, !is.na(un_code))

# Make sure that every country has values for both GTA evaluations and merge with GDP data
temp <- data.frame("exporter" = unique(table3$exporter),
                   "evaluation" = "harmful, liberalising")
temp <- cSplit(temp, "evaluation", ", ", "long")

temp <- merge(temp, select(country.names, "exporter" = name, un_code), by = "exporter", all.x = T)

table9 <- merge(temp, select(table3, exporter, evaluation, trade.coverage), by = c("exporter", "evaluation"), all.x = T); rm(temp)

# Merge with table 3 data
table9 <- merge(table9, select(wb.gdp.per.capita, "log.gdp.per.capita" = gdp.per.capita, un_code), by = "un_code", all.x = T)

# Print names of countries without GDP data
unique(table9$exporter[is.na(table9$log.gdp.per.capita)])

# Set NA values to zero, convert GDP per capita to numeric
table9[is.na(table9)] <- 0
table9$log.gdp.per.capita <- as.numeric(table9$log.gdp.per.capita)

# Convert GDP per capita to log10(GDP per capita)
table9$log.gdp.per.capita <- log(table9$log.gdp.per.capita, 10)


### Figure 10
# Merge table8 with is.g20 for plotting convenience
table10 <- merge(table8, select(country.names, un_code, is.g20), by = "un_code", all.x = T)
table10$is.g20 <- ifelse(table10$is.g20, "1", "0")


### Figure 11
# Merge table8 with is.ldc for plotting convenience
table11 <- merge(table8, select(country.names, un_code, is.ldc), by = "un_code", all.x = T)
table11$is.ldc <- ifelse(table11$is.ldc, "1", "0")


### Save data
save(table1, table2, table3, table8, table9, table10, table11, file = paste0(gta26.path, data.path, "harmful intervention statistics & maps.Rdata"))
