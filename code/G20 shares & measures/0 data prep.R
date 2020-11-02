rm(list = ls())

### REQUEST:
# 1. For the G20 nations, an XY plot of the share of measures implemented in 2009 that were red and harmful (X axis) against the share of measures implemented in 2020
# that are harmful (Y axis). On X axis please use title “Share of 2009 measures that created negative spillovers for trading partners.” On the Y axis please use the
# title “Share of 2020 measures that created negative spillovers for trading partners.”
#
# 2. Map of number of times each country was hit by red and amber measures implemented during 2009.
#
# 3. Map of number of times each country was hit by green measures implemented during 2009. If you can format this map and the map directly above into the same
# graphic (one vertically above the other), then great.
#
# 4. Map of share of nation’s exports that faced red and amber measures implemented during 2009.
#
# 5. Map of share of nation’s exports that faced green measures implemented during 2009. If you can format this map and the map directly above into the same
# graphic (one vertically above the other), then great.

library(gtalibrary)
library(tidyverse)

gta_setwd()
gta26.path = "0 report production/GTA 26/"
data.path = "data/G20 shares & measures/"
source(paste0(gta26.path, "help files/GTA 26 cutoff and definitions.R"))


### Table 1
table1 <- data.frame()
for (year in c("2009", "2020")){
  gta_data_slicer(implementing.country = country.names$un_code[country.names$is.g20 == T],
                  keep.implementer = T,
                  implementation.period = c(as.Date(paste0(year, "-01-01")), as.Date(paste0(year, "-12-31"))),
                  keep.implementation.na = F,
                  # lag.adjustment = str_remove(cutoff.date, "\\d{4}-") ## lag adjustment unnecessary? - asked Simon if we need this here.
  )
  
  table1 <- rbind(table1, master.sliced)
}; rm(master.sliced, parameter.choice.slicer)

# Create column with implementation year
table1$year.implemented <- sapply(table1$date.implemented, function(x){str_extract(x, "\\d{4}")})

# Summarize Red and Amber into harmful
table1$gta.evaluation <- ifelse(table1$gta.evaluation %in% c("Red", "Amber"), "harmful", "liberalising")

# Aggregate on year, implementer and evaluation
table1 <- aggregate(intervention.id ~ implementing.jurisdiction + year.implemented + gta.evaluation, table1, function(x){length(unique(x))})

# Create column of share of harmful measures
table1 <- merge(subset(table1, gta.evaluation == "harmful"), subset(table1, gta.evaluation == "liberalising"), by = c("implementing.jurisdiction", "year.implemented"), all = T)
table1[is.na(table1)] <- 0
table1$harmful.share <- table1$intervention.id.x / (table1$intervention.id.x + table1$intervention.id.y)

# Drop unnecessary columns
table1 <- select(table1, implementing.jurisdiction, year.implemented, harmful.share)


### Tables 2 and 3
gta_data_slicer(# implementing.country = country.names$un_code[country.names$is.g20 == T],
                # keep.implementer = T,
                implementation.period = c(as.Date("2009-01-01"), as.Date("2009-12-31")),
                keep.implementation.na = F) ## lag adjustment unnecessary? -YES

# Aggregate for affected country and evaluation
master.sliced$gta.evaluation <- ifelse(master.sliced$gta.evaluation %in% c("Red", "Amber"), "harmful", "liberalising")
table2 <- aggregate(intervention.id ~ a.un + gta.evaluation, master.sliced, function(x){length(unique(x))}); rm(master.sliced, parameter.choice.slicer)


### Tables 4 and 5
table3 <- data.frame()
for (evaluation in names(list("harmful" = c("Red", "Amber"), "liberalising" = "Green"))){
  gta_trade_coverage(coverage.period = c(2009, 2009),
                     gta.evaluation = unlist(list("harmful" = c("Red", "Amber"), "liberalising" = "Green")[evaluation]),
                     affected.flows = c("inward", "outward subsidy"),
                     # exporters = country.names$un_code[country.names$is.g20 == T],
                     # keep.exporters = T,
                     group.exporters = F,
                     implementation.period = c(as.Date("2009-01-01"), as.Date("2009-12-31"))) ## lag adjustment unnecessary? - YES
  
  trade.coverage.estimates$gta.evaluation <- evaluation
  table3 <- rbind(table3, trade.coverage.estimates)
}


### Save data
save(table1, table2, table3, file = paste0(gta26.path, data.path, "g20 shares & measures.Rdata"))
