rm(list = ls())

### REQUEST:
# 1. Including import tariff changes as a category, identify the top 5 most frequently implemented harmful MAST chapter policy interventions used from 2015-2019
# worldwide. Put all other policies into an “other” category. Using a 100% stacked chart, please plot for each year 2015 to 2020 the distribution of the 100%
# across the top 5 most used policy categories plus the other category. Please construct the charts so that the other category is at the top of each 100% stacked bar.
#
# 2. Please repeat 1 above for the implemented liberalising policy interventions.
#
# 3. For the years 2009 to 2020, plot a stacked chart of the number of anti-dumping, anti-subsidy, and safeguard investigations that were launched worldwide
# from 1 January to 21 October of each year, irrespective of the outcome of the investigation (if there is an outcome by 21 October). Please use the following
# legend labels: New dumping inquiries, New subsidy inquiries, and New safeguard inquiries.
#
# 4. For the top 5 categories of implemented harmful policies identified in 1. above, as well as for all harmful policy intervention, please calculate the share
# of world trade covered by these 6 categories for each year since 2015, confining the calculations to those policies implemented worldwide between 1 January and
# 21 October of the year in question. Do not use duration adjusted calculations. For each category of policy intervention and for all harmful policy intervention,
# please report each year’s findings next to one another in bar charts. Please organise the 6 sets of bar charts on to one page, ideally in landscape format
# (using one or two rows of bar charts—you decide what looks best). Please prepare the charts using 2015 year weights (for international trade data) and for 2019
# year weights—and compare them. If the results are very similar, then please send me the chart using the 2015 year weights.
#
# 5. Please repeat 4 but for the implemented liberalising interventions.

library(gtalibrary)
library(tidyverse)
library(openxlsx)
library(splitstackshape)

gta_setwd()
gta26.path = "0 report production/GTA 26/"
data.path = "data/Top MAST interventions/"
source(paste0(gta26.path, "help files/GTA 26 cutoff and definitions.R"))


### Tables 1 & 2
# Get data
gta_data_slicer(implementation.period = c(as.Date("2015-01-01"), as.Date(cutoff.date)),
                keep.implementation.na = F)

# Convert GTA evaluation into two categories
master.sliced$gta.evaluation <- ifelse(master.sliced$gta.evaluation %in% c("Red", "Amber"), "harmful", "liberalising")

# Identify the 5 most used MAST chapters from 2015 to 2019
top5.mast.harmful <- arrange(aggregate(intervention.id ~ mast.chapter, subset(master.sliced, year(date.implemented) <= 2019 & gta.evaluation == "harmful"), function(x){length(unique(x))}), desc(intervention.id))$mast.chapter[1:5]
top5.mast.liberalising <- arrange(aggregate(intervention.id ~ mast.chapter, subset(master.sliced, year(date.implemented) <= 2019 & gta.evaluation == "liberalising"), function(x){length(unique(x))}), desc(intervention.id))$mast.chapter[1:5]

# Aggregate all other chapters into one
master.sliced$mast.chapter=as.character(master.sliced$mast.chapter)
master.sliced$mast.chapter[master.sliced$gta.evaluation == "harmful"] <- ifelse(master.sliced$mast.chapter[master.sliced$gta.evaluation == "harmful"] %in% top5.mast.harmful, master.sliced$mast.chapter[master.sliced$gta.evaluation == "harmful"], "Other")
master.sliced$mast.chapter[master.sliced$gta.evaluation == "liberalising"] <- ifelse(master.sliced$mast.chapter[master.sliced$gta.evaluation == "liberalising"] %in% top5.mast.liberalising, master.sliced$mast.chapter[master.sliced$gta.evaluation == "liberalising"], "Other")

any(!unique(master.sliced$mast.chapter[master.sliced$gta.evaluation == "harmful"]) %in% c(top5.mast.harmful, "Other")) ## Should be FALSE
any(!unique(master.sliced$mast.chapter[master.sliced$gta.evaluation == "liberalising"]) %in% c(top5.mast.liberalising, "Other")) ## Should be FALSE

# Aggregate per year
master.sliced$year.implemented <- as.integer(year(master.sliced$date.implemented))
table1 <- select(aggregate(intervention.id ~ year.implemented + mast.chapter + gta.evaluation, master.sliced, function(x){length(unique(x))}), year.implemented, gta.evaluation, mast.chapter, "nr.of.interventions" = intervention.id)

# Convert to percentage of respective year
table1 <- merge(table1, select(aggregate(nr.of.interventions ~ year.implemented + gta.evaluation, table1, sum), year.implemented, gta.evaluation, "total.nr.int" = nr.of.interventions), by = c("year.implemented", "gta.evaluation"), all.x = T)
table1$perc.of.interventions <- table1$nr.of.interventions / table1$total.nr.int
table1$total.nr.int <- NULL
table1$nr.of.interventions <- NULL


### Table 3
# Get data
gta_data_slicer(intervention.types = c("Anti-dumping", "Anti-subsidy", "Safeguard", "Special safeguard"),
                keep.type = T)

# Keep only implementation dates between 1 Jan and cutoff date
table2 <- subset(master.sliced, format(date.announced, "%m-%d") <= format(as.Date(cutoff.date), "%m-%d"))

# Aggregate "Safeguard" and "Special safeguard" into one type
table2$intervention.type <- ifelse(table2$intervention.type %in% c("Safeguard", "Special safeguard"), "Safeguard", table2$intervention.type)

# Get rid of interventions announced before 2009
table2 <- subset(table2, year(date.announced) >= 2009)

# Aggregate for year implemented and intervention type
table2 <- select(aggregate(intervention.id ~ year(date.announced) + intervention.type, table2, function(x){length(unique(x))}), "year.announced" = "year(date.announced)", intervention.type, "nr.of.interventions" = intervention.id)


### Table 4
# Get data
## NOTE: using this computationally expensive method instead of lag-adjustment to not conflict with SE: "Do not use duration adjusted calculations."
mast.groups <- list("Top 1" = top5.mast.harmful[1], "Top 2" = top5.mast.harmful[2], "Top 3" = top5.mast.harmful[3], "Top 4" = top5.mast.harmful[4],
                    "Top 5" = top5.mast.harmful[5], "Other" = as.character(unique(int.mast.types$mast.chapter.id)[unique(!int.mast.types$mast.chapter.id %in% top5.mast.harmful)]))

table3 <- data.frame()
for (mast in names(mast.groups)){
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     implementation.period = c(as.Date(paste0("2009-01-01")), as.Date(cutoff.date)),
                     coverage.period = c(2009, 2009),
                     mast.chapters = unlist(mast.groups[mast]),
                     keep.mast = T,
                     group.mast = T,
                     trade.data = 2019)
  
  trade.coverage.estimates$mast.group <- mast
  table3 <- rbind(table3, trade.coverage.estimates)
}

data3 <- data.frame()
for (year in c(2010:2020)){
  for (mast in names(mast.groups)){
    gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                       implementation.period = c(as.Date(paste0(year, "-01-01")), as.Date(paste0(year,"-", format(as.Date(cutoff.date), "%m-%d")))),
                       coverage.period = c(year, year),
                       mast.chapters = unlist(mast.groups[mast]),
                       keep.mast = T,
                       group.mast = T,
                       trade.data = 2019)
    
    trade.coverage.estimates$mast.group <- mast
    data3 <- rbind(data3, trade.coverage.estimates)
  }
  table3 <- merge(table3, select(data3, -"Importing country", -"Exporting country", -"Number of interventions affecting exported product"), by = c("mast.group"), all.x = T)
  data3 <- data.frame()
  print(paste0("Done with year: ", year))
}; rm(data3)

# Drop unnecessary columns
table3 <- select(table3, -"Importing country", -"Exporting country", -"Number of interventions affecting exported product")

# Add chapter names back in
table3$mast.group <- c("Other", paste0("Chapter ", top5.mast.harmful))


### Table 5
# Get data
## NOTE: using this computationally expensive method instead of lag-adjustment to not conflict with SE: "Do not use duration adjusted calculations."
mast.groups <- list("Top 1" = top5.mast.liberalising[1], "Top 2" = top5.mast.liberalising[2], "Top 3" = top5.mast.liberalising[3], "Top 4" = top5.mast.liberalising[4],
                    "Top 5" = top5.mast.liberalising[5], "Other" = as.character(unique(int.mast.types$mast.chapter.id)[unique(!int.mast.types$mast.chapter.id %in% top5.mast.liberalising)]))

table4 <- data.frame()
for (mast in names(mast.groups)){
  gta_trade_coverage(gta.evaluation = "Green",
                     implementation.period = c(as.Date(paste0("2009-01-01")), as.Date(cutoff.date)),
                     coverage.period = c(2009, 2009),
                     mast.chapters = unlist(mast.groups[mast]),
                     keep.mast = T,
                     group.mast = T,
                     trade.data = 2019)
  
  trade.coverage.estimates$mast.group <- mast
  table4 <- rbind(table4, trade.coverage.estimates)
}

data4 <- data.frame()
for (year in c(2010:2020)){
  for (mast in names(mast.groups)){
    gta_trade_coverage(gta.evaluation = "Green",
                       implementation.period = c(as.Date(paste0(year, "-01-01")), as.Date(paste0(year,"-", format(as.Date(cutoff.date), "%m-%d")))),
                       coverage.period = c(year, year),
                       mast.chapters = unlist(mast.groups[mast]),
                       keep.mast = T,
                       group.mast = T,
                       trade.data = 2019)
    
    trade.coverage.estimates$mast.group <- mast
    data4 <- rbind(data4, trade.coverage.estimates)
  }
  table4 <- merge(table4, select(data4, -"Importing country", -"Exporting country", -"Number of interventions affecting exported product"), by = c("mast.group"), all.x = T)
  data4 <- data.frame()
  print(paste0("Done with year: ", year))
}

# Drop unnecessary columns
table4 <- select(table4, -"Importing country", -"Exporting country", -"Number of interventions affecting exported product")

# Add chapter names back in
table4$mast.group <- c("Other", paste0("Chapter ", top5.mast.liberalising))

### Save data
save(table1, table2, table3, table4, file = paste0(gta26.path, data.path, "top MAST interventions.Rdata"))
