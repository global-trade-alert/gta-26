rm(list = ls())

### REQUEST:
# 1. For the years 2009 to 2020, please plot a line chart showing the % of implemented measures worldwide of any type affecting the
# (a) food sector and the
# (b) medical goods, medical equipment, and medicines sectors.
# Please use the HS codes for each sector that we used in the World Bank/EUI monitoring project. To put these lines in perspective, please identify the 3 CPC
# sectors where the most number of measures have been implemented since we began our monitoring. Then add these three lines as well to this chart. For (a) please
# use the following legend label “Food and agri-food”. For (b) please use the following legend label “COVID-19 medical kit and medicines”. Please add a note at the
# bottom stating “Source: Global Trade Alert.” Do not use reporting lag adjusted data to compile this line chart.
#
# 2. Recently, you prepared a figure showing the month by month phase in and phase out of trade reforms and export curbs in the medical goods sector worldwide.
# Please update that chart using the latest excel file prepared (last Friday) for the WB/EUI monitoring but extend the month by month reporting into 2021
# (similar to what you have done for the year 2020).
#
# 3. Please repeat the chart directly above but for the food sector

library(gtalibrary)
library(tidyverse)
library(openxlsx)
library(splitstackshape)

gta_setwd()
gta26.path = "0 report production/GTA 26/"
data.path = "data/Food & medicines phase in & phase out/"
source(paste0(gta26.path, "help files/GTA 26 cutoff and definitions.R"))


### Table 1
# Get data
gta_data_slicer(implementation.period = c(as.Date("2009-01-01"), as.Date("2020-12-31")),
                keep.implementation.na = F)

master.sliced$product.category <- "all"
data1 <- master.sliced

for (product.category in names(list("food" = hs.covid.food, "medical" = hs.covid.medical))){
  gta_data_slicer(implementation.period = c(as.Date("2009-01-01"), as.Date("2020-12-31")),
                  keep.implementation.na = F,
                  hs.codes = unlist(list("food" = hs.covid.food, "medical" = hs.covid.medical)[product.category]),
                  keep.hs = T)
  
  master.sliced$product.category <- product.category
  data1 <- rbind(data1, master.sliced)
}

# Add a column with the implementation year
data1$year.implemented <- str_extract(data1$date.implemented, "\\d{4}")

# Identify the top 3 affected CPC sectors
top3.sectors <- unique(cSplit(select(subset(data1, product.category == "all"), intervention.id, affected.sector), splitCols = "affected.sector", sep = ", ", direction = "long"))
top3.sectors <- as.character(arrange(as.data.frame(table(top3.sectors$affected.sector)), desc(Freq))[1:3,1])

## Assemble the table
# Food
table1 <- select(aggregate(intervention.id ~ year.implemented, subset(data1, product.category == "food"), function(x){length(unique(x))}), year.implemented, "food" = intervention.id)

# Medical goods
table1 <- merge(table1, select(aggregate(intervention.id ~ year.implemented, subset(data1, product.category == "medical"), function(x){length(unique(x))}), year.implemented, "medical" = intervention.id), by = "year.implemented", all.x = T)

# Top 1 sector
table1 <- merge(table1, select(aggregate(intervention.id ~ year.implemented, subset(data1, product.category == "all" & grepl(top3.sectors[1], affected.sector, fixed = T)), function(x){length(unique(x))}), year.implemented, "top.1.sector" = intervention.id), by = "year.implemented", all.x = T)

# Top 2 sector
table1 <- merge(table1, select(aggregate(intervention.id ~ year.implemented, subset(data1, product.category == "all" & grepl(top3.sectors[2], affected.sector, fixed = T)), function(x){length(unique(x))}), year.implemented, "top.2.sector" = intervention.id), by = "year.implemented", all.x = T)

# Top 3 sector
table1 <- merge(table1, select(aggregate(intervention.id ~ year.implemented, subset(data1, product.category == "all" & grepl(top3.sectors[3], affected.sector, fixed = T)), function(x){length(unique(x))}), year.implemented, "top.3.sector" = intervention.id), by = "year.implemented", all.x = T)


# Add a column with the total interventions in order to convert the rest to percentages
table1 <- merge(table1, select(aggregate(intervention.id ~ year.implemented, subset(data1, product.category == "all"), function(x){length(unique(x))}), year.implemented, "total.interventions" = intervention.id), by = "year.implemented", all.x = T)

# Convert to percentages
table1$food <- table1$food / table1$total.interventions
table1$medical <- table1$medical / table1$total.interventions
table1$top.1.sector <- table1$top.1.sector / table1$total.interventions
table1$top.2.sector <- table1$top.2.sector / table1$total.interventions
table1$top.3.sector <- table1$top.3.sector / table1$total.interventions

# Drop the column with the total nr of interventions
table1$total.interventions <- NULL

# Indicate which were the top 3 sectors in the column names
names(table1) <- c("year.implemented", "food", "medical", paste0("Top 1 sector (", top3.sectors[1], ")"), paste0("Top 2 sector (", top3.sectors[2], ")"), paste0("Top 3 sector (", top3.sectors[3], ")"))


### Tables 2 & 3
# Load the data
table2 <- read.xlsx(paste0(gta26.path, "help files/GTA COVID trade barrier data - 201016.xlsx"), sheet = 1, detectDates = T)

# Keep only import liberalisations or export curbs
table2 <- subset(table2, (`Initial.assessment.(change.relative.to.1.Jan.2020)`=="liberalising" & Is.import.policy==T) | (`Initial.assessment.(change.relative.to.1.Jan.2020)`=="restrictive" & Is.export.policy==T))

# Aggregate into months
data.table::setnames(table2, old = "Initial.assessment.(change.relative.to.1.Jan.2020)", new = "gta.evaluation")
table2.food <- subset(table2, `Product:.Food` == T)
table2.medical <- subset(table2, `Product:.Any.medical.product` == T)
table2.food <- data.frame("int.type" = c("harmful", "liberalising"),
                          "Jan 20" = c(length(unique(subset(table2.food, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2020-01-31") & (Removal.date >= as.Date("2020-01-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.food, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2020-01-31") & (Removal.date >= as.Date("2020-01-01") | is.na(Removal.date))))$Entry.ID)),
                          "Feb 20" = c(length(unique(subset(table2.food, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2020-02-29") & (Removal.date >= as.Date("2020-02-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.food, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2020-02-29") & (Removal.date >= as.Date("2020-02-01") | is.na(Removal.date))))$Entry.ID)),
                          "Mar 20" = c(length(unique(subset(table2.food, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2020-03-31") & (Removal.date >= as.Date("2020-03-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.food, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2020-03-31") & (Removal.date >= as.Date("2020-03-01") | is.na(Removal.date))))$Entry.ID)),
                          "Apr 20" = c(length(unique(subset(table2.food, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2020-04-30") & (Removal.date >= as.Date("2020-04-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.food, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2020-04-30") & (Removal.date >= as.Date("2020-04-01") | is.na(Removal.date))))$Entry.ID)),
                          "May 20" = c(length(unique(subset(table2.food, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2020-05-31") & (Removal.date >= as.Date("2020-05-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.food, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2020-05-31") & (Removal.date >= as.Date("2020-05-01") | is.na(Removal.date))))$Entry.ID)),
                          "Jun 20" = c(length(unique(subset(table2.food, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2020-06-30") & (Removal.date >= as.Date("2020-06-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.food, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2020-06-30") & (Removal.date >= as.Date("2020-06-01") | is.na(Removal.date))))$Entry.ID)),
                          "Jul 20" = c(length(unique(subset(table2.food, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2020-07-31") & (Removal.date >= as.Date("2020-07-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.food, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2020-07-31") & (Removal.date >= as.Date("2020-07-01") | is.na(Removal.date))))$Entry.ID)),
                          "Aug 20" = c(length(unique(subset(table2.food, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2020-08-31") & (Removal.date >= as.Date("2020-08-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.food, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2020-08-31") & (Removal.date >= as.Date("2020-08-01") | is.na(Removal.date))))$Entry.ID)),
                          "Sep 20" = c(length(unique(subset(table2.food, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2020-09-30") & (Removal.date >= as.Date("2020-09-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.food, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2020-09-30") & (Removal.date >= as.Date("2020-09-01") | is.na(Removal.date))))$Entry.ID)),
                          "Oct 20" = c(length(unique(subset(table2.food, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2020-10-31") & (Removal.date >= as.Date("2020-10-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.food, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2020-10-31") & (Removal.date >= as.Date("2020-10-01") | is.na(Removal.date))))$Entry.ID)),
                          "Nov 20" = c(length(unique(subset(table2.food, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2020-11-30") & (Removal.date >= as.Date("2020-11-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.food, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2020-11-30") & (Removal.date >= as.Date("2020-11-01") | is.na(Removal.date))))$Entry.ID)),
                          "Dec 20" = c(length(unique(subset(table2.food, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2020-12-31") & (Removal.date >= as.Date("2020-12-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.food, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2020-12-31") & (Removal.date >= as.Date("2020-12-01") | is.na(Removal.date))))$Entry.ID)),
                          "Jan 21" = c(length(unique(subset(table2.food, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2021-01-31") & (Removal.date >= as.Date("2021-01-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.food, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2021-01-31") & (Removal.date >= as.Date("2021-01-01") | is.na(Removal.date))))$Entry.ID)),
                          "Feb 21" = c(length(unique(subset(table2.food, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2021-02-28") & (Removal.date >= as.Date("2021-02-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.food, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2021-02-28") & (Removal.date >= as.Date("2021-02-01") | is.na(Removal.date))))$Entry.ID)),
                          "Mar 21" = c(length(unique(subset(table2.food, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2021-03-31") & (Removal.date >= as.Date("2021-03-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.food, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2021-03-31") & (Removal.date >= as.Date("2021-03-01") | is.na(Removal.date))))$Entry.ID)),
                          "Apr 21" = c(length(unique(subset(table2.food, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2021-04-30") & (Removal.date >= as.Date("2021-04-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.food, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2021-04-30") & (Removal.date >= as.Date("2021-04-01") | is.na(Removal.date))))$Entry.ID)),
                          "May 21" = c(length(unique(subset(table2.food, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2021-05-31") & (Removal.date >= as.Date("2021-05-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.food, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2021-05-31") & (Removal.date >= as.Date("2021-05-01") | is.na(Removal.date))))$Entry.ID)),
                          "Jun 21" = c(length(unique(subset(table2.food, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2021-06-30") & (Removal.date >= as.Date("2021-06-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.food, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2021-06-30") & (Removal.date >= as.Date("2021-06-01") | is.na(Removal.date))))$Entry.ID)),
                          "Jul 21" = c(length(unique(subset(table2.food, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2021-07-31") & (Removal.date >= as.Date("2021-07-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.food, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2021-07-31") & (Removal.date >= as.Date("2021-07-01") | is.na(Removal.date))))$Entry.ID)),
                          "Aug 21" = c(length(unique(subset(table2.food, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2021-08-31") & (Removal.date >= as.Date("2021-08-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.food, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2021-08-31") & (Removal.date >= as.Date("2021-08-01") | is.na(Removal.date))))$Entry.ID)),
                          "Sep 21" = c(length(unique(subset(table2.food, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2021-09-30") & (Removal.date >= as.Date("2021-09-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.food, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2021-09-30") & (Removal.date >= as.Date("2021-09-01") | is.na(Removal.date))))$Entry.ID)),
                          "Oct 21" = c(length(unique(subset(table2.food, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2021-10-31") & (Removal.date >= as.Date("2021-10-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.food, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2021-10-31") & (Removal.date >= as.Date("2021-10-01") | is.na(Removal.date))))$Entry.ID)),
                          "Nov 21" = c(length(unique(subset(table2.food, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2021-11-30") & (Removal.date >= as.Date("2021-11-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.food, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2021-11-30") & (Removal.date >= as.Date("2021-11-01") | is.na(Removal.date))))$Entry.ID)),
                          "Dec 21" = c(length(unique(subset(table2.food, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2021-12-31") & (Removal.date >= as.Date("2021-12-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.food, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2021-12-31") & (Removal.date >= as.Date("2021-12-01") | is.na(Removal.date))))$Entry.ID))
)

names(table2.food) <- c("int.type", "Jan 20", "Feb 20", "Mar 20", "Apr 20", "May 20", "Jun 20", "Jul 20", "Aug 20", "Sep 20", "Oct 20", "Nov 20", "Dec 20",
                        "Jan 21", "Feb 21", "Mar 21", "Apr 21", "May 21", "Jun 21", "Jul 21", "Aug 21", "Sep 21", "Oct 21", "Nov 21", "Dec 21")

# Do the same for medical products
table2.medical <- data.frame("int.type" = c("harmful", "liberalising"),
                          "Jan 20" = c(length(unique(subset(table2.medical, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2020-01-31") & (Removal.date >= as.Date("2020-01-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.medical, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2020-01-31") & (Removal.date >= as.Date("2020-01-01") | is.na(Removal.date))))$Entry.ID)),
                          "Feb 20" = c(length(unique(subset(table2.medical, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2020-02-29") & (Removal.date >= as.Date("2020-02-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.medical, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2020-02-29") & (Removal.date >= as.Date("2020-02-01") | is.na(Removal.date))))$Entry.ID)),
                          "Mar 20" = c(length(unique(subset(table2.medical, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2020-03-31") & (Removal.date >= as.Date("2020-03-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.medical, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2020-03-31") & (Removal.date >= as.Date("2020-03-01") | is.na(Removal.date))))$Entry.ID)),
                          "Apr 20" = c(length(unique(subset(table2.medical, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2020-04-30") & (Removal.date >= as.Date("2020-04-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.medical, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2020-04-30") & (Removal.date >= as.Date("2020-04-01") | is.na(Removal.date))))$Entry.ID)),
                          "May 20" = c(length(unique(subset(table2.medical, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2020-05-31") & (Removal.date >= as.Date("2020-05-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.medical, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2020-05-31") & (Removal.date >= as.Date("2020-05-01") | is.na(Removal.date))))$Entry.ID)),
                          "Jun 20" = c(length(unique(subset(table2.medical, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2020-06-30") & (Removal.date >= as.Date("2020-06-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.medical, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2020-06-30") & (Removal.date >= as.Date("2020-06-01") | is.na(Removal.date))))$Entry.ID)),
                          "Jul 20" = c(length(unique(subset(table2.medical, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2020-07-31") & (Removal.date >= as.Date("2020-07-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.medical, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2020-07-31") & (Removal.date >= as.Date("2020-07-01") | is.na(Removal.date))))$Entry.ID)),
                          "Aug 20" = c(length(unique(subset(table2.medical, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2020-08-31") & (Removal.date >= as.Date("2020-08-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.medical, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2020-08-31") & (Removal.date >= as.Date("2020-08-01") | is.na(Removal.date))))$Entry.ID)),
                          "Sep 20" = c(length(unique(subset(table2.medical, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2020-09-30") & (Removal.date >= as.Date("2020-09-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.medical, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2020-09-30") & (Removal.date >= as.Date("2020-09-01") | is.na(Removal.date))))$Entry.ID)),
                          "Oct 20" = c(length(unique(subset(table2.medical, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2020-10-31") & (Removal.date >= as.Date("2020-10-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.medical, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2020-10-31") & (Removal.date >= as.Date("2020-10-01") | is.na(Removal.date))))$Entry.ID)),
                          "Nov 20" = c(length(unique(subset(table2.medical, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2020-11-30") & (Removal.date >= as.Date("2020-11-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.medical, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2020-11-30") & (Removal.date >= as.Date("2020-11-01") | is.na(Removal.date))))$Entry.ID)),
                          "Dec 20" = c(length(unique(subset(table2.medical, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2020-12-31") & (Removal.date >= as.Date("2020-12-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.medical, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2020-12-31") & (Removal.date >= as.Date("2020-12-01") | is.na(Removal.date))))$Entry.ID)),
                          "Jan 21" = c(length(unique(subset(table2.medical, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2021-01-31") & (Removal.date >= as.Date("2021-01-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.medical, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2021-01-31") & (Removal.date >= as.Date("2021-01-01") | is.na(Removal.date))))$Entry.ID)),
                          "Feb 21" = c(length(unique(subset(table2.medical, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2021-02-28") & (Removal.date >= as.Date("2021-02-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.medical, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2021-02-28") & (Removal.date >= as.Date("2021-02-01") | is.na(Removal.date))))$Entry.ID)),
                          "Mar 21" = c(length(unique(subset(table2.medical, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2021-03-31") & (Removal.date >= as.Date("2021-03-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.medical, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2021-03-31") & (Removal.date >= as.Date("2021-03-01") | is.na(Removal.date))))$Entry.ID)),
                          "Apr 21" = c(length(unique(subset(table2.medical, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2021-04-30") & (Removal.date >= as.Date("2021-04-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.medical, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2021-04-30") & (Removal.date >= as.Date("2021-04-01") | is.na(Removal.date))))$Entry.ID)),
                          "May 21" = c(length(unique(subset(table2.medical, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2021-05-31") & (Removal.date >= as.Date("2021-05-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.medical, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2021-05-31") & (Removal.date >= as.Date("2021-05-01") | is.na(Removal.date))))$Entry.ID)),
                          "Jun 21" = c(length(unique(subset(table2.medical, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2021-06-30") & (Removal.date >= as.Date("2021-06-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.medical, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2021-06-30") & (Removal.date >= as.Date("2021-06-01") | is.na(Removal.date))))$Entry.ID)),
                          "Jul 21" = c(length(unique(subset(table2.medical, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2021-07-31") & (Removal.date >= as.Date("2021-07-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.medical, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2021-07-31") & (Removal.date >= as.Date("2021-07-01") | is.na(Removal.date))))$Entry.ID)),
                          "Aug 21" = c(length(unique(subset(table2.medical, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2021-08-31") & (Removal.date >= as.Date("2021-08-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.medical, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2021-08-31") & (Removal.date >= as.Date("2021-08-01") | is.na(Removal.date))))$Entry.ID)),
                          "Sep 21" = c(length(unique(subset(table2.medical, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2021-09-30") & (Removal.date >= as.Date("2021-09-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.medical, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2021-09-30") & (Removal.date >= as.Date("2021-09-01") | is.na(Removal.date))))$Entry.ID)),
                          "Oct 21" = c(length(unique(subset(table2.medical, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2021-10-31") & (Removal.date >= as.Date("2021-10-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.medical, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2021-10-31") & (Removal.date >= as.Date("2021-10-01") | is.na(Removal.date))))$Entry.ID)),
                          "Nov 21" = c(length(unique(subset(table2.medical, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2021-11-30") & (Removal.date >= as.Date("2021-11-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.medical, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2021-11-30") & (Removal.date >= as.Date("2021-11-01") | is.na(Removal.date))))$Entry.ID)),
                          "Dec 21" = c(length(unique(subset(table2.medical, gta.evaluation == "restrictive" & Implementation.date <= as.Date("2021-12-31") & (Removal.date >= as.Date("2021-12-01") | is.na(Removal.date)))$Entry.ID)),
                                       length(unique(subset(table2.medical, gta.evaluation == "liberalising" & Implementation.date <= as.Date("2021-12-31") & (Removal.date >= as.Date("2021-12-01") | is.na(Removal.date))))$Entry.ID))
)

names(table2.medical) <- c("int.type", "Jan 20", "Feb 20", "Mar 20", "Apr 20", "May 20", "Jun 20", "Jul 20", "Aug 20", "Sep 20", "Oct 20", "Nov 20", "Dec 20",
                           "Jan 21", "Feb 21", "Mar 21", "Apr 21", "May 21", "Jun 21", "Jul 21", "Aug 21", "Sep 21", "Oct 21", "Nov 21", "Dec 21")

# Change to longer format for plotting
table2.food <- pivot_longer(table2.food, cols = c("Jan 20", "Feb 20", "Mar 20", "Apr 20", "May 20", "Jun 20", "Jul 20", "Aug 20", "Sep 20", "Oct 20", "Nov 20", "Dec 20",
                                                  "Jan 21", "Feb 21", "Mar 21", "Apr 21", "May 21", "Jun 21", "Jul 21", "Aug 21", "Sep 21", "Oct 21", "Nov 21", "Dec 21"), names_to = "time", values_to = "nr.of.interventions")
table2.medical <- pivot_longer(table2.medical, cols = c("Jan 20", "Feb 20", "Mar 20", "Apr 20", "May 20", "Jun 20", "Jul 20", "Aug 20", "Sep 20", "Oct 20", "Nov 20", "Dec 20",
                                                        "Jan 21", "Feb 21", "Mar 21", "Apr 21", "May 21", "Jun 21", "Jul 21", "Aug 21", "Sep 21", "Oct 21", "Nov 21", "Dec 21"), names_to = "time", values_to = "nr.of.interventions")

# Join them together into one table
table2.food$product.type <- "food"
table2.medical$product.type <- "medical"
table2 <- rbind(table2.food, table2.medical); rm(table2.food, table2.medical)

### Save data
save(table1, table2, file = paste0(gta26.path, data.path, "food & medicines phase in & phase out.Rdata"))
