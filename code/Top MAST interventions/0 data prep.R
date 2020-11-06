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

# 4lag. For the top 5 categories of implemented harmful policies identified in 1. above, as well as for all harmful policy intervention, please calculate the share
# of world trade covered by these 6 categories for each year since 2015, confining the calculations to those policies implemented worldwide between 1 January and
# 21 October of the year in question. Do not use duration adjusted calculations. For each category of policy intervention and for all harmful policy intervention,
# please report each year’s findings next to one another in bar charts. Please organise the 6 sets of bar charts on to one page, ideally in landscape format
# (using one or two rows of bar charts—you decide what looks best). Please prepare the charts using 2015 year weights (for international trade data) and for 2019
# year weights—and compare them. If the results are very similar, then please send me the chart using the 2015 year weights.
#
# 5lag. Please repeat 4 but for the implemented liberalising interventions.

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

any(!unique(master.sliced$mast.chapter[master.sliced$gta.evaluation == "harmful"]) %in% c(as.character(top5.mast.harmful), "Other")) ## Should be FALSE
any(!unique(master.sliced$mast.chapter[master.sliced$gta.evaluation == "liberalising"]) %in% c(as.character(top5.mast.liberalising), "Other")) ## Should be FALSE

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
gta_data_slicer(intervention.types = c("Anti-dumping", "Anti-subsidy", "Safeguard"),
                keep.type = T)

# Keep only announcement dates between 1 Jan and cutoff date
table3 <- subset(master.sliced, format(date.announced, "%m-%d") <= format(as.Date(cutoff.date), "%m-%d"))

# # Aggregate "Safeguard" and "Special safeguard" into one type
# table3$intervention.type <- ifelse(table3$intervention.type %in% c("Safeguard", "Special safeguard"), "Safeguard", table3$intervention.type)

# Get rid of interventions announced before 2009
table3 <- subset(table3, year(date.announced) >= 2009)

# Aggregate for year implemented and intervention type
table3 <- select(aggregate(intervention.id ~ year(date.announced) + intervention.type, table3, function(x){length(unique(x))}), "year.announced" = "year(date.announced)", intervention.type, "nr.of.interventions" = intervention.id)


### Table 4
# Get data
## NOTE: using this computationally expensive method instead of lag-adjustment to not conflict with SE: "Do not use duration adjusted calculations."
mast.groups <- as.character(top5.mast.harmful[1:5])
odd.chapters=c("FDI","MIG","CAP","P")

table4 <- data.frame()

for(yr in 2015:2020){
  print(yr)
  # by MAST
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     affected.flows = "inward",
                     implementation.period = c(as.Date(paste0(yr,"-01-01")), as.Date(paste0(yr,"-", format(as.Date(cutoff.date), "%m-%d")))),
                     coverage.period = c(yr, yr),
                     mast.chapters = mast.groups[! mast.groups %in% odd.chapters],
                     keep.mast = T,
                     group.mast = F,
                     intra.year.duration = F)
  
  if("trade.coverage.estimates" %in% ls()){
    
    trade.coverage.estimates$year=yr
    table4 <- rbind(table4, subset(select(trade.coverage.estimates, `MAST chapter ID`, `MAST chapter name`,year ,"trade.share"=names(trade.coverage.estimates)[grepl("Trade coverage",names(trade.coverage.estimates))]),
                                   `MAST chapter ID`!="All included MAST chapters"))
    
    
  } else{
   
    tce.replacement=data.frame(id=mast.groups[! mast.groups %in% odd.chapters],
                               name=NA,
                               year=yr,
                               trade.share=0,
                               stringsAsFactors = F)
    names(tce.replacement)=c("MAST chapter ID","MAST chapter name","year","trade.share")
    
    table4 <- rbind(table4,
                    tce.replacement)
    rm(tce.replacement)
  }
  
  if("P" %in% mast.groups){
    
    rm(trade.coverage.estimates)
    gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                       affected.flows = "outward",
                       implementer.role = "exporter",
                       implementation.period = c(as.Date(paste0(yr,"-01-01")), as.Date(paste0(yr,"-", format(as.Date(cutoff.date), "%m-%d")))),
                       coverage.period = c(yr, yr),
                       mast.chapters = "P",
                       keep.mast = T,
                       group.mast = T,
                       intra.year.duration = F)
    
  
 
    if("trade.coverage.estimates" %in% ls()){
      
      trade.coverage.estimates$`MAST chapter ID`="P - barriers"
      trade.coverage.estimates$`MAST chapter name`="P: Export barriers"
      trade.coverage.estimates$year=yr
      table4 <- rbind(table4, subset(select(trade.coverage.estimates, `MAST chapter ID`, `MAST chapter name`,year ,"trade.share"=names(trade.coverage.estimates)[grepl("Trade coverage",names(trade.coverage.estimates))]),
                                     `MAST chapter ID`!="All included MAST chapters"))
      
      
    } else{
      
      tce.replacement=data.frame(id="P - barriers",
                                 name="P: Export barriers",
                                 year=yr,
                                 trade.share=0,
                                 stringsAsFactors = F)
      names(tce.replacement)=c("MAST chapter ID","MAST chapter name","year","trade.share")
      
      table4 <- rbind(table4,
                      tce.replacement)
      rm(tce.replacement)
    }
    
    
    
    rm(trade.coverage.estimates)
    gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                       affected.flows = "outward subsidy",
                       implementation.period = c(as.Date(paste0(yr,"-01-01")), as.Date(paste0(yr,"-", format(as.Date(cutoff.date), "%m-%d")))),
                       coverage.period = c(yr, yr),
                       mast.chapters = "P",
                       keep.mast = T,
                       group.mast = T,
                       intra.year.duration = F)
    

    
    if("trade.coverage.estimates" %in% ls()){
      
      trade.coverage.estimates$`MAST chapter ID`="P - subsidy"
      trade.coverage.estimates$`MAST chapter name`="P: Export incentives"
      trade.coverage.estimates$year=yr
      table4 <- rbind(table4, subset(select(trade.coverage.estimates, `MAST chapter ID`, `MAST chapter name`,year ,"trade.share"=names(trade.coverage.estimates)[grepl("Trade coverage",names(trade.coverage.estimates))]),
                                     `MAST chapter ID`!="All included MAST chapters"))
      
      
      
    } else{
      
      tce.replacement=data.frame(id="P - subsidy",
                                 name="P: Export incentives",
                                 year=yr,
                                 trade.share=0,
                                 stringsAsFactors = F)
      names(tce.replacement)=c("MAST chapter ID","MAST chapter name","year","trade.share")
      
      table4 <- rbind(table4,
                      tce.replacement)
      rm(tce.replacement)
    }
    
  }
  
  # ALL
  rm(trade.coverage.estimates)
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     implementation.period = c(as.Date(paste0(yr,"-01-01")), as.Date(paste0(yr,"-", format(as.Date(cutoff.date), "%m-%d")))),
                     coverage.period = c(yr, yr),
                     intra.year.duration = F)
  trade.coverage.estimates$`MAST chapter ID`="ALL"
  trade.coverage.estimates$`MAST chapter name`="All harmful interventions"
  trade.coverage.estimates$year=yr
  table4 <- rbind(table4, select(trade.coverage.estimates, `MAST chapter ID`, `MAST chapter name`, year , "trade.share"=names(trade.coverage.estimates)[grepl("Trade coverage",names(trade.coverage.estimates))]))
  rm(trade.coverage.estimates)
}



### Table 5
# Get data
## NOTE: using this computationally expensive method instead of lag-adjustment to not conflict with SE: "Do not use duration adjusted calculations."
mast.groups <- as.character(top5.mast.liberalising[1:5])
odd.chapters=c("FDI","MIG","CAP","P")

table5=data.frame()

for(yr in 2015:2020){
  print(yr)
  # by MAST
  gta_trade_coverage(gta.evaluation = c("Green"),
                     affected.flows = "inward",
                     implementation.period = c(as.Date(paste0(yr,"-01-01")), as.Date(paste0(yr,"-", format(as.Date(cutoff.date), "%m-%d")))),
                     coverage.period = c(yr, yr),
                     mast.chapters = mast.groups[! mast.groups %in% odd.chapters],
                     keep.mast = T,
                     group.mast = F,
                     intra.year.duration = F)
  
  if("trade.coverage.estimates" %in% ls()){
    
    trade.coverage.estimates$year=yr
    table5 <- rbind(table5, subset(select(trade.coverage.estimates, `MAST chapter ID`, `MAST chapter name`,year ,"trade.share"=names(trade.coverage.estimates)[grepl("Trade coverage",names(trade.coverage.estimates))]),
                                   `MAST chapter ID`!="All included MAST chapters"))
    
    
  } else{
    
    tce.replacement=data.frame(id=mast.groups[! mast.groups %in% odd.chapters],
                               name=NA,
                               year=yr,
                               trade.share=0,
                               stringsAsFactors = F)
    names(tce.replacement)=c("MAST chapter ID","MAST chapter name","year","trade.share")
    
    table5 <- rbind(table5,
                    tce.replacement)
    rm(tce.replacement)
  }
  
  if("P" %in% mast.groups){
    
    rm(trade.coverage.estimates)
    gta_trade_coverage(gta.evaluation = c("Green"),
                       affected.flows = "outward",
                       implementer.role = "exporter",
                       implementation.period = c(as.Date(paste0(yr,"-01-01")), as.Date(paste0(yr,"-", format(as.Date(cutoff.date), "%m-%d")))),
                       coverage.period = c(yr, yr),
                       mast.chapters = "P",
                       keep.mast = T,
                       group.mast = T,
                       intra.year.duration = F)
    
    
    
    if("trade.coverage.estimates" %in% ls()){
      
      trade.coverage.estimates$`MAST chapter ID`="P - barriers"
      trade.coverage.estimates$`MAST chapter name`="P: Export barriers"
      trade.coverage.estimates$year=yr
      table5 <- rbind(table5, subset(select(trade.coverage.estimates, `MAST chapter ID`, `MAST chapter name`,year ,"trade.share"=names(trade.coverage.estimates)[grepl("Trade coverage",names(trade.coverage.estimates))]),
                                     `MAST chapter ID`!="All included MAST chapters"))
      
      
    } else{
      
      tce.replacement=data.frame(id="P - barriers",
                                 name="P: Export barriers",
                                 year=yr,
                                 trade.share=0,
                                 stringsAsFactors = F)
      names(tce.replacement)=c("MAST chapter ID","MAST chapter name","year","trade.share")
      
      table5 <- rbind(table5,
                      tce.replacement)
      rm(tce.replacement)
    }
    
    
    
    rm(trade.coverage.estimates)
    gta_trade_coverage(gta.evaluation = c("Green"),
                       affected.flows = "outward subsidy",
                       implementation.period = c(as.Date(paste0(yr,"-01-01")), as.Date(paste0(yr,"-", format(as.Date(cutoff.date), "%m-%d")))),
                       coverage.period = c(yr, yr),
                       mast.chapters = "P",
                       keep.mast = T,
                       group.mast = T,
                       intra.year.duration = F)
    
    
    
    if("trade.coverage.estimates" %in% ls()){
      
      trade.coverage.estimates$`MAST chapter ID`="P - subsidy"
      trade.coverage.estimates$`MAST chapter name`="P: Export incentives"
      trade.coverage.estimates$year=yr
      table5 <- rbind(table5, subset(select(trade.coverage.estimates, `MAST chapter ID`, `MAST chapter name`,year ,"trade.share"=names(trade.coverage.estimates)[grepl("Trade coverage",names(trade.coverage.estimates))]),
                                     `MAST chapter ID`!="All included MAST chapters"))
      
      
      
    } else{
      
      tce.replacement=data.frame(id="P - subsidy",
                                 name="P: Export incentives",
                                 year=yr,
                                 trade.share=0,
                                 stringsAsFactors = F)
      names(tce.replacement)=c("MAST chapter ID","MAST chapter name","year","trade.share")
      
      table5 <- rbind(table5,
                      tce.replacement)
      rm(tce.replacement)
      
    }
    
  }
  
  # ALL
  rm(trade.coverage.estimates)
  gta_trade_coverage(gta.evaluation = c("Green"),
                     implementation.period = c(as.Date(paste0(yr,"-01-01")), as.Date(paste0(yr,"-", format(as.Date(cutoff.date), "%m-%d")))),
                     coverage.period = c(yr, yr),
                     intra.year.duration = F)
  trade.coverage.estimates$`MAST chapter ID`="ALL"
  trade.coverage.estimates$`MAST chapter name`="All liberalising interventions"
  trade.coverage.estimates$year=yr
  table5 <- rbind(table5, select(trade.coverage.estimates, `MAST chapter ID`, `MAST chapter name`, year , "trade.share"=names(trade.coverage.estimates)[grepl("Trade coverage",names(trade.coverage.estimates))]))
  rm(trade.coverage.estimates)
}



### Table 4 LAG ADJUSTED
# Get data
## NOTE: using this computationally expensive method instead of lag-adjustment to not conflict with SE: "Do not use duration adjusted calculations."
mast.groups <- as.character(top5.mast.harmful[1:5])
odd.chapters=c("FDI","MIG","CAP","P")

table4lag <- data.frame()

for(yr in 2015:2020){
  print(yr)
  # by MAST
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     affected.flows = "inward",
                     implementation.period = c(as.Date(paste0(yr,"-01-01")), as.Date(paste0(yr,"-", format(as.Date(cutoff.date), "%m-%d")))),
                     coverage.period = c(yr, yr),
                     mast.chapters = mast.groups[! mast.groups %in% odd.chapters],
                     keep.mast = T,
                     group.mast = F,
                     intra.year.duration = F,
                     lag.adjustment = format(as.Date(cutoff.date), "%m-%d"))
  
  if("trade.coverage.estimates" %in% ls()){
    
    trade.coverage.estimates$year=yr
    table4lag <- rbind(table4lag, subset(select(trade.coverage.estimates, `MAST chapter ID`, `MAST chapter name`,year ,"trade.share"=names(trade.coverage.estimates)[grepl("Trade coverage",names(trade.coverage.estimates))]),
                                         `MAST chapter ID`!="All included MAST chapters"))
    
    
  } else{
    
    tce.replacement=data.frame(id=mast.groups[! mast.groups %in% odd.chapters],
                               name=NA,
                               year=yr,
                               trade.share=0,
                               stringsAsFactors = F)
    names(tce.replacement)=c("MAST chapter ID","MAST chapter name","year","trade.share")
    
    table4lag <- rbind(table4lag,
                       tce.replacement)
    rm(tce.replacement)
  }
  
  if("P" %in% mast.groups){
    
    rm(trade.coverage.estimates)
    gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                       affected.flows = "outward",
                       implementer.role = "exporter",
                       implementation.period = c(as.Date(paste0(yr,"-01-01")), as.Date(paste0(yr,"-", format(as.Date(cutoff.date), "%m-%d")))),
                       coverage.period = c(yr, yr),
                       mast.chapters = "P",
                       keep.mast = T,
                       group.mast = T,
                       intra.year.duration = F,
                       lag.adjustment = format(as.Date(cutoff.date), "%m-%d"))
    
    
    
    if("trade.coverage.estimates" %in% ls()){
      
      trade.coverage.estimates$`MAST chapter ID`="P - barriers"
      trade.coverage.estimates$`MAST chapter name`="P: Export barriers"
      trade.coverage.estimates$year=yr
      table4lag <- rbind(table4lag, subset(select(trade.coverage.estimates, `MAST chapter ID`, `MAST chapter name`,year ,"trade.share"=names(trade.coverage.estimates)[grepl("Trade coverage",names(trade.coverage.estimates))]),
                                           `MAST chapter ID`!="All included MAST chapters"))
      
      
    } else{
      
      tce.replacement=data.frame(id="P - barriers",
                                 name="P: Export barriers",
                                 year=yr,
                                 trade.share=0,
                                 stringsAsFactors = F)
      names(tce.replacement)=c("MAST chapter ID","MAST chapter name","year","trade.share")
      
      table4lag <- rbind(table4lag,
                         tce.replacement)
      rm(tce.replacement)
    }
    
    
    
    rm(trade.coverage.estimates)
    gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                       affected.flows = "outward subsidy",
                       implementation.period = c(as.Date(paste0(yr,"-01-01")), as.Date(paste0(yr,"-", format(as.Date(cutoff.date), "%m-%d")))),
                       coverage.period = c(yr, yr),
                       mast.chapters = "P",
                       keep.mast = T,
                       group.mast = T,
                       intra.year.duration = F,
                       lag.adjustment = format(as.Date(cutoff.date), "%m-%d"))
    
    
    
    if("trade.coverage.estimates" %in% ls()){
      
      trade.coverage.estimates$`MAST chapter ID`="P - subsidy"
      trade.coverage.estimates$`MAST chapter name`="P: Export incentives"
      trade.coverage.estimates$year=yr
      table4lag <- rbind(table4lag, subset(select(trade.coverage.estimates, `MAST chapter ID`, `MAST chapter name`,year ,"trade.share"=names(trade.coverage.estimates)[grepl("Trade coverage",names(trade.coverage.estimates))]),
                                           `MAST chapter ID`!="All included MAST chapters"))
      
      
      
    } else{
      
      tce.replacement=data.frame(id="P - subsidy",
                                 name="P: Export incentives",
                                 year=yr,
                                 trade.share=0,
                                 stringsAsFactors = F)
      names(tce.replacement)=c("MAST chapter ID","MAST chapter name","year","trade.share")
      
      table4lag <- rbind(table4lag,
                         tce.replacement)
      rm(tce.replacement)
    }
    
  }
  
  # ALL
  rm(trade.coverage.estimates)
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     implementation.period = c(as.Date(paste0(yr,"-01-01")), as.Date(paste0(yr,"-", format(as.Date(cutoff.date), "%m-%d")))),
                     coverage.period = c(yr, yr),
                     intra.year.duration = F,
                     lag.adjustment = format(as.Date(cutoff.date), "%m-%d"))
  trade.coverage.estimates$`MAST chapter ID`="ALL"
  trade.coverage.estimates$`MAST chapter name`="All harmful interventions"
  trade.coverage.estimates$year=yr
  table4lag <- rbind(table4lag, select(trade.coverage.estimates, `MAST chapter ID`, `MAST chapter name`, year , "trade.share"=names(trade.coverage.estimates)[grepl("Trade coverage",names(trade.coverage.estimates))]))
  rm(trade.coverage.estimates)
}



### Table 5 LAG ADJUSTED
# Get data
## NOTE: using this computationally expensive method instead of lag-adjustment to not conflict with SE: "Do not use duration adjusted calculations."
mast.groups <- as.character(top5.mast.liberalising[1:5])
odd.chapters=c("FDI","MIG","CAP","P")

table5lag=data.frame()

for(yr in 2015:2020){
  print(yr)
  # by MAST
  gta_trade_coverage(gta.evaluation = c("Green"),
                     affected.flows = "inward",
                     implementation.period = c(as.Date(paste0(yr,"-01-01")), as.Date(paste0(yr,"-", format(as.Date(cutoff.date), "%m-%d")))),
                     coverage.period = c(yr, yr),
                     mast.chapters = mast.groups[! mast.groups %in% odd.chapters],
                     keep.mast = T,
                     group.mast = F,
                     intra.year.duration = F,
                     lag.adjustment = format(as.Date(cutoff.date), "%m-%d"))
  
  if("trade.coverage.estimates" %in% ls()){
    
    trade.coverage.estimates$year=yr
    table5lag <- rbind(table5lag, subset(select(trade.coverage.estimates, `MAST chapter ID`, `MAST chapter name`,year ,"trade.share"=names(trade.coverage.estimates)[grepl("Trade coverage",names(trade.coverage.estimates))]),
                                         `MAST chapter ID`!="All included MAST chapters"))
    
    
  } else{
    
    tce.replacement=data.frame(id=mast.groups[! mast.groups %in% odd.chapters],
                               name=NA,
                               year=yr,
                               trade.share=0,
                               stringsAsFactors = F)
    names(tce.replacement)=c("MAST chapter ID","MAST chapter name","year","trade.share")
    
    table5lag <- rbind(table5lag,
                       tce.replacement)
    rm(tce.replacement)
  }
  
  if("P" %in% mast.groups){
    
    rm(trade.coverage.estimates)
    gta_trade_coverage(gta.evaluation = c("Green"),
                       affected.flows = "outward",
                       implementer.role = "exporter",
                       implementation.period = c(as.Date(paste0(yr,"-01-01")), as.Date(paste0(yr,"-", format(as.Date(cutoff.date), "%m-%d")))),
                       coverage.period = c(yr, yr),
                       mast.chapters = "P",
                       keep.mast = T,
                       group.mast = T,
                       intra.year.duration = F,
                       lag.adjustment = format(as.Date(cutoff.date), "%m-%d"))
    
    
    
    if("trade.coverage.estimates" %in% ls()){
      
      trade.coverage.estimates$`MAST chapter ID`="P - barriers"
      trade.coverage.estimates$`MAST chapter name`="P: Export barriers"
      trade.coverage.estimates$year=yr
      table5lag <- rbind(table5lag, subset(select(trade.coverage.estimates, `MAST chapter ID`, `MAST chapter name`,year ,"trade.share"=names(trade.coverage.estimates)[grepl("Trade coverage",names(trade.coverage.estimates))]),
                                           `MAST chapter ID`!="All included MAST chapters"))
      
      
    } else{
      
      tce.replacement=data.frame(id="P - barriers",
                                 name="P: Export barriers",
                                 year=yr,
                                 trade.share=0,
                                 stringsAsFactors = F)
      names(tce.replacement)=c("MAST chapter ID","MAST chapter name","year","trade.share")
      
      table5lag <- rbind(table5lag,
                         tce.replacement)
      rm(tce.replacement)
    }
    
    
    
    rm(trade.coverage.estimates)
    gta_trade_coverage(gta.evaluation = c("Green"),
                       affected.flows = "outward subsidy",
                       implementation.period = c(as.Date(paste0(yr,"-01-01")), as.Date(paste0(yr,"-", format(as.Date(cutoff.date), "%m-%d")))),
                       coverage.period = c(yr, yr),
                       mast.chapters = "P",
                       keep.mast = T,
                       group.mast = T,
                       intra.year.duration = F,
                       lag.adjustment = format(as.Date(cutoff.date), "%m-%d"))
    
    
    
    if("trade.coverage.estimates" %in% ls()){
      
      trade.coverage.estimates$`MAST chapter ID`="P - subsidy"
      trade.coverage.estimates$`MAST chapter name`="P: Export incentives"
      trade.coverage.estimates$year=yr
      table5lag <- rbind(table5lag, subset(select(trade.coverage.estimates, `MAST chapter ID`, `MAST chapter name`,year ,"trade.share"=names(trade.coverage.estimates)[grepl("Trade coverage",names(trade.coverage.estimates))]),
                                           `MAST chapter ID`!="All included MAST chapters"))
      
      
      
    } else{
      
      tce.replacement=data.frame(id="P - subsidy",
                                 name="P: Export incentives",
                                 year=yr,
                                 trade.share=0,
                                 stringsAsFactors = F)
      names(tce.replacement)=c("MAST chapter ID","MAST chapter name","year","trade.share")
      
      table5lag <- rbind(table5lag,
                         tce.replacement)
      rm(tce.replacement)
      
    }
    
  }
  
  # ALL
  rm(trade.coverage.estimates)
  gta_trade_coverage(gta.evaluation = c("Green"),
                     implementation.period = c(as.Date(paste0(yr,"-01-01")), as.Date(paste0(yr,"-", format(as.Date(cutoff.date), "%m-%d")))),
                     coverage.period = c(yr, yr),
                     intra.year.duration = F,
                     lag.adjustment = format(as.Date(cutoff.date), "%m-%d"))
  trade.coverage.estimates$`MAST chapter ID`="ALL"
  trade.coverage.estimates$`MAST chapter name`="All liberalising interventions"
  trade.coverage.estimates$year=yr
  table5lag <- rbind(table5lag, select(trade.coverage.estimates, `MAST chapter ID`, `MAST chapter name`, year , "trade.share"=names(trade.coverage.estimates)[grepl("Trade coverage",names(trade.coverage.estimates))]))
  rm(trade.coverage.estimates)
}





### Save data
save(table1, table3, table4, table5,table4lag, table5lag, file = paste0(gta26.path, data.path, "top MAST interventions.Rdata"))
