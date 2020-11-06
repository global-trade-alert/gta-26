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
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(scales)
library(stringr)
library(openxlsx)
library(ggpubr)

gta_setwd()
gta26.path = "0 report production/GTA 26/"
data.path = "data/Top MAST interventions/"
out.path = "tables & figures/6 - Top MAST interventions/"
source(paste0(gta26.path, "help files/GTA 26 cutoff and definitions.R"))

gta_colour_palette()

# Load data
load(paste0(gta26.path, data.path, "top MAST interventions.Rdata"))

### Save Excel files of the data
write.xlsx(table1, file = paste0(gta26.path, out.path, "Figure 1 & 2 data.xlsx"))
write.xlsx(table3, file = paste0(gta26.path, out.path, "Figure 3  data.xlsx"))
write.xlsx(table4, file = paste0(gta26.path, out.path, "Figure 4 data.xlsx"))
write.xlsx(table5, file = paste0(gta26.path, out.path, "Figure 5 data.xlsx"))
write.xlsx(table4lag, file = paste0(gta26.path, out.path, "Figure 4 data - lag adjusted.xlsx"))
write.xlsx(table5lag, file = paste0(gta26.path, out.path, "Figure 5 data - lag adjusted.xlsx"))


### Functions
# Simple bar chart
bar.function <- function(data, fill.colour, x.lab = "", y.lab, y.breaks = seq(0,1,0.1), y.labels = label_percent(accuracy = 1L), y.limits = c(0,1)){
  plot <- ggplot(data = data)+
    geom_bar(aes(x=forcats::fct_inorder(as.character(year), ordered = T), y=trade.coverage), width=0.65, stat = "identity", fill=fill.colour) +
    labs(x=x.lab, y="")+
    scale_y_continuous(name=str_wrap(y.lab, 40), breaks=y.breaks, labels=y.labels, limits = y.limits,
                       sec.axis = sec_axis(trans = ~., name=str_wrap(y.lab, 40), breaks=y.breaks, labels=y.labels))+
    gta_theme(x.bottom.angle = 90, x.bottom.align = 1)+
    guides(guide_legend(title = NULL, label.hjust = 0, label.vjust = 0.5, title.position = "top", title.vjust = 0.5, nrow = 2))+
    theme(axis.text.x.bottom = element_text(hjust=0.5, size=10),
          axis.title.y.left = element_text(size=10),
          axis.title.y.right = element_text(size=10),
          legend.text = element_text(size=9),
          legend.title = element_blank(),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent", color="transparent"),
          legend.position = "bottom",
          legend.background = element_rect(fill="transparent", colour = "transparent"),
          legend.margin = margin(t=10,b=0,r=100,l=100),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size=.1, color=gta_colour$qualitative[1]),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank())
  
  return(plot)
}


### Figure 1
# Add MAST chapter descriptions
temp <- unique(select(int.mast.types, mast.chapter.id, mast.chapter.name))
table1 <- merge(table1, temp, by.x = "mast.chapter", by.y = "mast.chapter.id", all.x = T); rm(temp)
table1$mast.chapter.name[is.na(table1$mast.chapter.name)] <- "Other"
table1$mast.chapter <- table1$mast.chapter.name
table1$mast.chapter.name <- NULL

# Order the MAST chapters so that "Other" is plotted on top
table1$mast.chapter <- factor(table1$mast.chapter, levels = c("Other", unique(table1$mast.chapter)[str_detect(unique(table1$mast.chapter), "Other", T)]))

# Plot
fig1 <- ggplot(data = subset(table1, gta.evaluation == "harmful"))+
  geom_bar(aes(x=forcats::fct_inorder(as.character(year.implemented), ordered = T), y=perc.of.interventions, fill=mast.chapter), width=0.65, stat = "identity") +
  labs(x="", y="", caption = "Source: Global Trade Alert.")+
  scale_y_continuous(name=str_wrap("Percentage of measures recorded", 40), breaks=seq(0,1,0.1), labels=label_percent(accuracy = 1L),
                     sec.axis = sec_axis(trans = ~., name=str_wrap("Percentage of measures recorded", 40), breaks=seq(0,1,0.1), labels=label_percent(accuracy = 1L)))+
  scale_fill_manual(values=c(gta_colour$qualitative[6],gta_colour$qualitative[5],gta_colour$qualitative[4],gta_colour$qualitative[3],gta_colour$qualitative[2],
                             gta_colour$qualitative[1]), labels = levels(table1$mast.chapter)[levels(table1$mast.chapter) %in% unique(subset(table1, gta.evaluation == "harmful")$mast.chapter)])+
  gta_theme()+
  guides(guide_legend(title = NULL, label.hjust = 0, label.vjust = 0.5, title.position = "top", title.vjust = 0.5, nrow = 2))+
  theme(axis.text.x.bottom = element_text(hjust=0.5, size=10),
        axis.title.y.left = element_text(size=10),
        axis.title.y.right = element_text(size=10),
        legend.text = element_text(size=9),
        legend.title = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent", color="transparent"),
        legend.position = "bottom",
        legend.background = element_rect(fill="transparent", colour = "transparent"),
        legend.margin = margin(t=10,b=0,r=100,l=100),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=.1, color=gta_colour$qualitative[1]),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank())

fig1

gta_plot_saver(plot = fig1,
               path = paste0(gta26.path, out.path),
               name = "Figure 1 - Percentage of harmful measures per MAST chapter",
               png = T,
               pdf = T,
               jpg = T,
               eps = T,
               width = 25,
               height = 21)



### Figure 2
# Plot
fig2 <- ggplot(data = subset(table1, gta.evaluation == "liberalising"))+
  geom_bar(aes(x=forcats::fct_inorder(as.character(year.implemented), ordered = T), y=perc.of.interventions, fill=mast.chapter), width=0.65, stat = "identity") +
  labs(x="", y="", caption = "Source: Global Trade Alert.")+
  scale_y_continuous(name=str_wrap("Percentage of measures recorded", 40), breaks=seq(0,1,0.1), labels=label_percent(accuracy = 1L),
                     sec.axis = sec_axis(trans = ~., name=str_wrap("Percentage of measures recorded", 40), breaks=seq(0,1,0.1), labels=label_percent(accuracy = 1L)))+
  scale_fill_manual(values=c(gta_colour$qualitative[6],gta_colour$qualitative[5],gta_colour$qualitative[4],gta_colour$qualitative[3],gta_colour$qualitative[2],
                             gta_colour$qualitative[1]), labels = levels(table1$mast.chapter)[levels(table1$mast.chapter) %in% unique(subset(table1, gta.evaluation == "liberalising")$mast.chapter)])+
  gta_theme()+
  guides(guide_legend(title = NULL, label.hjust = 0, label.vjust = 0.5, title.position = "top", title.vjust = 0.5, nrow = 2))+
  theme(axis.text.x.bottom = element_text(hjust=0.5, size=10),
        axis.title.y.left = element_text(size=10),
        axis.title.y.right = element_text(size=10),
        legend.text = element_text(size=9),
        legend.title = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent", color="transparent"),
        legend.position = "bottom",
        legend.background = element_rect(fill="transparent", colour = "transparent"),
        legend.margin = margin(t=10,b=0,r=100,l=100),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=.1, color=gta_colour$qualitative[1]),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank())

fig2

gta_plot_saver(plot = fig2,
               path = paste0(gta26.path, out.path),
               name = "Figure 2 - Percentage of liberalising measures per MAST chapter",
               png = T,
               pdf = T,
               jpg = T,
               eps = T,
               width = 25,
               height = 21)



### Figure 3
# Plot
fig3 <- ggplot(data = table3)+
  geom_bar(aes(x=forcats::fct_inorder(as.character(year.announced), ordered = T), y=nr.of.interventions, fill=intervention.type), width=0.65, stat = "identity") +
  labs(x="", y="", caption = "Source: Global Trade Alert.")+
  scale_y_continuous(name=str_wrap("Number of measures recorded", 40), breaks=seq(0,300,25), labels=seq(0,300,25),
                     sec.axis = sec_axis(trans = ~., name=str_wrap("Number of measures recorded", 40), breaks=seq(0,300,25), labels=seq(0,300,25)))+
  scale_fill_manual(values=c("Safeguard" = gta_colour$qualitative[1], "Anti-subsidy" = gta_colour$qualitative[2],"Anti-dumping" = gta_colour$qualitative[4]), labels = c("New dumping inquiries", "New subsidy inquiries", "New safeguard inquiries"))+
  gta_theme(x.bottom.angle = 90, x.bottom.align = 1)+
  guides(guide_legend(title = NULL, label.hjust = 0, label.vjust = 0.5, title.position = "top", title.vjust = 0.5, nrow = 2))+
  theme(axis.text.x.bottom = element_text(hjust=0.5, size=10),
        axis.title.y.left = element_text(size=10),
        axis.title.y.right = element_text(size=10),
        legend.text = element_text(size=9),
        legend.title = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent", color="transparent"),
        legend.position = "bottom",
        legend.background = element_rect(fill="transparent", colour = "transparent"),
        legend.margin = margin(t=10,b=0,r=100,l=100),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=.1, color=gta_colour$qualitative[1]),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank())

fig3

gta_plot_saver(plot = fig3,
               path = paste0(gta26.path, out.path),
               name = "Figure 3 - Number of different investigations introduced",
               png = T,
               pdf = T,
               jpg = T,
               eps = T)



### Figure 4
# # Convert data to longer format
# table4 <- pivot_longer(table4, cols = names(table4)[-1], names_to = "year", names_prefix = "Trade coverage estimate for ", values_to = "trade.coverage")

data.table::setnames(table4, old = "MAST chapter name", new = "mast.group")
data.table::setnames(table4, old = "trade.share", new = "trade.coverage")

# Plot for all measures
p1 <- bar.function(data = subset(table4, mast.group == 'All harmful interventions'), fill.colour = gta_colour$harmful[1], x.lab = 'All harmful interventions', y.lab = '', y.breaks = seq(0,0.45,0.05), y.limits = c(0,0.45))
p2 <- bar.function(data = subset(table4, mast.group == 'P: Export incentives'), fill.colour = gta_colour$harmful[1], x.lab = 'P: Export incentives', y.lab = '', y.breaks = seq(0,0.45,0.05), y.limits = c(0,0.45))
p3 <- bar.function(data = subset(table4, mast.group == 'L: Subsidies (excl. export subsidies)'), fill.colour = gta_colour$harmful[1], x.lab = 'L: Subsidies (excl. export subsidies)', y.lab = '', y.breaks = seq(0,0.45,0.05), y.limits = c(0,0.45))
p4 <- bar.function(data = subset(table4, mast.group == 'Tariff measures'), fill.colour = gta_colour$harmful[1], x.lab = 'Tariff measures', y.lab = '', y.breaks = seq(0,0.45,0.05), y.limits = c(0,0.45))
p5 <- bar.function(data = subset(table4, mast.group == 'D: Contingent trade-protective measures'), fill.colour = gta_colour$harmful[1], x.lab = 'D: Contingent trade-protective measures', y.lab = '', y.breaks = seq(0,0.006,0.001), y.limits = c(0,0.006), y.labels = label_percent(accuracy = 0.1))
p6 <- bar.function(data = subset(table4, mast.group == 'I: Trade-related investment measures '), fill.colour = gta_colour$harmful[1], x.lab = 'I: Trade-related investment measures', y.lab = '', y.breaks = seq(0,0.006,0.001), y.limits = c(0,0.006), y.labels = label_percent(accuracy = 0.1))
p7 <- bar.function(data = subset(table4, mast.group == 'P: Export barriers'), fill.colour = gta_colour$harmful[1], x.lab = 'P: Export barriers', y.lab = '', y.breaks = seq(0,0.006,0.001), y.limits = c(0,0.006), y.labels = label_percent(accuracy = 0.1))

# # Plot with row 1 scale
# for (i in c(1:3)){
#   eval(parse(text = paste0("p", i + 1," <- bar.function(data = subset(table4, mast.group == '", unique(table4$mast.group[table4$mast.group != "All harmful interventions"])[i], "'), fill.colour = gta_colour$harmful[1], x.lab = '", unique(table4$mast.group[table4$mast.group != "All harmful interventions"])[i], "', y.lab = '', y.breaks = seq(0,0.45,0.05), y.limits = c(0,0.45))")))
# }
# 
# # Plot with row 2 scale
# for (i in c(4:6)){
#   eval(parse(text = paste0("p", i," <- bar.function(data = subset(table4, mast.group == '", unique(table4$mast.group[table4$mast.group != "All harmful interventions"])[i], "'), fill.colour = gta_colour$harmful[1], x.lab = '", unique(table4$mast.group[table4$mast.group != "All harmful interventions"])[i], "', y.lab = '', y.breaks = seq(0,0.45,0.05), y.limits = c(0,0.45))")))
# }


fig4 <- grid.arrange(p1, p2, p3, p4, p5, p6, p7, nrow = 2, bottom = text_grob("Source: Global Trade Alert.", family = "Open Sans", color = "#3f3f3f", size = 10))

gta_plot_saver(plot = fig4,
               path = paste0(gta26.path, out.path),
               name = "Figure 4 - Trade covered by harmful interventions by MAST chapter",
               png = T,
               pdf = T,
               jpg = T,
               eps = T,
               width = 29,
               height = 21)


### Figure 4 - lag adjusted
data.table::setnames(table4lag, old = "MAST chapter name", new = "mast.group")
data.table::setnames(table4lag, old = "trade.share", new = "trade.coverage")

# Plot for all measures
p1 <- bar.function(data = subset(table4lag, mast.group == 'All harmful interventions'), fill.colour = gta_colour$harmful[1], x.lab = 'All harmful interventions', y.lab = '', y.breaks = seq(0,0.4,0.05), y.limits = c(0,0.4))
p2 <- bar.function(data = subset(table4lag, mast.group == 'P: Export incentives'), fill.colour = gta_colour$harmful[1], x.lab = 'P: Export incentives', y.lab = '', y.breaks = seq(0,0.4,0.05), y.limits = c(0,0.4))
p3 <- bar.function(data = subset(table4lag, mast.group == 'L: Subsidies (excl. export subsidies)'), fill.colour = gta_colour$harmful[1], x.lab = 'L: Subsidies (excl. export subsidies)', y.lab = '', y.breaks = seq(0,0.4,0.05), y.limits = c(0,0.4))
p4 <- bar.function(data = subset(table4lag, mast.group == 'Tariff measures'), fill.colour = gta_colour$harmful[1], x.lab = 'Tariff measures', y.lab = '', y.breaks = seq(0,0.4,0.05), y.limits = c(0,0.4))
p5 <- bar.function(data = subset(table4lag, mast.group == 'D: Contingent trade-protective measures'), fill.colour = gta_colour$harmful[1], x.lab = 'D: Contingent trade-protective measures', y.lab = '', y.breaks = seq(0,0.006,0.001), y.limits = c(0,0.006), y.labels = label_percent(accuracy = 0.1))
p6 <- bar.function(data = subset(table4lag, mast.group == 'I: Trade-related investment measures '), fill.colour = gta_colour$harmful[1], x.lab = 'I: Trade-related investment measures', y.lab = '', y.breaks = seq(0,0.006,0.001), y.limits = c(0,0.006), y.labels = label_percent(accuracy = 0.1))
p7 <- bar.function(data = subset(table4lag, mast.group == 'P: Export barriers'), fill.colour = gta_colour$harmful[1], x.lab = 'P: Export barriers', y.lab = '', y.breaks = seq(0,0.006,0.001), y.limits = c(0,0.006), y.labels = label_percent(accuracy = 0.1))

# # Plot
# for (i in c(1:7)){
#   eval(parse(text = paste0("p", i," <- bar.function(data = subset(table4lag, mast.group == '", unique(table4lag$mast.group)[i], "'), fill.colour = gta_colour$harmful[1], x.lab = '", unique(table4lag$mast.group)[i], "', y.lab = '', y.breaks = seq(0,0.40,0.05), y.limits = c(0,0.40))")))
# }

fig4lag <- grid.arrange(p1, p2, p3, p4, p5, p6, p7, nrow = 2, bottom = text_grob("Source: Global Trade Alert.", family = "Open Sans", color = "#3f3f3f", size = 10))

gta_plot_saver(plot = fig4lag,
               path = paste0(gta26.path, out.path),
               name = "Figure 4 - Trade covered by harmful interventions by MAST chapter - lag adjusted",
               png = T,
               pdf = T,
               jpg = T,
               eps = T,
               width = 29,
               height = 21)


### Figure 5
# # Convert data to longer format
# table4 <- pivot_longer(table5, cols = names(table5)[-1], names_to = "year", names_prefix = "Trade coverage estimate for ", values_to = "trade.coverage")

data.table::setnames(table5, old = "MAST chapter name", new = "mast.group")
data.table::setnames(table5, old = "trade.share", new = "trade.coverage")

# Plot for all measures
p1 <- bar.function(data = subset(table5, mast.group == 'All liberalising interventions'), fill.colour = gta_colour$liberalising[1], x.lab = 'All liberalising interventions', y.lab = '', y.breaks = seq(0,0.3,0.05), y.limits = c(0,0.3))
p2 <- bar.function(data = subset(table5, mast.group == 'P: Export incentives'), fill.colour = gta_colour$liberalising[1], x.lab = 'P: Export incentives', y.lab = '', y.breaks = seq(0,0.3,0.05), y.limits = c(0,0.3))
p3 <- bar.function(data = subset(table5, mast.group == 'Tariff measures'), fill.colour = gta_colour$liberalising[1], x.lab = 'Tariff measures', y.lab = '', y.breaks = seq(0,0.3,0.05), y.limits = c(0,0.3))
p4 <- bar.function(data = subset(table5, mast.group == 'L: Subsidies (excl. export subsidies)'), fill.colour = gta_colour$liberalising[1], x.lab = 'L: Subsidies (excl. export subsidies)', y.lab = '', y.breaks = seq(0,0.05,0.01), y.limits = c(0,0.05))
p5 <- bar.function(data = subset(table5, mast.group == 'E: Non-automatic licensing, quotas etc.'), fill.colour = gta_colour$liberalising[1], x.lab = 'E: Non-automatic licensing, quotas etc.', y.lab = '', y.breaks = seq(0,0.05,0.01), y.limits = c(0,0.05))
p6 <- bar.function(data = subset(table5, mast.group == 'P: Export barriers'), fill.colour = gta_colour$liberalising[1], x.lab = 'P: Export barriers', y.lab = '', y.breaks = seq(0,0.05,0.01), y.limits = c(0,0.05))

# # Plot
# for (i in c(1:6)){
#   eval(parse(text = paste0("p", i," <- bar.function(data = subset(table5, mast.group == '", unique(table5$mast.group)[i], "'), fill.colour = gta_colour$liberalising[1], x.lab = '", unique(table5$mast.group)[i], "', y.lab = '', y.breaks = seq(0,0.3,0.05), y.limits = c(0,0.3))")))
# }

fig5 <- grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, bottom = text_grob("Source: Global Trade Alert.", family = "Open Sans", color = "#3f3f3f", size = 10))

gta_plot_saver(plot = fig5,
               path = paste0(gta26.path, out.path),
               name = "Figure 5 - Trade covered by liberalising interventions by MAST chapter",
               png = T,
               pdf = T,
               jpg = T,
               eps = T,
               width = 29,
               height = 21)


### Figure 5 - lag adjusted
data.table::setnames(table5lag, old = "MAST chapter name", new = "mast.group")
data.table::setnames(table5lag, old = "trade.share", new = "trade.coverage")

# Plot for all measures
p1 <- bar.function(data = subset(table5lag, mast.group == 'All liberalising interventions'), fill.colour = gta_colour$liberalising[1], x.lab = 'All liberalising interventions', y.lab = '', y.breaks = seq(0,0.3,0.05), y.limits = c(0,0.3))
p2 <- bar.function(data = subset(table5lag, mast.group == 'P: Export incentives'), fill.colour = gta_colour$liberalising[1], x.lab = 'P: Export incentives', y.lab = '', y.breaks = seq(0,0.3,0.05), y.limits = c(0,0.3))
p3 <- bar.function(data = subset(table5lag, mast.group == 'Tariff measures'), fill.colour = gta_colour$liberalising[1], x.lab = 'Tariff measures', y.lab = '', y.breaks = seq(0,0.3,0.05), y.limits = c(0,0.3))
p4 <- bar.function(data = subset(table5lag, mast.group == 'L: Subsidies (excl. export subsidies)'), fill.colour = gta_colour$liberalising[1], x.lab = 'L: Subsidies (excl. export subsidies)', y.lab = '', y.breaks = seq(0,0.011,0.001), y.limits = c(0,0.011), y.labels = label_percent(accuracy = 0.1))
p5 <- bar.function(data = subset(table5lag, mast.group == 'E: Non-automatic licensing, quotas etc.'), fill.colour = gta_colour$liberalising[1], x.lab = 'E: Non-automatic licensing, quotas etc.', y.lab = '', y.breaks = seq(0,0.011,0.001), y.limits = c(0,0.011), y.labels = label_percent(accuracy = 0.1))
p6 <- bar.function(data = subset(table5lag, mast.group == 'P: Export barriers'), fill.colour = gta_colour$liberalising[1], x.lab = 'P: Export barriers', y.lab = '', y.breaks = seq(0,0.011,0.001), y.limits = c(0,0.011), y.labels = label_percent(accuracy = 0.1))

# # Plot
# for (i in c(1:6)){
#   eval(parse(text = paste0("p", i," <- bar.function(data = subset(table5lag, mast.group == '", unique(table5lag$mast.group)[i], "'), fill.colour = gta_colour$liberalising[1], x.lab = '", unique(table5lag$mast.group)[i], "', y.lab = '', y.breaks = seq(0,0.3,0.05), y.limits = c(0,0.3))")))
# }

fig5lag <- grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, bottom = text_grob("Source: Global Trade Alert.", family = "Open Sans", color = "#3f3f3f", size = 10))

gta_plot_saver(plot = fig5lag,
               path = paste0(gta26.path, out.path),
               name = "Figure 5 - Trade covered by liberalising interventions by MAST chapter - lag adjusted",
               png = T,
               pdf = T,
               jpg = T,
               eps = T,
               width = 29,
               height = 21)
