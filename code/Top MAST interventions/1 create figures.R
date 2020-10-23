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
write.xlsx(table2, file = paste0(gta26.path, out.path, "Figure 3  data.xlsx"))
write.xlsx(table3, file = paste0(gta26.path, out.path, "Figure 4 data.xlsx"))
write.xlsx(table4, file = paste0(gta26.path, out.path, "Figure 5 data.xlsx"))


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
               jpg = T)



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

gta_plot_saver(plot = fig1,
               path = paste0(gta26.path, out.path),
               name = "Figure 2 - Percentage of liberalising measures per MAST chapter",
               png = T,
               pdf = T,
               jpg = T)



### Figure 3
# Plot
fig3 <- ggplot(data = table2)+
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
               jpg = T)



### Figure 4
# Convert data to longer format
table3 <- pivot_longer(table3, cols = names(table3)[-1], names_to = "year", names_prefix = "Trade coverage estimate for ", values_to = "trade.coverage")

# Plot
for (i in c(1:6)){
  eval(parse(text = paste0("p", i," <- bar.function(data = subset(table3, mast.group == '", unique(table3$mast.group)[i], "'), fill.colour = gta_colour$harmful[1], x.lab = '", unique(table3$mast.group)[i], "', y.lab = '', y.breaks = seq(0,0.2,0.02), y.limits = c(0,0.21))")))
}

fig4 <- grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, bottom = text_grob("Source: Global Trade Alert.", family = "Open Sans", color = "#3f3f3f", size = 10))

gta_plot_saver(plot = fig4,
               path = paste0(gta26.path, out.path),
               name = "Figure 4 - Trade covered by harmful interventions by MAST chapter",
               png = T,
               pdf = T,
               jpg = T,
               width = 29,
               height = 21)


### Figure 5
# Convert data to longer format
table4 <- pivot_longer(table4, cols = names(table4)[-1], names_to = "year", names_prefix = "Trade coverage estimate for ", values_to = "trade.coverage")

# Plot
for (i in c(1:6)){
  eval(parse(text = paste0("p", i," <- bar.function(data = subset(table4, mast.group == '", unique(table4$mast.group)[i], "'), fill.colour = gta_colour$liberalising[1], x.lab = '", unique(table4$mast.group)[i], "', y.lab = '', y.breaks = seq(0,0.11,0.01), y.limits = c(0,0.11))")))
}

fig5 <- grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, bottom = text_grob("Source: Global Trade Alert.", family = "Open Sans", color = "#3f3f3f", size = 10))

gta_plot_saver(plot = fig5,
               path = paste0(gta26.path, out.path),
               name = "Figure 5 - Trade covered by liberalising interventions by MAST chapter",
               png = T,
               pdf = T,
               jpg = T,
               width = 29,
               height = 21)