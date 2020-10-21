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
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(scales)
library(stringr)
library(openxlsx)

gta_setwd()
gta26.path = "0 report production/GTA 26/"
data.path = "data/Food & medicines phase in & phase out/"
out.path = "tables & figures/5 - Food & medicines phase in & phase out/"
source(paste0(gta26.path, "help files/GTA 26 cutoff and definitions.R"))

gta_colour_palette()

# Load data
load(paste0(gta26.path, data.path, "food & medicines phase in & phase out.Rdata"))

### Save Excel files of the data
write.xlsx(table1, file = paste0(gta26.path, out.path, "Figure 1 data.xlsx"))
write.xlsx(table2, file = paste0(gta26.path, out.path, "Figure 2 & 3 data.xlsx"))


### Figure 1
# Pivot the data to longer format
table1 <- pivot_longer(table1, cols = c(names(table1)[-1]), names_to = "product", values_to = "nr.of.interventions")

# Plot
fig1 <- ggplot(data=table1)+
  geom_line(aes(x=as.numeric(year.implemented), y=nr.of.interventions, colour=product), size=1)+
  # geom_text(aes(x=month.group, y=index, label=index), vjust=1, colour="#555555", size=2.5)+
  scale_y_continuous(name=str_wrap("Percentage of implemented measures", 50),
                     labels=label_percent(accuracy = 1L), limits = c(0,0.2), breaks=seq(0,0.2,0.025),
                     sec.axis = sec_axis(trans = ~., name=str_wrap("Percentage of implemented measures", 50), breaks=seq(0,0.2,0.025),labels=label_percent(accuracy = 1L)))+
  scale_x_continuous(name = NULL, breaks = c(2009:2020), labels = as.character(c(2009:2020))) +
  scale_color_manual(values=c("food"=gta_colour$qualitative[1],"medical"=gta_colour$qualitative[2],"Top 1 sector (412)"=gta_colour$qualitative[3],
                              "Top 2 sector (491)"=gta_colour$qualitative[4], "Top 3 sector (429)"=gta_colour$qualitative[5]), labels=c("Food and agri-food", "COVID-19 medical kit and medicines", "Top 1 sector (412)", "Top 2 sector (491)", "Top 3 sector (429)"))+
  labs(x = "", caption = "Source: Global Trade Alert.") +
  guides(colour=guide_legend(title=NULL, ncol = 3, 
                             label.hjust = 0, label.vjust = 0.5, 
                             title.position = "top", title.hjust = 0, 
                             direction = "horizontal", 
                             label.position = "right")) +
  gta_theme(x.bottom.angle = 90, x.bottom.align = 1) +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_line(size=.1, color="#555555"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption.position = "plot",
        axis.text.x = element_text())

fig1

gta_plot_saver(plot = fig1,
               path = paste0(gta26.path, out.path),
               name = "Figure 1 - Yearly percentage of food and medical interventions",
               png = T,
               pdf = T,
               jpg = T)


### Figure 2
fig2 <- ggplot(data = subset(table2, product.type == "medical"))+
  geom_line(aes(x=forcats::fct_inorder(time, ordered = T), y=nr.of.interventions, colour=int.type, group=int.type), size=1)+
  geom_label(aes(x = forcats::fct_inorder(time, ordered = T), y = nr.of.interventions, label = nr.of.interventions), colour = "#555555", size=2.2, label.size = 0.2, label.padding = unit(0.15, "lines"), position = position_dodge2(width = 1, preserve = "total"))+
  scale_y_continuous(name=str_wrap("Cumulative global total number of measures introduced in 2020 that were still in force in the medical goods and medicines sectors", 70), limits = c(0,200), breaks=seq(0,200,25),  
                     sec.axis = sec_axis(trans = ~., name=str_wrap("Cumulative global total number of measures introduced in 2020 that were still in force in the medical goods and medicines sectors", 70), breaks=seq(0,200,25)))+
  scale_color_manual(values=c(gta_colour$harmful[1], gta_colour$liberalising[1]))+
  labs(caption = "Source: Global Trade Alert.") +
  guides(colour=guide_legend(title=NULL, 
                             label.hjust = 0, label.vjust = 0.5, 
                             title.position = "top", title.hjust = 0, 
                             direction = "horizontal", 
                             label.position = "right"))+
  gta_theme(x.bottom.angle = 90, x.bottom.align = 1) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "#dddddd"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom",
        axis.title.x.bottom = element_blank())

fig2

gta_plot_saver(plot = fig2,
               path = paste0(gta26.path, out.path),
               name = "Figure 2 - Phase in & phase out for medical goods",
               png = T,
               pdf = T,
               jpg = T,
               width = 21,
               height = 21)


### Figure 3
fig3 <- ggplot(data = subset(table2, product.type == "food"))+
  geom_line(aes(x=forcats::fct_inorder(time, ordered = T), y=nr.of.interventions, colour=int.type, group=int.type), size=1)+
  geom_label(aes(x = forcats::fct_inorder(time, ordered = T), y = nr.of.interventions, label = nr.of.interventions), colour = "#555555", size=2.2, label.size = 0.2, label.padding = unit(0.15, "lines"), position = position_dodge2(width = 0.8, preserve = "total"))+
  scale_y_continuous(name=str_wrap("Cumulative global total number of measures introduced in 2020 that were still in force in the food sector", 70), limits = c(0,85), breaks=seq(0,85,10),  
                     sec.axis = sec_axis(trans = ~., name=str_wrap("Cumulative global total number of measures introduced in 2020 that were still in force in the food sector", 70), breaks=seq(0,85,10)))+
  scale_color_manual(values=c(gta_colour$harmful[1], gta_colour$liberalising[1]))+
  labs(caption = "Source: Global Trade Alert.") +
  guides(colour=guide_legend(title=NULL, 
                             label.hjust = 0, label.vjust = 0.5, 
                             title.position = "top", title.hjust = 0, 
                             direction = "horizontal", 
                             label.position = "right"))+
  gta_theme(x.bottom.angle = 90, x.bottom.align = 1) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "#dddddd"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom",
        axis.title.x.bottom = element_blank())

fig3

gta_plot_saver(plot = fig3,
               path = paste0(gta26.path, out.path),
               name = "Figure 3 - Phase in & phase out for food",
               png = T,
               pdf = T,
               jpg = T,
               width = 21,
               height = 21)
