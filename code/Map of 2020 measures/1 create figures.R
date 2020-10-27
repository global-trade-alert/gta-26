rm(list = ls())

### REQUEST:
# 1. Please produce a map that shows for each country the total number of measures of any type recorded this year in the GTA database. Please use the following
# ranges for the map: 0 measures, 1-9 measures, 10-50 measures, 50-100 measures, more than 100 measures. Please add at the bottom of the map the text
# “Source: Global Trade Alert. Map relates only to policy intervention announced or implemented from 1 January 2020 to 21 October 2020.”

library(gtalibrary)
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(scales)
library(stringr)

gta_setwd()
gta26.path = "0 report production/GTA 26/"
data.path = "data/Map of 2020 measures/"
out.path = "tables & figures/10 - Map of 2020 measures/"
source(paste0(gta26.path, "help files/GTA 26 cutoff and definitions.R"))

gta_colour_palette()

# Load data
load(paste0(gta26.path, data.path, "map of 2020 measures.Rdata"))

### Save Excel files of the data
write.xlsx(table1, file = paste0(gta26.path, out.path, "Figure 1 data.xlsx"))


### Figure 1
# Prepare plotting data
world.fig1 <- gta_plot_map_df(table1, countries = "un_code", values = "group")

# Plot
fig1 <- ggplot() +
  geom_polygon(data = subset(world.fig1, country != "Antarctica"), aes(x = long, y = lat, group = group), fill="#dadada", size = 0.15, color = "white") +
  geom_polygon(data = subset(world.fig1, country != "Antarctica"), aes(x = long, y = lat, group = group, fill=value), na.rm = T, size = 0.15, color = "white") +
  geom_polygon(data=subset(world.fig1, country == "Greenland"), aes(x=long, y=lat, group = group), fill="#dadada", size = 0.15, colour = "white") +
  coord_fixed() + # Important to fix world map proportions
  scale_x_continuous(limits = c(-13900000,17000000))+
  labs(x = "", y = "", caption = "Source: Global Trade Alert. Map relates only to policy interventions announced or implemented from 1 January 2020 to 21 October 2020.") +
  scale_fill_manual(values = c("0" = "#b3e0fc", "1" = gta_colour$blue[4], "2" = gta_colour$blue[3], "3" = gta_colour$blue[2], "4" = gta_colour$blue[1], "5" = "#0f3f57"),
                    position="bottom", labels=c("0 measures", "1-9 measures", "10-50 measures", "50-100 measures", "more than 100 measures"), na.translate=F) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.55,0),
        legend.justification = c(0.5,0.4),
        legend.direction = "horizontal",
        plot.title = element_text(family = "Open Sans", face = "bold", colour = "#333333", size = 11, hjust = 0.5, margin = margin(b=10)),
        plot.subtitle = element_text(family = "Open Sans", face = "bold", colour = "#333333", size = 9, hjust = 0.5, margin = margin(b=10)),
        legend.title = element_text(vjust= 0.3, family="", colour = "#333333", size = 11*0.8, margin = margin(r=10,b=5),hjust=0.5),
        legend.text = element_text(family="", colour = "#333333", size = 11*0.75, angle = 0, hjust=0, vjust=1, margin = margin(r=10)),
        legend.text.align = 0.6,
        legend.background = element_rect(fill="transparent"),
        plot.background = element_rect(fill="#F9F9F9"),
        plot.caption = element_text(hjust = 0.5, vjust = 0.8, margin = margin(t=60),size=8, color="#777777",lineheight = 1)) +
  guides(fill = guide_legend(title="Number of measures implemented in 2020", label.position = "bottom", title.position = "top"))

fig1

gta_plot_saver(plot = fig1,
               path = paste0(gta26.path, out.path),
               name = "Figure 1 - Number of measures implemented in 2020 map",
               png = T,
               pdf = T,
               jpg = T,
               width = 29,
               height = 17.2)
