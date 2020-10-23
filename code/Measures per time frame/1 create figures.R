rm(list = ls())

### REQUEST:
# 1. Please prepare a stacked bar chart for each year 2009 with the number of measures recorded in the GTA database worldwide by 21 October 2009, from
# 22 October 2009 to 21 October 2010, from 22 October 2010 to 21 October 2015, from 22 October 2015 to 21 October 2020. On the same stacked bar chart
# please add another column for the measures implemented by the G20, again for the same time periods. Please add a note at the bottom stating
# “Source: Global Trade Alert.”
#
# 2. Please prepare a stacked column along the following lines. This chart only relates to measures implemented during 2009. Define the set of transparent
# policy instruments as all import tariff changes, all trade defence changes, all import quotas, all export quotas, all export taxes, all export licensing
# requirements, all import bans, all import quotas, and all export bans and all export quotas. Define subsidies to import competing firms as all measures
# in chapter L of the MAST classification. Define export incentives as all the measures in Chapter M except any export restriction or other export limit.
# Define other commercial policies as any policy intervention not in these three above categories. Please plot from 2009 to 2020 the 100% stacked columns
# showing the information reported in GTA database of these 4 classes of policy intervention that have been documented up to 31 December of each year
# (21 October for the year 2020). I have identified in bold text the legend labels to be used. Please add a note at the bottom stating “Source: Global Trade Alert.”

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
data.path = "data/Measures per time frame/"
out.path = "tables & figures/3 - Measures per time frame/"
source(paste0(gta26.path, "help files/GTA 26 cutoff and definitions.R"))

gta_colour_palette()

# Load data
load(paste0(gta26.path, data.path, "measures per time frame.Rdata"))

### Save Excel files of the data
write.xlsx(table1, file = paste0(gta26.path, out.path, "Figure 1 data.xlsx"))
write.xlsx(table2, file = paste0(gta26.path, out.path, "Figure 2 data.xlsx"))


### Figure 1
# Transform the data for plotting
table1 <- pivot_longer(table1, cols = c("worldwide", "G20"), names_to = "region", values_to = "nr.of.measures")

# Plot
fig1 <- ggplot(data = table1)+
  geom_bar(aes(x=region, y=nr.of.measures, fill=forcats::fct_reorder(period, desc(nr.of.measures))), width=0.65, stat = "identity") +
  labs(x="", y="", caption = "Source: Global Trade Alert.")+
  scale_y_continuous(name=str_wrap("Number of measures recorded", 30), breaks=seq(0,30000,2000), labels=seq(0,30000,2000),
                     sec.axis = sec_axis(trans = ~., name=str_wrap("Number of measures recorded", 30), breaks=seq(0,30000,2000), labels=seq(0,30000,2000)))+
  scale_fill_manual(values=c("before 21 Oct 2009" = gta_colour$qualitative[3], "22 Oct 2009 to 21 Oct 2010" = gta_colour$qualitative[4], "22 Oct 2010 to 21 Oct 2015" = gta_colour$qualitative[1],
                             "22 Oct 2015 to 21 Oct 2020" = gta_colour$qualitative[2]), labels = c("22 Oct 2015 to 21 Oct 2020", "22 Oct 2010 to 21 Oct 2015", "22 Oct 2009 to 21 Oct 2010", "before 21 Oct 2009"))+
  gta_theme()+
  guides(guide_legend(title = NULL, label.hjust = 0, label.vjust = 0.5, title.position = "top", title.vjust = 0.5, ncol = 2, nrow = 2))+
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
               name = "Figure 1 - Measures recorded per time frame",
               png = T,
               pdf = T,
               jpg = T,
               width = 21,
               height = 21)


### Figure 2
fig2 <- ggplot(data = table2)+
  geom_bar(aes(x=forcats::fct_inorder(as.character(year.submitted)), y=percentage.per.year, fill=SE.int.type), width=0.65, stat = "identity") +
  labs(x="", y="", caption = "Source: Global Trade Alert.")+
  scale_y_continuous(name=str_wrap("Percentage of recorded measures", 40), breaks=seq(0,1,0.1), labels=percent,
                     sec.axis = sec_axis(trans = ~., name=str_wrap("Percentage of recorded measures", 40), breaks=seq(0,1,0.1), labels=percent))+
  scale_fill_manual(values=c("export incentives" = gta_colour$qualitative[1], "other commercial policies" = gta_colour$qualitative[2], "subsidies to import competing firms" = gta_colour$qualitative[3],
                             "transparent policy instruments" = gta_colour$qualitative[4]), labels = c("export incentives", "other commercial policies", "subsidies to import competing firms", "transparent policy instruments"))+
  gta_theme(x.bottom.angle = 90, x.bottom.align = 1)+
  guides(guide_legend(title = NULL, label.hjust = 0, label.vjust = 0.5, title.position = "top", title.vjust = 0.5, ncol = 2, nrow = 2))+
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
               name = "Figure 2 - Types of measures recorded per time frame",
               png = T,
               pdf = T,
               jpg = T,
               width = 25,
               height = 21)